-module (r9_acceptor).
-behaviour(gen_server).

-export([start_link/0, 
         set_next_recursor/1,
         stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
            next_recursor,
            concurrent_queries,
            logger
        }).

-record(query_entry, {
            request,
            expire_time
        }).

-define(SERVER, ?MODULE).
-define(QUERY_TIMEOUT, 5).  

-include("r9_dns.hrl").
-include("r9_resolver.hrl").

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

set_next_recursor(NextRecursor) ->
    gen_server:cast(?SERVER, {set_next_recursor, NextRecursor}).

stop() ->
    gen_server:call(?SERVER, stop).


init([]) ->
    {ok, Logger} = r9_logger:get_logger(?MODULE),
    {ok, C} = eredis:start_link(),
    {ok, #state{logger = Logger,
            concurrent_queries = C}}.

handle_call(stop, _From, #state{concurrent_queries = C} = State) ->
    r9_logger:stop(State#state.logger),
    eredis:stop(C),
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({set_next_recursor, NextRecursor}, State) ->
    {noreply, State#state{next_recursor = NextRecursor}};


handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({handle_query, #request{question = Question} = Request},  
            #state{concurrent_queries = C, next_recursor = NextRecursor} = State) ->
    QueryID= query_id(Question),
    Now = r9_util:local_now(),
    case eredis:q(C, ["LINDEX", QueryID, -1]) of
        {ok, undefined} -> 
            NextRecursor ! {handle_query, Request};
        {ok, QueryEntryBinary} ->
            #query_entry{expire_time = ExpireTime} = binary_to_term(QueryEntryBinary),
            if 
                Now > ExpireTime ->
                    NextRecursor ! {handle_query, Request},
                    eredis:q(C, ["DEL", QueryID]);
                true ->
                    r9_logger:log(State#state.logger, "merge query :" ++ r9_message_question:to_string(Question))
            end
    end,
    eredis:q(C, ["RPUSH", QueryID, #query_entry{request = Request, expire_time = Now + ?QUERY_TIMEOUT}]),
    {noreply, State};

handle_info({handle_response, Response},  #state{concurrent_queries = C} = State) ->
    answer_clients_with_same_question(C, Response),
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%utils
answer_clients_with_same_question(C, #response{question = Question, reply_message = Message} = Response) ->
    QueryID = query_id(Question),
    {ok, Clients} = eredis:q(C, ["LRANGE", QueryID, 0, -1]),
    <<_:16, RawDataExceptID/binary>> = r9_message:to_wire(Message),
    lists:foreach(fun(QueryEntryBinary) ->
                #query_entry{request = Request} = binary_to_term(QueryEntryBinary),
                #request{id = ID, client = Client} = Request,
                case Client of
                    {host, _IP, _Port} ->
                        gen_server:cast(r9_server, {handle_response, 
                                Client, 
                                list_to_binary([<<ID:16/big>>, RawDataExceptID])}); 
                    Pid ->
                        Pid ! {handle_response, Response}
                end
    end, Clients),
    eredis:q(C, ["DEL", QueryID]).

query_id(Question) ->
    lists:concat(["Q+", r9_message_question:id(Question)]).
