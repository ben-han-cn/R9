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
            id,
            request}).

-define(SERVER, ?MODULE).

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
    {ok, #state{logger = Logger,
                concurrent_queries = ets:new(message_table, [bag, 
                                                             {keypos, #query_entry.id},
                                                             {read_concurrency, true},
                                                             {write_concurrency, true}])}}.

handle_call(stop, _From, #state{concurrent_queries = ConcurrentQueryTable} = State) ->
    ets:delete(ConcurrentQueryTable),
    r9_logger:stop(State#state.logger),
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({set_next_recursor, NextRecursor}, State) ->
    {noreply, State#state{next_recursor = NextRecursor}};


handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({accept_request, Socket, Client, Packet},  
            #state{concurrent_queries = ConcurrentQueryTable, next_recursor = NextRecursor} = State) ->
    {Message, _} = r9_message:from_wire(Packet),
    ID = r9_message_header:id(r9_message:header(Message)),
    Question = r9_message:question(Message),
    r9_logger:log(State#state.logger, r9_message_question:to_string(Question)),
    Request = #request{id = ID, 
                       question = Question,
                       client = Client,
                       socket = Socket},
    MessageID = r9_message_question:id(Question),
    ets:member(ConcurrentQueryTable, MessageID) orelse (NextRecursor ! {handle_query, Request}),
    ets:insert(ConcurrentQueryTable, #query_entry{id = MessageID, request = Request}),
    {noreply, State};

handle_info({handle_response, Response},  #state{concurrent_queries = ConcurrentQueryTable} = State) ->
    answer_clients_with_same_question(ConcurrentQueryTable, Response),
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%utils
answer_clients_with_same_question(ConcurrentQueryTable, #response{question = Question, reply_message = Message}) ->
    QuestionID = r9_message_question:id(Question),
    case ets:lookup(ConcurrentQueryTable, QuestionID) of
        [] -> io:format("~~error unknown asker for response ~p", [QuestionID]);
        Askers ->
            <<_:16, RawDataExceptID/binary>> = r9_message:to_wire(Message),
            lists:foreach(fun(#query_entry{request = Request}) ->
                            #request{id = ID, client = Client, socket = Socket} = Request,
                            gen_udp:send(Socket, 
                                         Client#host.ip, 
                                         Client#host.port, 
                                         list_to_binary([<<ID:16/big>>, RawDataExceptID])) 
                         end, Askers)
    end,
    ets:delete(ConcurrentQueryTable, QuestionID).
