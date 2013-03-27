-module (r9_iterator).
-behaviour(gen_server).
-export([start_link/0, 
        set_prev_recursor/1,
        stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(out_query, {
        client_query
    }). 

-record(state, {
        cache,
        prev_recursor,
        next_recursor,
        out_queries,
        socket
    }).
-define(ITERATOR, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?ITERATOR}, ?ITERATOR, [], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

set_prev_recursor(PrevRecursor) ->
    gen_server:cast(?ITERATOR, {set_prev_recursor, PrevRecursor}).

init([]) ->
    case gen_udp:open(5555, [binary]) of
        {ok, Socket} -> io:format("forwarder open socket succeed ~n"),
            {ok, Cache} = r9_cache:start_link(),
            {ok, #state{cache = Cache,
                    out_queries = [],
                    socket = Socket}};

        {error, Reason} -> io:format("for forwarder open socket failed: ~p ~n", [Reason]),
            {error, Reason}
    end.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({set_prev_recursor, PrevRecursor}, State) ->
    {noreply, State#state{prev_recursor = PrevRecursor}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, _Socket, _IP, _Port, Packet}, #state{prev_recursor = PrevRecursor} = State) ->
    {Message, _} = r9_message:from_wire(Packet, 0),
    r9_cache:put_message(Message),
    PrevRecursor ! {handle_response, Packet},
    {noreply, State};

handle_info({handle_query, Query}, #state{socket = Socket, out_queries = OutQueries} = State) ->
    NewState = State#state{out_queries = [#out_query{client_query = Query} | OutQueries]},
    {QueryMessage, _} = r9_message:from_wire(Query),
    Question = r9_message:question(QueryMessage),
    case r9_cache:get_message(r9_message_question:name(Question), 
                              r9_message_question:type(Question)) of
        {ok, CachedMessage} ->
            QueryID = r9_message_header:id(r9_message:header(QueryMessage)),
            Response = r9_message:to_wire(r9_message:set_id(CachedMessage, QueryID)),
            State#state.prev_recursor ! {handle_response, Response},
            {noreply, NewState}; 
        {not_found} ->
            case gen_udp:send(Socket, "8.8.8.8", 53, Query) of
                ok -> {noreply, NewState}; 
                {error, Reason} -> io:format("send query failed: ~p ~n", [Reason]),
                    {stop, Reason, State}
            end
    end;

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
