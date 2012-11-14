-module (r9_echo).
-behaviour(gen_server).
-export([start_link/0, handle_query/5, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
        }).
-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

handle_query(Pid, Socket, IP, Port, Packet) ->
    gen_server:cast(Pid, {echo, Socket, IP, Port, Packet}).

stop(Pid) ->
    gen_server:call(Pid, stop).


init([]) ->
    {ok, #state{}}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({echo, Socket, IP, Port, Query}, State) ->
    MessageHeader = r9_message_header:from_wire(Query),
    r9_message_header:print(MessageHeader),
    Response = r9_message_header:set_qr(Query),
    case gen_udp:send(Socket, IP, Port, Response) of
        ok ->
            {noreply, State};
        {error, Reason} ->
            io:format("send package failed ~p ~n", [Reason]),
            {stop, "socket send failed", State}
    end.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

