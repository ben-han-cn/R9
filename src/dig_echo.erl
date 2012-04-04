-module (dig_echo).
-behaviour(gen_server).
-export([start_link/1, handle_query/5, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
        }).
-define(SERVER, ?MODULE).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

handle_query(Name, Socket, IP, Port, Packet) ->
    gen_server:cast(Name, {echo, Socket, IP, Port, Packet}).

stop(Name) ->
    gen_server:call(Name, stop).


init([]) ->
    {ok, #state{}}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({echo, Socket, IP, Port, Query}, State) ->
    Response = dig_message_header:message_header_set_qr(Query),
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

