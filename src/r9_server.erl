-module (r9_server).
-behaviour(gen_server).
-import(r9_echo).

-export([start_link/1, 
         set_next_recursor/1,
         stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
            next_recursor,
            ip,
            port,
            socket
        }).

-define(SERVER, ?MODULE).
-define(INVALID_SOCKET, 1).

start_link(Port) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

set_next_recursor(NextRecursor) ->
    gen_server:cast(?SERVER, {set_next_recursor, NextRecursor}).

stop() ->
    gen_server:call(?SERVER, stop).


init([Port]) ->
    case gen_udp:open(Port, [binary]) of
            {ok, Socket} ->
                io:format("server start to run ~n"),
                {ok, #state{socket = Socket}};
            {error, Reason} ->
                {stop, Reason}
    end.

handle_call(stop, _From, State) ->
    gen_udp:close(State#state.socket),
    {stop, normal, ok, #state{socket = -1}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({set_next_recursor, NextRecursor}, State) ->
    {noreply, State#state{next_recursor = NextRecursor}};

handle_cast({handle_response, IP, Port, Response},  #state{socket = Socket} = State) ->
    case gen_udp:send(Socket, IP, Port, Response) of
        ok -> {noreply, State};
        {error, Reason} -> io:format("send to end user failed ~p ~n", [Reason]),
                           {stop, Reason, State}
    end;

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({udp, _Socket, IP, Port, Packet}, #state{next_recursor = NextRecursor} = State) ->
    NextRecursor ! {handle_query, Packet},
    {noreply, State#state{ip = IP, port = Port}};

handle_info({handle_response, Packet}, #state{socket = Socket, ip = IP, port = Port} = State) ->
    case gen_udp:send(Socket, IP, Port, Packet) of
        ok -> {noreply, State}; 
        {error, Reason} -> io:format("send query failed: ~p ~n", [Reason]),
            {stop, Reason, State}
    end.


terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

