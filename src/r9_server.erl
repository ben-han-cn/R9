-module (r9_server).
-behaviour(gen_server).
-import(r9_echo).

-export([start_link/2, 
         stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
            acceptor,
            ip,
            port,
            socket
        }).

-define(SERVER, ?MODULE).
-define(INVALID_SOCKET, 1).
-define(DEFAULT_LISTEN_PORT, 53).

-include("r9_resolver.hrl"). 

start_link(Config, Acceptor) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Config, Acceptor], []).

stop() ->
    gen_server:call(?SERVER, stop).


init([Config, Acceptor]) ->
    Port = case r9_config:get(Config, "listen_port") of
        {ok, PortStr} -> r9_util:string_to_integer(PortStr);
        {not_found} -> ?DEFAULT_LISTEN_PORT
    end,
    case gen_udp:open(Port, [binary]) of
            {ok, Socket} ->
                io:format("server start to run ~n"),
                {ok, #state{socket = Socket, acceptor = Acceptor}};
            {error, Reason} ->
                {stop, Reason}
    end.

handle_call(stop, _From, State) ->
    gen_udp:close(State#state.socket),
    {stop, normal, ok, #state{socket = -1}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast({handle_response, IP, Port, Response},  #state{socket = Socket} = State) ->
    case gen_udp:send(Socket, IP, Port, Response) of
        ok -> {noreply, State};
        {error, Reason} -> io:format("send to end user failed ~p ~n", [Reason]),
                           {stop, Reason, State}
    end;

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({udp, Socket, IP, Port, Packet}, #state{acceptor = Acceptor} = State) ->
    Acceptor ! {accept_request, Socket, #host{ip = IP, port = Port}, Packet},
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

