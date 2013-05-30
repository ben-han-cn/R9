-module (r9_zone_tracer).
-behaviour(gen_server).
-export([start_link/2, 
        stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {
        zone_name,
        cache,
        name_servers,
        socket,
        logger
    }).

-define(ITERATOR, ?MODULE).
-include("r9_resolver.hrl").

start_link(ZoneName, NameServers) ->
    gen_server:start_link({local, ?ITERATOR}, ?ITERATOR, [ZoneName, NameServers], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

init([ZoneName, NameServers]) ->
     case gen_udp:open(5554, [binary]) of 
         {ok, Socket} -> io:format("forwarder open socket succeed ~n"),
                {ok, Cache} = r9_cache:start_link(),
                {ok, Logger} = r9_logger:get_logger(lists:concat([?MODULE, ":", ZoneName])),
                {ok, #state{
                    zone_name = ZoneName,
                    name_servers = NameServers,
                    cache = Cache,
                    socket = Socket,
                    logger = Logger}};
        {error, Reason} -> io:format("for forwarder open socket failed: ~p ~n", [Reason]),
            {stop, Reason}
    end.


handle_call(stop, _From, State) ->
    r9_logger:stop(State#state.logger),
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, _Socket, _IP, _Port, Packet}, State) ->
    {Message, _} = r9_message:from_wire(Packet, 0),
    r9_logger:log(State#state.logger, lists:concat(["get response for ", r9_message_question:to_string(r9_message:question(Message))])),
    r9_cache:put_message(State#state.cache, Message),
    {noreply, State};

handle_info({handle_query, #request{question = Question}}, State) ->
    QueryName = r9_message_question:name(Question), 
    case r9_wire_name:contains(State#state.zone_name, QueryName) of
            true -> 
                resolver_helper(Question, State);
            false ->
                io:format("---> handle query which isn't under current zone")
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


resolver_helper(Question, State) ->
    case r9_cache:get_message(State#state.cache,
                              r9_message_question:name(Question), 
                              r9_message_question:type(Question)) of
        {ok, _CachedMessage} ->
            r9_logger:log(State#state.logger, lists:concat(["cache hit for ", r9_message_question:to_string(Question)]));
        {not_found} ->
            DelegateQuery = r9_message:to_wire(r9_message:make_query(Question)),
            r9_logger:log(State#state.logger, lists:concat(["forward query ", r9_message_question:to_string(Question)])),
            NameServer = lists:nth(State#state.name_servers, 0),
            gen_udp:send(State#state.socket, NameServer, 53, DelegateQuery) 
    end.
