-module (r9_cache).
-behaviour(gen_server).

-export([start_link/0, 
         stop/1,
         put_message/2,
         get_message/3,
         get_rrset/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
            message_cache
        }).

-define(SERVER, ?MODULE).
-include("r9_dns.hrl").

start_link() -> 
    gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

put_message(Pid, Message) ->
    gen_server:cast(Pid, {put_message, Message}).

get_message(Pid, Name, Type) ->
    gen_server:call(Pid, {get_message, Name, Type}).

get_rrset(Pid, Name, Type) ->
    gen_server:call(Pid, {get_rrset, Name, Type}).

init([]) ->
    {ok, #state{message_cache = r9_message_redis:create()}}.

handle_call(stop, _From, #state{message_cache = MessageCache} = State) ->
    r9_message_redis:delete(MessageCache),
    {stop, normal, ok, State};

handle_call({get_message, Name, Type}, _From, #state{message_cache = MessageCache} = State) ->
    {reply, r9_message_redis:find_message(MessageCache, Name, Type), State};                                                 
handle_call({get_rrset, Name, Type}, _From, #state{message_cache = MessageCache} = State) ->
    {reply, r9_message_redis:find_rrset(MessageCache, Name, Type), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({put_message, Message}, #state{message_cache = MessageCache} = State) ->
    r9_message_redis:insert_message(MessageCache, Message),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("<< get unexpected message ~p ~n", [Msg]),
    {noreply, State}. 

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
