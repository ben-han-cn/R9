-module (r9_logger).
-behaviour(gen_server).
-export([get_logger/1,
        log/2,
        stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {
        mod_name,
        output
    }).

-include("r9_resolver.hrl").

get_logger(Mod) ->
    gen_server:start_link(?MODULE, [Mod], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

log(Pid, Info) ->
    gen_server:cast(Pid, {log, Info}).


init([Mod]) ->
    {ok, #state{mod_name = atom_to_list(Mod)}}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({log, Info}, #state{mod_name = ModName} = State) ->
    io:format("~s ~s [~s] ~n", [r9_util:date_to_string(calendar:local_time()), ModName, Info]), 
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
