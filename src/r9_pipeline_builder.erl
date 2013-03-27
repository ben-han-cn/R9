-module (r9_pipeline_builder).
-behaviour(gen_server).
-export([start_link/1, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
            recursors
        }).
-define(SERVER, ?MODULE).

start_link(Port) ->
    gen_server:start_link(?MODULE, [Port], []).

stop(Pid) ->
    gen_server:call(Pid, stop).


init([Port]) ->
    {ok, Iterator} = r9_iterator:start_link(),
    {ok, Acceptor} = r9_acceptor:start_link(),
    r9_acceptor:set_next_recursor(Iterator),
    r9_iterator:set_prev_recursor(Acceptor),
    {ok, _Server} = r9_server:start_link(Port, Acceptor),
    io:format("----> server start at ~p ~n", [Port]),
    {ok, #state{recursors = [Acceptor, Iterator]}}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Reason, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
