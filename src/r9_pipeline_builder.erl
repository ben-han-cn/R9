-module (r9_pipeline_builder).
-behaviour(gen_server).
-export([start_link/1, 
         recursor_header/0,
         stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
            recursors
        }).
-define(SERVER, ?MODULE).

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

stop() ->
    gen_server:call(?MODULE, stop).


init([Config]) ->
    {ok, Iterator} = r9_iterator:start_link(),
    {ok, Acceptor} = r9_acceptor:start_link(),
    {ok, LocalAuth} = r9_local_auth:start_link(Config),
    build_pipe_line([{r9_acceptor, Acceptor}, {r9_local_auth, LocalAuth}, {r9_iterator, Iterator}]),

    {ok, Parser} = r9_parser:start_link(),
    {ok, _Server} = r9_server:start_link(Config, Parser),
    {ok, #state{recursors = [Acceptor, LocalAuth, Iterator]}}.

recursor_header() ->
    gen_server:call(?MODULE, recursor_header).

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(recursor_header, _From, #state{recursors = Recursors} = State) ->
    {reply, {ok, lists:nth(1, Recursors)}, State};

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


%%impl
build_pipe_line([FirstRecursor|RestRecursor]) ->
    lists:foldl(fun(Recursor, PrevRecursor) -> 
                    connect(PrevRecursor, Recursor),
                    Recursor
            end, FirstRecursor, RestRecursor).

connect(PrevRecursor, Recursor) ->
    {PrevModule, PrevInstance} = PrevRecursor,
    {Module, Instance} = Recursor,
    apply(PrevModule, set_next_recursor, [Instance]),
    apply(Module, set_prev_recursor, [PrevInstance]).

