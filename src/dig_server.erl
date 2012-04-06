-module (dig_server).
-behaviour(gen_server).
-import(dig_echo).

-export([start_link/1, run/1, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
            socket_,
            is_running_,
            child_count_ = 0,
            children_ = []  %lists store all the children's pid
        }).
-define(SERVER, ?MODULE).

start_link(ChildCount) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [ChildCount], []).

run(Port) ->
    gen_server:call(?SERVER, {run, Port}).

stop() ->
    gen_server:call(?SERVER, stop).


init([ChildCount]) ->
    process_flag(trap_exit, true),
    {ok, #state{is_running_ = false, child_count_ = ChildCount}}.

handle_call({run, _Port}, _From, State) when State#state.is_running_ ->
        io:format("server already run"),
        {reply, ok, State};
handle_call({run, Port}, _From, State) ->
        case gen_udp:open(Port, [binary]) of
            {ok, Socket} ->
                io:format("server start to run ~n"),
                Children = lists:foldl(fun (_Index, ChildrenTmp) ->
                                            {ok, Child} = dig_echo:start_link(),
                                            [Child | ChildrenTmp]
                                       end,
                                       [],
                                       lists:seq(1, State#state.child_count_)),
                {reply, ok, State#state{socket_ = Socket, is_running_ = true, children_ = Children}};
            {error, _Reason} ->
                {reply, error, State}
        end;

handle_call(stop, _From, State) ->
    lists:foreach(fun (Child) ->
                    dig_echo:stop(Child)
                  end,
                  State#state.children_),
    gen_udp:close(State#state.socket_),
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({udp, Socket, IP, Port, Packet}, State) ->
    [ChildToQuery | RestChild] = State#state.children_,
    dig_echo:handle_query(ChildToQuery,
                          Socket,
                          IP,
                          Port,
                          Packet),
                      {noreply, State#state{children_ = lists:append(RestChild, [ChildToQuery])}};
handle_info({'EXIT', Pid, _Reason}, State) ->
    ActiveChildren = State#state.children_,
    case lists:member(Pid, ActiveChildren) of 
        true -> 
            io:format("process [~p] dead restart new process ~n", [Pid]),
            {ok, NewChild} = dig_echo:start_link(),
            {noreply, State#state{children_ = [NewChild | lists:delete(Pid, ActiveChildren)]}};
        _ ->
            io:format("unknown process dead [~p] ~n", [Pid]),
            {noreply, State}
    end.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

