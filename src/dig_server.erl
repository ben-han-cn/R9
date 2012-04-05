-module (dig_server).
-behaviour(gen_server).
-import(dig_echo).

-export([start_link/1, run/1, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
            socket_,
            is_running_,
            children_  %lists store all the children's name
        }).
-define(SERVER, ?MODULE).

start_link(ChildCount) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [ChildCount], []).

run(Port) ->
    gen_server:call(?SERVER, {run, Port}).

stop() ->
    gen_server:call(?SERVER, stop).


init([ChildCount]) ->
    {ok, #state{is_running_ = false, children_ = create_child(ChildCount)}}.

handle_call({run, _Port}, _From, State) when State#state.is_running_ ->
        io:format("server already run"),
        {reply, ok, State};
handle_call({run, Port}, _From, State) ->
        case gen_udp:open(Port, [binary]) of
            {ok, Socket} ->
                io:format("server start to run ~n"),
                lists:foreach(fun (Child) ->
                                dig_echo:start_link(Child)
                              end,
                State#state.children_),

                {reply, ok, State#state{socket_ = Socket, is_running_ = true} };
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
                      {noreply, State#state{children_ = lists:append(RestChild, [ChildToQuery])}}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


create_child(Count) ->
    lists:map(fun (ChildNum) ->
                list_to_atom(lists:concat([child, ChildNum]))
              end, 
              lists:seq(1, Count)).

 


