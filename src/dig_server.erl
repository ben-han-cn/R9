-module (dig_server).
-behaviour(gen_server).
-import(dig_echo).

-export([start_link/1, run/1, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
            socket_,
            is_running_,
            children_ ,
            next_child_to_query = 1
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
                tuple_to_list(State#state.children_)),

                {reply, ok, State#state{socket_ = Socket, is_running_ = true} };
            {error, _Reason} ->
                {reply, error, State}
        end;

handle_call(stop, _From, State) ->
    lists:foreach(fun (Child) ->
                    dig_echo:stop(Child)
                  end,
                  tuple_to_list(State#state.children_)),
    gen_udp:close(State#state.socket_),
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({udp, Socket, IP, Port, Packet}, State) ->
    ChildToQuery = State#state.next_child_to_query,
    dig_echo:handle_query(element(ChildToQuery, State#state.children_),
                          Socket,
                          IP,
                          Port,
                          Packet),
    NextChildToQuery = next_child_to_query(ChildToQuery, State),
    {noreply, State#state{next_child_to_query = NextChildToQuery}}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


create_child(Count) ->
    create_child_helper(Count, {}).

create_child_helper(0, Children) ->
    Children;
create_child_helper(N, Children) ->
    NewChildren = erlang:append_element(Children, list_to_atom(lists:concat([child, N]))),
    create_child_helper(N - 1, NewChildren).

next_child_to_query(CurrentChildIndex, State) ->
    ChildCount = tuple_size(State#state.children_),
    if 
        CurrentChildIndex =:= ChildCount ->  1;
        true -> CurrentChildIndex + 1
    end.
 


