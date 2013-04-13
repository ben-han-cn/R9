-module (r9_local_auth).
-behaviour(gen_server).
-export([start_link/1, 
        set_prev_recursor/1,
        set_next_recursor/1,
        stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {
        local_data,
        prev_recursor,
        next_recursor
    }).

-include("r9_resolver.hrl").

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

stop(Pid) ->
    gen_server:call(Pid, stop).

set_prev_recursor(PrevRecursor) ->
    gen_server:cast(?MODULE, {set_prev_recursor, PrevRecursor}).
set_next_recursor(NextRecursor) ->
    gen_server:cast(?MODULE, {set_next_recursor, NextRecursor}).

init([Config]) ->
    {ok, LocalZonePath} = r9_config:get(Config, "local_zone_path"),
    {ok, #state{local_data = r9_local_data:load(LocalZonePath)}}.

handle_call(stop, _From, #state{local_data = LocalData} = State) ->
    r9_local_data:destroy(LocalData),
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({set_prev_recursor, PrevRecursor}, State) ->
    {noreply, State#state{prev_recursor = PrevRecursor}};

handle_cast({set_next_recursor, NextRecursor}, State) ->
    {noreply, State#state{next_recursor = NextRecursor}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({handle_query, #request{question = Question} = Request}, 
            #state{local_data = LocalData} = State) ->
    case get_local_data(LocalData, Question) of
        {ok, LocalMessage} ->
            State#state.prev_recursor ! {handle_response, #response{question = Question, 
                                                                    reply_message = LocalMessage}},
            {noreply, State}; 
        {not_found} ->
            State#state.next_recursor! {handle_query, Request},
            {noreply, State}
    end;

handle_info({handle_response, Response},  State) ->
    State#state.prev_recursor ! {handle_response, Response},
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% impl
get_local_data(LocalData, Question) ->
    case r9_local_data:find_rrset(LocalData, 
                                  r9_message_question:name(Question), 
                                  r9_message_question:type(Question)) of
        {ok, RRset} -> {ok, r9_message:make_response(Question, [RRset], [], [])};
        {not_found} -> {not_found}
    end.
