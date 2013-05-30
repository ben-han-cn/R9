-module (r9_parser).
-behaviour(gen_server).

-export([start_link/0, 
         stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
            logger
        }).

-define(SERVER, ?MODULE).

-include("r9_dns.hrl").
-include("r9_resolver.hrl").

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).


init([]) ->
    {ok, Logger} = r9_logger:get_logger(?MODULE),
    {ok, #state{logger = Logger}}.

handle_call(stop, _From, State) ->
    r9_logger:stop(State#state.logger),
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({accept_request, Client, Packet}, State) ->
    {Message, _} = r9_message:from_wire(Packet),
    ID = r9_message_header:id(r9_message:header(Message)),
    Question = r9_message:question(Message),
    r9_logger:log(State#state.logger, r9_message_question:to_string(Question)),
    {ok, RecursorHeader} = r9_pipeline_builder:recursor_header(),
    RecursorHeader ! {handle_query, #request{id = ID, 
                       question = Question,
                       client = Client}},
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
