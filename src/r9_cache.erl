-module (r9_cache).
-behaviour(gen_server).

-export([start_link/0, 
         stop/0,
         put_message/1,
         get_message/2,
         get_rrset/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
            message_table,
            rrset_table
        }).

-record(message_entry, {
            message_id,
            header,
            question,
            answer_rrset_keys,
            authority_rrset_keys,
            additional_rrset_keys}).

-record(rrset_entry, {
            rrset_id,
            rrset}).

-define(SERVER, ?MODULE).
-include("r9_dns.hrl").

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:call(?SERVER, stop).

put_message(Message) ->
    gen_server:cast(?SERVER, {put_message, Message}).

get_message(Name, Type) ->
    gen_server:call(?SERVER, {get_message, Name, Type}).

get_rrset(Name, Type) ->
    gen_server:call(?SERVER, {get_rrset, Name, Type}).

init([]) ->
    {ok, #state{message_table = ets:new(message_table, [{keypos, #message_entry.message_id},
                                                       {read_concurrency, true},
                                                       {write_concurrency, true}]),
                rrset_table = ets:new(rrset_table, [{keypos, #rrset_entry.rrset_id},
                                                    {read_concurrency, true},
                                                    {write_concurrency, true}])}}.

handle_call(stop, _From, State) ->
    ets:delete(State#state.message_table),
    ets:delete(State#state.rrset_table),
    {stop, normal, ok, State};

handle_call({get_message, Name, Type}, _From, #state{message_table = MessageTable, 
                                                     rrset_table = RRsetTable} = State) ->
    {reply, find_message(MessageTable, RRsetTable, Name, Type), State};                                                 
handle_call({get_rrset, Name, Type}, _From, #state{rrset_table = RRsetTable} = State) ->
    {reply, find_rrset(RRsetTable, Name, Type), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({put_message, Message}, #state{message_table = MessageTable,
                                           rrset_table = RRsetTable} = State) ->

    AnswerSectionRRsetKeys = store_section(RRsetTable, r9_message:answer_section(Message)),
    AdditionalRRsetKeys = store_section(RRsetTable, r9_message:authority_section(Message)),
    AuthorityRRsetKeys = store_section(RRsetTable, r9_message:additional_section(Message)),
    ets:insert(MessageTable, #message_entry{message_id = message_id(Message),
                                            header = r9_message:header(Message),
                                            question = r9_message:question(Message),
                                            answer_rrset_keys = AnswerSectionRRsetKeys,
                                            authority_rrset_keys = AuthorityRRsetKeys,
                                            additional_rrset_keys = AdditionalRRsetKeys}),
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


%% util
message_id(Message) ->
    Question = r9_message:question(Message),
    generate_id(r9_message_question:name(Question), r9_message_question:type(Question)).
                  
generate_id(Name, Type) ->
    lists:concat([r9_wire_name:to_string(Name), "/", r9_rr:rtype_to_string(Type)]).

rrset_id(RRset) ->
    generate_id(r9_rrset:name(RRset), r9_rrset:type(RRset)).

store_section(RRsetTable, Section) ->
    lists:foldl(fun(RRset, IDs) ->
                    RRsetID = rrset_id(RRset),
                    ets:insert(RRsetTable, #rrset_entry{rrset_id = RRsetID,
                                                        rrset = RRset}),
                    [RRsetID | IDs] 
            end, [], r9_message_section:rrsets(Section)).

find_rrset(RRsetTable, Name, Type) ->
    find_rrset(RRsetTable, generate_id(Name, Type)).

find_rrset(RRsetTable, RRsetID) ->
    case ets:lookup(RRsetTable, RRsetID) of
        [] -> {not_found};
        [#rrset_entry{rrset = RRset}] -> {ok, RRset}
    end.

find_section(RRsetTable, SectionRRsetIDs) ->
    lists:foldl(fun(RRsetID, RRsets) ->
                    {ok, RRset} = find_rrset(RRsetTable, RRsetID),
                    [RRset | RRsets]
            end, [], SectionRRsetIDs).

find_message(MessageTable, RRsetTable, Name, Type) ->
    case ets:lookup(MessageTable, generate_id(Name, Type)) of
        [] -> 
            {not_found};
        [#message_entry{header = Header,
                        question = Question,
                        answer_rrset_keys = AnswerRRsetKeys,
                        authority_rrset_keys = AuthorityRRsetKeys,
                        additional_rrset_keys = AdditionalRRsetKeys}] -> 
                    AnswerSection = r9_message_section:from_rrsets(find_section(RRsetTable, AnswerRRsetKeys)),
                    AuthoritySection = r9_message_section:from_rrsets(find_section(RRsetTable, AuthorityRRsetKeys)),
                    AdditionalSection= r9_message_section:from_rrsets(find_section(RRsetTable, AdditionalRRsetKeys)),
            {ok, #message{header = Header,
                          question = Question,
                          answer_section = AnswerSection,
                          authority_section = AuthoritySection,
                          additional_section = AdditionalSection}}
    end.
