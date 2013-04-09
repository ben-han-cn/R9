-module (r9_message_cache).
-export([create/0,
         insert_message/2,
         delete_message/3,
         destroy/1,
         find_rrset/3,
         find_message/3]).

-record(message_entry, {
            message_id,
            header,
            question,
            answer_rrset_keys,
            authority_rrset_keys,
            additional_rrset_keys,
            expiration}).

-record(message_cache, {
            message_table,
            rrset_cache}).
-include("r9_dns.hrl").

create() ->
    #message_cache{message_table = ets:new(message_table, [{keypos, #message_entry.message_id},
                                                           {read_concurrency, true},
                                                           {write_concurrency, true}]),
                   rrset_cache = r9_rrset_cache:create()}.

insert_message(#message_cache{message_table = MessageTable, rrset_cache = RRsetCache}, Message) ->
    {AnswerMinTTl, AnswerSectionRRsetKeys} = store_section(RRsetCache, r9_message:answer_section(Message)),
    {AdditionalMinTTl, AdditionalRRsetKeys} = store_section(RRsetCache, r9_message:authority_section(Message)),
    {AuthorityMinTTl, AuthorityRRsetKeys} = store_section(RRsetCache, r9_message:additional_section(Message)),
    ets:insert(MessageTable, #message_entry{message_id = message_id(Message),
                                            header = r9_message:header(Message),
                                            question = r9_message:question(Message),
                                            answer_rrset_keys = AnswerSectionRRsetKeys,
                                            authority_rrset_keys = AuthorityRRsetKeys,
                                            additional_rrset_keys = AdditionalRRsetKeys,
                                            expiration = r9_util:local_now() + lists:min([AnswerMinTTl, AdditionalMinTTl, AuthorityMinTTl])}).

message_id(Message) ->
    r9_message_question:id(r9_message:question(Message)).


store_section(RRsetCache, Section) ->
    r9_rrset_cache:insert_rrsets(RRsetCache, r9_message_section:rrsets(Section)).
    

find_section(RRsetCache, SectionRRsetIDs) ->
    lists:foldl(fun(RRsetID, RRsets) ->
                case r9_rrset_cache:find_rrset(RRsetCache, RRsetID) of 
                        {ok, RRset} -> [RRset | RRsets];
                        {not_found} -> RRsets
                    end
            end, [], SectionRRsetIDs).

find_message(#message_cache{message_table = MessageTable, rrset_cache = RRsetCache}, Name, Type) ->
    MessageID = r9_message_question:id(Name, Type),
    case ets:lookup(MessageTable, MessageID) of
        [] -> 
            {not_found};
        [#message_entry{header = Header,
                        question = Question,
                        answer_rrset_keys = AnswerRRsetKeys,
                        authority_rrset_keys = AuthorityRRsetKeys,
                        additional_rrset_keys = AdditionalRRsetKeys,
                        expiration = ExpireTime}] -> 
            case r9_util:local_now() > ExpireTime of 
                true -> 
                    ets:delete(MessageTable, MessageID),
                    {not_found};
                false -> 
                    AnswerSection = r9_message_section:from_rrsets(find_section(RRsetCache, AnswerRRsetKeys)),
                    AuthoritySection = r9_message_section:from_rrsets(find_section(RRsetCache, AuthorityRRsetKeys)),
                    AdditionalSection= r9_message_section:from_rrsets(find_section(RRsetCache, AdditionalRRsetKeys)),
                    {ok, #message{header = Header,
                          question = Question,
                          answer_section = AnswerSection,
                          authority_section = AuthoritySection,
                          additional_section = AdditionalSection}}
          end
    end.

find_rrset(#message_cache{rrset_cache = RRsetCache}, Name, Type) ->
    r9_rrset_cache:find_rrset(RRsetCache, Name, Type).

delete_message(#message_cache{message_table = MessageTable}, Name, Type) ->
    ets:delete(MessageTable, r9_message_question:id(Name, Type)).

destroy(#message_cache{message_table = MessageTable, rrset_cache = RRsetCache}) ->
    r9_rrset_cache:destroy(RRsetCache),
    ets:delete(MessageTable).
