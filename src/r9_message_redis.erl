-module (r9_message_redis).
-export([create/0,
         insert_message/2,
         delete_message/3,
         destroy/1,
         find_rrset/3,
         find_message/3]).

-record(message_entry, {header,
            question,
            answer_rrset_keys,
            authority_rrset_keys,
            additional_rrset_keys}).

-record(message_cache, {
            redis_link,
            rrset_cache}).
-include("r9_dns.hrl").

create() ->
    {ok, C} = eredis:start_link(),
    #message_cache{redis_link = C,
                   rrset_cache = r9_rrset_redis:create()}.

insert_message(#message_cache{redis_link = C , rrset_cache = RRsetCache}, Message) ->
    {AnswerMinTTl, AnswerSectionRRsetKeys} = store_section(RRsetCache, r9_message:answer_section(Message)),
    {AdditionalMinTTl, AdditionalRRsetKeys} = store_section(RRsetCache, r9_message:authority_section(Message)),
    {AuthorityMinTTl, AuthorityRRsetKeys} = store_section(RRsetCache, r9_message:additional_section(Message)),

    MinRRsetTTL = lists:min([AnswerMinTTl, AdditionalMinTTl, AuthorityMinTTl]),
    eredis:q(C, ["SETEX", message_id(Message), MinRRsetTTL, #message_entry{ header = r9_message:header(Message),
                                                                            question = r9_message:question(Message),
                                                                            answer_rrset_keys = AnswerSectionRRsetKeys,
                                                                            authority_rrset_keys = AuthorityRRsetKeys,
                                                                            additional_rrset_keys = AdditionalRRsetKeys}]).

message_id(Message) when is_record(Message, message)->
    message_id(r9_message:question(Message));
message_id(Question) when is_record(Question, question)->
    message_id(r9_message_question:id(Question));
message_id(MessageID) ->
    lists:concat(["M+", MessageID]).

message_id(Name, Type) -> 
    message_id(r9_message_question:id(Name, Type)).


store_section(RRsetCache, Section) ->
    r9_rrset_redis:insert_rrsets(RRsetCache, r9_message_section:rrsets(Section)).
    

find_section(RRsetCache, SectionRRsetIDs) ->
    lists:foldl(fun(RRsetID, RRsets) ->
                case r9_rrset_redis:find_rrset(RRsetCache, RRsetID) of 
                        {ok, RRset} -> [RRset | RRsets];
                        {not_found} -> RRsets
                    end
            end, [], SectionRRsetIDs).

find_message(#message_cache{redis_link = C, rrset_cache = RRsetCache}, Name, Type) ->
    MessageID = message_id(Name, Type),
    case eredis:q(C, ["GET", MessageID]) of
        {ok, undefined} -> {not_found};
        {ok, MessageEntryBinary} ->
            #message_entry{header = Header,
                        question = Question,
                        answer_rrset_keys = AnswerRRsetKeys,
                        authority_rrset_keys = AuthorityRRsetKeys,
                        additional_rrset_keys = AdditionalRRsetKeys} = binary_to_term(MessageEntryBinary),
            AnswerSection = r9_message_section:from_rrsets(find_section(RRsetCache, AnswerRRsetKeys)),
            AuthoritySection = r9_message_section:from_rrsets(find_section(RRsetCache, AuthorityRRsetKeys)),
            AdditionalSection= r9_message_section:from_rrsets(find_section(RRsetCache, AdditionalRRsetKeys)),
            {ok, #message{header = Header,
                   question = Question,
                   answer_section = AnswerSection,
                   authority_section = AuthoritySection,
                   additional_section = AdditionalSection}}
    end.

find_rrset(#message_cache{rrset_cache = RRsetCache}, Name, Type) ->
    r9_rrset_redis:find_rrset(RRsetCache, Name, Type).

delete_message(#message_cache{redis_link = C}, Name, Type) ->
    eredis:q(C, ["DEL", message_id(Name, Type)]).

destroy(#message_cache{rrset_cache = RRsetCache}) ->
    r9_rrset_redis:destroy(RRsetCache).
