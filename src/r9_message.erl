-module(r9_message).

-export([from_wire/1,
         from_wire/2,
         to_wire/1,
         make_query/1,
         header/1,
         question/1,
         answer_section/1,
         authority_section/1,
         additional_section/1,
         sections/1,
         set_flag/3,
         set_id/2
        ]).


-include("r9_dns.hrl").


from_wire(WireData) ->
    from_wire(WireData, 0).
from_wire(WireData, CurrentPos) ->
    {Header, NextPos} = r9_message_header:from_wire(WireData, CurrentPos),
    {Question, NNextPos} = r9_message_question:from_wire(WireData, NextPos),
    {AnswerSection, NNNextPos} = r9_message_section:from_wire(WireData, NNextPos, r9_message_header:answer_section_count(Header)),
    {AuthoritySection, NNNNextPos} = r9_message_section:from_wire(WireData, NNNextPos, r9_message_header:authority_section_count(Header)),
    {AdditionalSection, NNNNNextPos} = r9_message_section:from_wire(WireData, NNNNextPos, r9_message_header:additional_section_count(Header)),
    {#message{header = Header,
             question = Question,
             answer_section = AnswerSection,
             authority_section = AuthoritySection,
             additional_section = AdditionalSection}, NNNNNextPos}.

to_wire(Message) ->
    list_to_binary([r9_message_header:to_wire(header(Message)),
                    r9_message_question:to_wire(question(Message)),
                    r9_message_section:to_wire(answer_section(Message)),
                    r9_message_section:to_wire(authority_section(Message)),
                    r9_message_section:to_wire(additional_section(Message))]).


make_query(Question) ->
    #message{header = r9_message_header:make_query_header(),
             question = Question,
             answer_section = r9_message_section:from_rrsets([]),
             authority_section = r9_message_section:from_rrsets([]),
             additional_section = r9_message_section:from_rrsets([])}.

header(Message) -> Message#message.header.
question(Message) -> Message#message.question.
answer_section(Message) -> Message#message.answer_section.
authority_section(Message) -> Message#message.authority_section.
additional_section(Message) -> Message#message.additional_section.
sections(Message) -> [answer_section(Message), authority_section(Message), additional_section(Message)].


set_flag(#message{header = Header} = Message, Flag, Value) ->
    Message#message{header = r9_message_header:set_flag(Header, Flag, Value)}.

set_id(#message{header = Header} = Message, ID) ->
    Message#message{header = r9_message_header:set_id(Header, ID)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
message_from_wire_test()->
    WireData = <<16#2c,16#4e,16#80,16#80,16#0,16#1,16#0,16#1,16#0,16#0,16#0,16#0,16#2,16#63,16#6e,16#0,16#0,16#6,16#0,16#1,16#c0,16#c,16#0,16#6,16#0,16#1,16#0,16#0,16#2b,16#2e,16#0,16#29,16#1,16#61,16#3,16#64,16#6e,16#73,16#c0,16#c,16#4,16#72,16#6f,16#6f,16#74,16#5,16#63,16#6e,16#6e,16#69,16#63,16#c0,16#c,16#78,16#6,16#99,16#a,16#0,16#0,16#1c,16#20,16#0,16#0,16#e,16#10,16#0,16#24,16#ea,16#0,16#0,16#0,16#54,16#60>>,
    {Message, NextPosToParse} = from_wire(WireData, 0),
%    ?assertEqual(to_wire(Message), WireData),
    Header = header(Message),
    ?assertEqual(r9_message_header:id(Header), 11342),
    ?assertEqual(r9_message_header:question_section_count(Header), 1),
    ?assertEqual(r9_message_header:answer_section_count(Header), 1),

    Question = question(Message),
    ?assert(r9_wire_name:is_equal(r9_message_question:name(Question), r9_wire_name:from_string("cn."))),
    ?assertEqual(r9_message_question:type(Question), ?TYPE_SOA),

    AnswerSection = answer_section(Message),
    [RR_SOA|_] = r9_message_section:rrs(AnswerSection),

    ?assertEqual(r9_wire_name:to_string(r9_rr:name(RR_SOA)), "cn."),
    ?assertEqual(r9_rr:type(RR_SOA), ?TYPE_SOA),
    ?assertEqual(r9_rr:class(RR_SOA), ?CLASS_IN),
    ?assertEqual(r9_rr:ttl(RR_SOA), 11054),

    SOARdata = r9_rr:rdata(RR_SOA),
    ?assertEqual(r9_wire_name:to_string(SOARdata#soa.mname), "a.dns.cn."),
    ?assertEqual(r9_wire_name:to_string(SOARdata#soa.rname), "root.cnnic.cn."),
    ?assertEqual(SOARdata#soa.serial, 2013698314),
    ?assertEqual(SOARdata#soa.refresh, 7200),
    ?assertEqual(SOARdata#soa.retry, 3600),
    ?assertEqual(SOARdata#soa.expire, 2419200),
    ?assertEqual(SOARdata#soa.minimum, 21600),
    ?assertEqual(NextPosToParse, 73).

message_from_wire_bug_test()->
    WireData = <<185,25,129,128,0,1,0,1,0,0,0,0,4,116,105,109,101,4,97,115,105,97,5,97,112,112,108,101,3,99,111,109,0,0,16,0,1,192,12,0,16,0,1,0,0,11,119,0,32,31,110,116,112,32,109,105,110,112,111,108,108,32,57,32,109,97,120,112,111,108,108,32,49,50,32,105,98,117,114,115,116>>,
    {Message, _NextPosToParse} = from_wire(WireData, 0),
    Question = question(Message),
    ?assert(r9_wire_name:is_equal(r9_message_question:name(Question), r9_wire_name:from_string("time.asia.apple.com."))).

-endif.

