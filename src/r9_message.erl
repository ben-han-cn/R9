-module(r9_message).

-export([from_wire/2,
         header/1,
         question/1,
         answer_section/1,
         authority_section/1,
         additional_section/1,
         sections/1
        ]).


-record(message, { header,
                   question,
                   answer_section,
                   authority_section,
                   additional_section}).

-include("r9_dns.hrl").


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

header(Message) -> Message#message.header.
question(Message) -> Message#message.question.
answer_section(Message) -> Message#message.answer_section.
authority_section(Message) -> Message#message.authority_section.
additional_section(Message) -> Message#message.additional_section.
sections(Message) -> [answer_section(Message), authority_section(Message), additional_section(Message)].



%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
message_from_wire_test()->
    WireData = <<16#2c,16#4e,16#80,16#80,16#0,16#1,16#0,16#1,16#0,16#0,16#0,16#0,16#2,16#63,16#6e,16#0,16#0,16#6,16#0,16#1,16#c0,16#c,16#0,16#6,16#0,16#1,16#0,16#0,16#2b,16#2e,16#0,16#29,16#1,16#61,16#3,16#64,16#6e,16#73,16#c0,16#c,16#4,16#72,16#6f,16#6f,16#74,16#5,16#63,16#6e,16#6e,16#69,16#63,16#c0,16#c,16#78,16#6,16#99,16#a,16#0,16#0,16#1c,16#20,16#0,16#0,16#e,16#10,16#0,16#24,16#ea,16#0,16#0,16#0,16#54,16#60>>,
    {Message, NextPosToParse} = from_wire(WireData, 0),
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
-endif.

