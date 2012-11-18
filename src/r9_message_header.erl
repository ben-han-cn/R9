-module(r9_message_header).

-export([from_wire/2,
        id/1,
        print/1,
        set_qr/1,
        qr/1,
        opcode/1,
        tc/1,
        rd/1,
        ra/1,
        question_section_count/1,
        answer_section_count/1,
        authority_section_count/1,
        additional_section_count/1
    ]).

-include("r9_dns.hrl").

id(Header) -> Header#message_header.id.
qr(Header) -> Header#message_header.qr.
opcode(Header) -> Header#message_header.opcode.
tc(Header) -> Header#message_header.tc.
rd(Header) -> Header#message_header.rd.
ra(Header) -> Header#message_header.ra.
question_section_count(Header) -> Header#message_header.question_sec_count.
answer_section_count(Header) -> Header#message_header.answer_sec_count.
authority_section_count(Header) -> Header#message_header.authority_sec_count.
additional_section_count(Header) -> Header#message_header.additional_sec_count.

from_wire(WireData, CurrentPos) ->
    <<_ParsedData:CurrentPos/bytes, 
      ID:16/big, 
      Flags:16/big, 
      QuestionCount:16/integer-big, 
      AnswerCount:16/integer-big, 
      AuthorityCount:16/integer-big, 
      AdditionalCount:16/integer-big, 
      _/bits>> = WireData,

    <<Qr:1, Opcode:4, AA:1, TC:1, RD:1, RA:1, _Z:3, RCode:4>> = <<Flags:16>>,
    {#message_header{id = ID, 
                    qr = Qr, 
                    opcode = Opcode, 
                    aa = AA, 
                    tc = TC, 
                    rd = RD, 
                    ra = RA, 
                    rcode = RCode, 
                    question_sec_count = QuestionCount, 
                    answer_sec_count = AnswerCount, 
                    authority_sec_count = AuthorityCount, 
                    additional_sec_count = AdditionalCount}, CurrentPos + 12}.


set_qr(WireData) when is_bitstring(WireData) and (byte_size(WireData) >= 12) ->
    <<ID:16/bits, Flags:15/bits, _Qr:1, Others/bits>> = WireData,
    <<ID:16/bits, 1:1/integer, Flags:15/bits, Others/bits>>.


%% bind format
%%  ;; ->>HEADER<<- opcode: QUERY, status: NOERROR, id: 63196
%%  ;; flags: qr rd ra; QUERY: 1, ANSWER: 3, AUTHORITY: 4, ADDITIONAL: 4
print(MessageHeader) when is_record(MessageHeader, message_header) ->
    io:format(";; ->>HEADER<<- opcode: ~s, status: ~s, id: ~p ~n",
            [opcode_to_str(MessageHeader#message_header.opcode),
            rcode_to_str(MessageHeader#message_header.rcode),
            MessageHeader#message_header.id]),
    io:format(";; flags:"),
    MessageHeader#message_header.qr =:= 1 andalso io:format(" qr"),
    MessageHeader#message_header.aa =:= 1 andalso io:format(" aa"),
    MessageHeader#message_header.tc =:= 1 andalso io:format(" tc"),
    MessageHeader#message_header.rd =:= 1 andalso io:format(" rd"),
    MessageHeader#message_header.ra =:= 1 andalso io:format(" ra"),
    MessageHeader#message_header.ad =:= 1 andalso io:format(" ad"),
    MessageHeader#message_header.cd =:= 1 andalso io:format(" cd"),

    io:format("; Query: ~p, ANSWER: ~p, AUTHORITY: ~p, ADDITIONAL: ~p ~n",
        [MessageHeader#message_header.question_sec_count,
        MessageHeader#message_header.answer_sec_count,
        MessageHeader#message_header.authority_sec_count,
        MessageHeader#message_header.additional_sec_count]).
        
%% convert opcode/rcode to readable string
opcode_to_str(Opcode) ->
    lists:nth(Opcode + 1, ?OPCODE_STR).
rcode_to_str(Rcode) ->
    lists:nth(Rcode + 1, ?RCODE_STR).



%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
dig_message_header_test()->
    {Header, _} = from_wire(<<16#2c,16#4e,16#80,16#80,16#0,16#1,16#0,16#1,16#0,16#0,16#0,16#0,16#2,16#63,16#6e,16#0,16#0,16#6,16#0,16#1>>, 0),
    print(Header),
    ?assertEqual(1, 2).

-endif.

