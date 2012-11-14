-module(r9_message_header).

-export([from_wire/1,
        print/1,
        set_qr/1]).

-include("r9_dns.hrl").

from_wire(WireData) when is_bitstring(WireData) and (byte_size(WireData) >= 12) ->
    <<ID:16/integer-big, Flags:16/integer-big, QuestionCount:16/integer-big, AnswerCount:16/integer-big, AuthorityCount:16/integer-big, AdditionalCount:16/integer-big, _Question/bits>> = WireData,
    <<Qr:1/integer, Opcode:4/integer, AA:1/integer, TC:1/integer, RD:1/integer, RA:1/integer, _Z:3/integer, RCode:4/integer >> = <<Flags:16>>,
    #message_header{id = ID, 
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
                    additional_sec_count = AdditionalCount}.


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
    io:format(";; falgs:"),
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
    TwoShortData = <<1>>,
    ?assertException(error, function_clause, from_wire(TwoShortData)).
-endif.
