-module(r9_rr).
-export([from_wire/2,
        to_wire/1,
        from_string/1,
        to_string/1,
        belongs_to_rrset/2,
        name/1,
        type/1,
        class/1,
        ttl/1,
        rtype_to_string/1,
        rdata/1]).

-include("r9_dns.hrl").

name(RR) -> RR#rr.name.
type(RR) -> RR#rr.type.
class(RR) -> RR#rr.class.
ttl(RR) -> RR#rr.ttl.
rdata(RR) -> RR#rr.rdata.

% example.com 2915 IN  A 1.1.1.1
from_string(Str) -> 
    [NameStr, TTL, CLASS, Type|Rdata] = string:tokens(Str, "."),
    UnifyType = string:to_upper(Type),
    {_Rdlen, RdataStruct} = r9_rdata_struct:from_string(UnifyType, Rdata),
    #rr{name = r9_wire_name:from_string(NameStr), 
        type = UnifyType,
        class = string:to_upper(CLASS),
        ttl = string:to_integer(TTL),
        rdata = RdataStruct}.


% 0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
% +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
% /                                               /
% /                      NAME                     /
% +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
% |                      TYPE                     |
% +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
% |                     CLASS                     |
% +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
% |                      TTL                      |
% |                                               |
% +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
% |                   RDLENGTH                    |
% +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--|
% /                     RDATA                     /
% +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

from_wire(WholeMessage, CurrentPos) ->
    {Name, NextPos} = r9_wire_name:from_wire(WholeMessage, CurrentPos),
    <<_ParsedData:NextPos/bytes, Type:16/big, CLASS:16/big, TTL:32/big, Rdlen:16/big, _/bits>> = WholeMessage,
    RdataStartPos = NextPos + 2 + 2 + 4 + 2,
    {#rr{name = Name, 
            type = Type,
            class = CLASS,
            ttl = TTL,
            rdata = r9_rdata_struct:from_wire(Type, WholeMessage, RdataStartPos)},
     RdataStartPos + Rdlen}. 

to_wire(RR) ->
    WireNameBinary = r9_wire_name:to_wire(name(RR)),
    Type = type(RR),
    Class = class(RR),
    TTL = ttl(RR),
    {Rdlen, RdataBinary} = r9_rdata_struct:to_wire(rdata(RR)),
    <<WireNameBinary/binary, Type:16/big, Class:16/big, TTL:32/big, Rdlen:16/big, RdataBinary/binary>>.

rtype_to_string(?TYPE_A) -> "A";
rtype_to_string(?TYPE_NS) -> "NS";
rtype_to_string(?TYPE_CNAME) -> "CNAME";
rtype_to_string(?TYPE_SOA) -> "SOA";
rtype_to_string(?TYPE_PTR) -> "PTR";
rtype_to_string(?TYPE_AAAA) -> "AAAA";
rtype_to_string(?TYPE_OPT) -> "OPT";
rtype_to_string(_) ->  "unknown type".

to_string(RR) ->
    r9_wire_name:to_string(name(RR)) ++ " " ++ rtype_to_string(type(RR)) ++ " " ++ integer_to_list(ttl(RR)) ++ " " ++ r9_rdata_struct:to_string(rdata(RR)).

belongs_to_rrset(RR, RRset) ->
    r9_wire_name:is_equal(r9_rrset:name(RRset), name(RR)) and
    (r9_rrset:type(RRset) =:= type(RR)). 


%test
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

from_wire_test() ->
    WireData1 = <<16#85,16#9f,16#80,16#80,16#0,16#1,16#0,16#1,16#0,16#0,16#0,16#0,16#4,16#6b,16#6e,16#65,16#74,16#2,16#63,16#6e,16#0,16#0,16#1,16#0,16#1,16#c0,16#c,16#0,16#1,16#0,16#1,16#0,16#0,16#1,16#56,16#0,16#4,16#ca,16#ad,16#b,16#a>>,
    {RR_A, _} = from_wire(WireData1, 25),
    ?assertEqual(r9_wire_name:to_string(name(RR_A)), "knet.cn."),
    ?assertEqual(type(RR_A), ?TYPE_A),
    ?assertEqual(class(RR_A), ?CLASS_IN),
    ?assertEqual(ttl(RR_A), 342),
    #a{ip = IP} = rdata(RR_A), 
    ?assertEqual(r9_util:ipv4_to_string(IP), "202.173.11.10"),
    io:format("--> towire ~p ~n", [to_wire(RR_A)]), 
    io:format("--> expect ~p ~n", [<<4, 107, 110, 101, 116, 2, 99, 110, 0, 0, 1, 0, 1, 0, 0, 1, 86, 0, 4, 202, 173, 11, 10>>]), 
    ?assertEqual(to_wire(RR_A), <<4, 107, 110, 101, 116, 2, 99, 110, 0, 0, 1, 0, 1, 0, 0, 1, 86, 0, 4, 202, 173, 11, 10>>),


    {RR_SOA, NextPosToParse} = from_wire(<<16#2c,16#4e,16#80,16#80,16#0,16#1,16#0,16#1,16#0,16#0,16#0,16#0,16#2,16#63,16#6e,16#0,16#0,16#6,16#0,16#1,16#c0,16#c,16#0,16#6,16#0,16#1,16#0,16#0,16#2b,16#2e,16#0,16#29,16#1,16#61,16#3,16#64,16#6e,16#73,16#c0,16#c,16#4,16#72,16#6f,16#6f,16#74,16#5,16#63,16#6e,16#6e,16#69,16#63,16#c0,16#c,16#78,16#6,16#99,16#a,16#0,16#0,16#1c,16#20,16#0,16#0,16#e,16#10,16#0,16#24,16#ea,16#0,16#0,16#0,16#54,16#60>>, 20),
    ?assertEqual(r9_wire_name:to_string(name(RR_SOA)), "cn."),
    ?assertEqual(type(RR_SOA), ?TYPE_SOA),
    ?assertEqual(class(RR_SOA), ?CLASS_IN),
    ?assertEqual(ttl(RR_SOA), 11054),
    #soa{mname= MName, 
        rname = RName, 
        serial = Serial, 
        refresh = Refresh, 
        retry = Retry,
        expire = Expire,
        minimum = Minimum} = rdata(RR_SOA), 
    ?assertEqual(r9_wire_name:to_string(MName), "a.dns.cn."),
    ?assertEqual(r9_wire_name:to_string(RName), "root.cnnic.cn."),
    ?assertEqual(Serial, 2013698314),
    ?assertEqual(Refresh, 7200),
    ?assertEqual(Retry, 3600),
    ?assertEqual(Expire, 2419200),
    ?assertEqual(Minimum, 21600),
    ?assertEqual(NextPosToParse, 73).

-endif.








