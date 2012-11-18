-module(r9_rr).
-export([from_wire/2,
         from_string/1,
         name/1,
         type/1,
         class/1,
         ttl/1,
         rdata/1]).

-include("r9_dns.hrl").
-record(rr, {name,   
             type,
             class,
             ttl = 0,
             rdlen, 
             rdata}). 


%rdata API
rdata_from_string("A", IP) ->
    {4, #a{ip = r9_util:ipv4_from_string(IP)}};
rdata_from_string(_, _) ->
    throw("unknow type").

% A
rdata_from_wire(?TYPE_A, WholeMessage, StartPos) ->
    <<_ParsedData:StartPos/bytes, IP:32/bits, _/bits>> = WholeMessage,
    #a{ip = r9_util:ipv4_from_wire(IP)};

% NS
rdata_from_wire(?TYPE_NS, WholeMessage, StartPos) ->
    #domain{domain= element(1, r9_wire_name:from_wire(WholeMessage, StartPos))};

% CNAME
rdata_from_wire(?TYPE_CNAME, WholeMessage, StartPos) ->
    #domain{domain= element(1, r9_wire_name:from_wire(WholeMessage, StartPos))};

%SOA
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%/                     MNAME                     /
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%/                     RNAME                     /
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%|                    SERIAL                     |
%|                                               |
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%|                    REFRESH                    |
%|                                               |
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%|                     RETRY                     |
%|                                               |
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%|                    EXPIRE                     |
%|                                               |
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%|                    MINIMUM                    |
%|                                               |
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
rdata_from_wire(?TYPE_SOA, WholeMessage, StartPos) ->
    {MName, NextPos} = r9_wire_name:from_wire(WholeMessage, StartPos),
    {RName, NNextPos} = r9_wire_name:from_wire(WholeMessage, NextPos),
    <<_ParsedData:NNextPos/bytes, Serial:32/big, Refresh:32/big, Retry:32/big, Expire:32/big, Minimum:32/big, _/bits>> = WholeMessage,
    #soa{mname = MName, 
         rname = RName, 
         serial = Serial,
         refresh = Refresh, 
         retry = Retry,
         expire = Expire,
         minimum = Minimum};
 
% PTR
rdata_from_wire(?TYPE_PTR, WholeMessage, StartPos) ->
    #domain{domain= element(1, r9_wire_name:from_wire(WholeMessage, StartPos))};

% MX
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%|                  PREFERENCE                   |
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
%/                   EXCHANGE                    /
%+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
rdata_from_wire(?TYPE_MX, WholeMessage, StartPos) ->
    <<_ParsedData:StartPos/bytes, Preference:16/big, _/bits>> = WholeMessage,
    #mx{preference = Preference, exchange = r9_wire_name:from_wire(WholeMessage, StartPos + 2)};

rdata_from_wire(?TYPE_AAAA, WholeMessage, StartPos) ->
    <<_ParsedData:StartPos/bytes, IP:128/bits, _/bits>> = WholeMessage,
    #aaaa{ip = r9_util:ipv6_from_wire(IP)};

rdata_from_wire(_, _, _) ->
    throw("unknow type").

%rdata_to_string("A", #a{ip = IP}) ->
%    r9_util:ipv4_to_string(IP);
%rdata_to_string(_, _) ->
%    throw("unknow type").


 name(RR) -> RR#rr.name.
 type(RR) -> RR#rr.type.
 class(RR) -> RR#rr.class.
 ttl(RR) -> RR#rr.ttl.
 rdata(RR) -> RR#rr.rdata.

 % example.com 2915 IN  A 1.1.1.1
 from_string(Str) -> 
     [NameStr, TTL, CLASS, Type|Rdata] = string:tokens(Str, "."),
     UnifyType = string:to_upper(Type),
     {Rdlen, RdataStruct} = rdata_from_string(UnifyType, Rdata),
     #rr{name = r9_wire_name:from_string(NameStr), 
         type = UnifyType,
         class = string:to_upper(CLASS),
         ttl = string:to_integer(TTL),
         rdlen = Rdlen,
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
         rdlen = Rdlen,
         rdata = rdata_from_wire(Type, WholeMessage, RdataStartPos)}, RdataStartPos + Rdlen}. 



%test
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

from_wire_test() ->
    {RR_A, _} = from_wire(<<16#85,16#9f,16#80,16#80,16#0,16#1,16#0,16#1,16#0,16#0,16#0,16#0,16#4,16#6b,16#6e,16#65,16#74,16#2,16#63,16#6e,16#0,16#0,16#1,16#0,16#1,16#c0,16#c,16#0,16#1,16#0,16#1,16#0,16#0,16#1,16#56,16#0,16#4,16#ca,16#ad,16#b,16#a>>, 25),
    ?assertEqual(r9_wire_name:to_string(name(RR_A)), "knet.cn."),
    ?assertEqual(type(RR_A), ?TYPE_A),
    ?assertEqual(class(RR_A), ?CLASS_IN),
    ?assertEqual(ttl(RR_A), 342),
    #a{ip = IP} = rdata(RR_A), 
    ?assertEqual(r9_util:ipv4_to_string(IP), "202.173.11.10"),


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






 

