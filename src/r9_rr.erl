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

-record(a, {ip}).
%rdata API
rdata_from_string("A", IP) ->
    {4, #a{ip = r9_util:ipv4_from_string(IP)}};
rdata_from_string(_, _) ->
    throw("unknow type").


rdata_from_wire(?TYPE_A, _WholeMessage, IP) ->
    #a{ip = r9_util:ipv4_from_wire(IP)};
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
    <<Type:16/big, CLASS:16/big, TTL:32/big, Rdlen:16/big, Rdata:Rdlen/binary-unit:8, _/bits>> = r9_util:binary_rest(WholeMessage, NextPos),
    {#rr{name = Name, 
         type = Type,
         class = CLASS,
         ttl = TTL,
         rdlen = Rdlen,
         rdata = rdata_from_wire(Type, WholeMessage, Rdata)}, NextPos + 2 + 2 + 4 + 2 + Rdlen}. 



%test
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

from_wire_test() ->
    {RR, _} = from_wire(<<16#85,16#9f,16#80,16#80,16#0,16#1,16#0,16#1,16#0,16#0,16#0,16#0,16#4,16#6b,16#6e,16#65,16#74,16#2,16#63,16#6e,16#0,16#0,16#1,16#0,16#1,16#c0,16#c,16#0,16#1,16#0,16#1,16#0,16#0,16#1,16#56,16#0,16#4,16#ca,16#ad,16#b,16#a>>, 25),
    ?assertEqual(r9_wire_name:to_string(name(RR)), "knet.cn."),
    ?assertEqual(type(RR), ?TYPE_A),
    ?assertEqual(class(RR), ?CLASS_IN),
    ?assertEqual(ttl(RR), 342),
    #a{ip = IP} = rdata(RR), 
    ?assertEqual(r9_util:ipv4_to_string(IP), "202.173.11.10").
-endif.






 

