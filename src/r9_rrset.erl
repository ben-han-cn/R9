-module(r9_rrset).
-export([to_string/1,
        to_wire/1,
        rrs/1,
        name/1,
        type/1,
        class/1,
        ttl/1,
        from_rr/1,
        add_rr/2,
        rr_count/1,
        rdatas/1]).

-include("r9_dns.hrl").

from_rr(RR) ->
    #rrset{name = RR#rr.name,
           type = RR#rr.type,
           class = RR#rr.class,
           ttl =  RR#rr.ttl,
           rdatas = [r9_rr:rdata(RR)]}.

add_rr(RRset, RR) ->
    RRset#rrset{rdatas = [r9_rr:rdata(RR) | RRset#rrset.rdatas]}.

rr_count(RRset) ->
    length(RRset#rrset.rdatas).

rrs(RRset) ->
    lists:map(fun(Rdata) ->
                #rr{name=RRset#rrset.name,
                    type=RRset#rrset.type,
                    class=RRset#rrset.class,
                    ttl=RRset#rrset.ttl,
                    rdata=Rdata}  
        end,
        RRset#rrset.rdatas).

to_wire(RRset) ->
    list_to_binary(lists:map(fun(RR) ->
                                r9_rr:to_wire(RR)
                             end, rrs(RRset))).

name(RRset) -> RRset#rrset.name.
type(RRset) -> RRset#rrset.type.
class(RRset) -> RRset#rrset.class.
ttl(RRset) -> RRset#rrset.ttl.
rdatas(RRset) -> RRset#rrset.rdatas.

to_string(RRset) ->
    lists:concat(lists:map(fun(RR) ->
                                r9_rr:to_string(RR) ++ "\n"
                        end, rrs(RRset))).
