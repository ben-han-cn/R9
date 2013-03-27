-module(r9_message_section).

-export([from_wire/3,
         to_wire/1,
         rrs/1,
         rrsets/1,
         from_rrsets/1,
         rr_count/1]).

-record(section, {rr_count,
                  rrs}).

rrs_from_wire(_WireData, CurrentPos, RRs, 0) -> {lists:reverse(RRs), CurrentPos};
rrs_from_wire(WireData, CurrentPos, RRs, RRCount) -> 
    {RR, NextPos} = r9_rr:from_wire(WireData, CurrentPos),
    rrs_from_wire(WireData, NextPos, [RR | RRs], RRCount - 1).


from_wire(_WireData, CurrentPos, 0) ->
    {#section{rr_count = 0, rrs = []}, CurrentPos};
from_wire(WireData, CurrentPos, RRCount) ->
    {RRs, NextPos} = rrs_from_wire(WireData, CurrentPos, [], RRCount),
    {#section{rr_count = RRCount, rrs = RRs}, NextPos}.

to_wire(Section) ->
    list_to_binary(lists:map(fun(RRset) ->
                                r9_rrset:to_wire(RRset)
                        end, rrsets(Section))).

rrs(Section) -> Section#section.rrs.
rr_count(Section) -> Section#section.rr_count.
rrsets(#section{rr_count = RRCount}) when RRCount =:= 0 -> 
    [];
rrsets(Section) -> 
    {LastRRset, NewRRsets} = lists:foldl(fun(RR, {CurrentRRset, RRsets}) ->
                if  
                    CurrentRRset =:= nil -> 
                        {r9_rrset:from_rr(RR), []};
                    true ->
                        case r9_rr:belongs_to_rrset(RR, CurrentRRset) of
                            true ->
                                {r9_rrset:add_rr(CurrentRRset, RR), RRsets};
                            false ->
                                {r9_rrset:from_rr(RR), [CurrentRRset | RRsets]}
                        end
                end
        end, {nil, []}, rrs(Section)),
    [LastRRset | NewRRsets].

from_rrsets(RRsets) ->
    RRs = lists:foldl(fun(RRset, RRs) ->
                lists:append(r9_rrset:rrs(RRset), RRs)
        end, [], RRsets),
    #section{rr_count = length(RRs), rrs = RRs}.
%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
