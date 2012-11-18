-module(r9_message_section).

-export([from_wire/3,
         rrs/1,
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

rrs(Section) -> Section#section.rrs.
rr_count(Section) -> Section#section.rr_count.


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
