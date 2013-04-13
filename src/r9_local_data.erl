-module(r9_local_data).
-export([load/1,
        destroy/1,
        find_rrset/2,
        find_rrset/3]).

-include("r9_dns.hrl").

-record(local_data, {rrset_cache}).


load(FileName) ->
    Lines = r9_util:read_lines(FileName),
    RRsetCache = r9_rrset_cache:create(),
    parse_rr(RRsetCache, Lines),
    #local_data{rrset_cache = RRsetCache}.

destroy(#local_data{rrset_cache = RRsetCache}) ->
    ets:delete(RRsetCache).

parse_rr(RRsetCache, []) -> RRsetCache;
parse_rr(RRsetCache, [Line|Rest]) ->
    RR = r9_rr:from_string(Line),
    parse_rr(RRsetCache, Rest, r9_rrset:from_rr(RR)).
parse_rr(RRsetCache, [], LastRRset) ->
    r9_rrset_cache:insert_rrset(RRsetCache, LastRRset),
    RRsetCache;
parse_rr(RRsetCache, [Line|Rest], LastRRset) ->
    RR = r9_rr:from_string(Line),
    case r9_rr:belongs_to_rrset(RR, LastRRset) of
        true  -> parse_rr(RRsetCache, Rest, r9_rrset:add_rr(LastRRset, RR));
        false -> r9_rrset_cache:insert_rrset(RRsetCache, LastRRset),
                 parse_rr(RRsetCache, Rest, r9_rrset:from_rr(RR))
    end.


find_rrset(#local_data{rrset_cache = RRsetCache}, Name, Type) ->
    r9_rrset_cache:find_rrset(RRsetCache, Name, Type).


find_rrset(#local_data{rrset_cache = RRsetCache}, RRsetID) ->
    r9_rrset_cache:find_rrset(RRsetCache, RRsetID).
