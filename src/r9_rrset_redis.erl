-module (r9_rrset_redis).

-export ([create/0,
          create/1,
          rrset_id/1,
          rrset_id/2,
          insert_rrset/2,
          insert_rrsets/2,
          find_rrset/3,
          find_rrset/2,
          delete_rrset/2,
          delete_rrset/3,
          destroy/1]).

-record(rrset_entry,{ rrset,
            expiration}).

-record(rrset_cache, {check_expire,
        redis_link}).

-include("r9_dns.hrl").
-define(MAX_TTL, 864000).


create()->
    create([{check_expire, true}]).
create([{check_expire, CheckExpire}]) ->
    {ok, C} = eredis:start_link(),
    #rrset_cache{check_expire = CheckExpire, redis_link = C}.

rrset_id(RRset) ->
    rrset_id(r9_rrset:name(RRset), r9_rrset:type(RRset)).
rrset_id(Name, Type) ->
    lists:concat(["R+", r9_message_question:id(Name, Type)]).

%% return MinTTL of inserted RRsets, and ID of each RRset
%% {MinTTL, IDs}
insert_rrset(#rrset_cache{redis_link = C, 
                          check_expire = CheckExpire}, RRset) ->
    RRsetID = rrset_id(RRset),
    case CheckExpire of
        true ->
            RRsetEntry = #rrset_entry{rrset = RRset, expiration = r9_util:local_now() + r9_rrset:ttl(RRset)},
            eredis:q(C, ["SETEX", RRsetID, r9_rrset:ttl(RRset), RRsetEntry]);
        false ->
            eredis:q(C, ["SET", RRsetID, RRset])
    end,
    {r9_rrset:ttl(RRset), RRsetID}.

insert_rrsets(Cache, RRsets) ->
    lists:foldl(fun(RRset, {MinTTL, IDs}) ->
                    RRsetID = rrset_id(RRset),
                    {TTL, _} = insert_rrset(Cache, RRset),
                    {min(MinTTL, TTL), [RRsetID | IDs]}
            end, {?MAX_TTL, []}, RRsets).

find_rrset(RRsetCache, Name, Type) ->
    find_rrset(RRsetCache, rrset_id(Name, Type)).

find_rrset(#rrset_cache{redis_link = C, check_expire = CheckExpire}, RRsetID) ->
    case eredis:q(C, ["GET", RRsetID]) of
        {ok, undefined} -> {not_found};
        {ok, RRsetEntryBinary} -> 
            if 
                CheckExpire -> 
                    #rrset_entry{rrset = RRset, expiration = ExpireTime} = binary_to_term(RRsetEntryBinary),
                    {ok, RRset#rrset{ttl = ExpireTime - r9_util:local_now()}};
                true ->
                    {ok, binary_to_term(RRsetEntryBinary)}
            end
    end.

delete_rrset(RRsetCache, Name, Type) ->
    eredis:q(RRsetCache#rrset_cache.redis_link, ["DEL", rrset_id(Name, Type)]).
delete_rrset(RRsetCache, RRsetID) ->
    eredis:q(RRsetCache#rrset_cache.redis_link, ["DEL", RRsetID]).

destroy(RRsetCache) ->
    eredis:stop(RRsetCache#rrset_cache.redis_link).
