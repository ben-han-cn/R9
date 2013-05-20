-module (r9_rrset_cache).

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

-record(rrset_entry,{
            rrset_id,
            rrset,
            expiration}).

-record(rrset_cache, {check_expire,
        rrset_table}).

-include("r9_dns.hrl").
-define(MAX_TTL, 864000).


create()->
    create([{check_expire, true}]).
create([{check_expire, CheckExpire}]) ->
    #rrset_cache{check_expire = CheckExpire,
        rrset_table = ets:new(rrset_table, [{keypos, #rrset_entry.rrset_id}])}.

rrset_id(RRset) ->
    rrset_id(r9_rrset:name(RRset), r9_rrset:type(RRset)).
rrset_id(Name, Type) ->
    r9_message_question:id(Name, Type).

%% return MinTTL of inserted RRsets, and ID of each RRset
%% {MinTTL, IDs}
insert_rrset(#rrset_cache{rrset_table = RRsetTable, 
                          check_expire = CheckExpire}, RRset) ->
    RRsetID = rrset_id(RRset),
    ets:insert(RRsetTable, #rrset_entry{rrset_id = RRsetID,
                                   rrset = RRset,
                                   expiration = rrset_expire_time(CheckExpire, RRset)}),
    {r9_rrset:ttl(RRset), RRsetID}.

insert_rrsets(#rrset_cache{rrset_table = RRsetTable, 
                          check_expire = CheckExpire}, RRsets) ->
    lists:foldl(fun(RRset, {MinTTL, IDs}) ->
                    RRsetID = rrset_id(RRset),
                    ets:insert(RRsetTable, #rrset_entry{rrset_id = RRsetID,
                                                   rrset = RRset,
                                                   expiration = rrset_expire_time(CheckExpire, RRset)}),
                    {case MinTTL > r9_rrset:ttl(RRset) of
                            true -> r9_rrset:ttl(RRset);
                            false -> MinTTL
                    end,
                    [RRsetID | IDs]}
            end, {?MAX_TTL, []}, RRsets).

find_rrset(RRsetCache, Name, Type) ->
    find_rrset(RRsetCache, r9_message_question:id(Name, Type)).

find_rrset(#rrset_cache{rrset_table = RRsetTable, check_expire = CheckExpire}, RRsetID) ->
    Now = r9_util:local_now(),
    case ets:lookup(RRsetTable, RRsetID) of
        [] -> {not_found};
        [#rrset_entry{rrset = RRset, expiration = ExpireTime}] -> 
            if 
                (not CheckExpire) -> {ok, RRset};
                Now > ExpireTime ->
                    ets:delete(RRsetTable, RRsetID),
                    {not_found};
                true -> {ok, RRset#rrset{ttl = ExpireTime - Now}}
            end
    end.

delete_rrset(RRsetCache, Name, Type) ->
    ets:delete(RRsetCache#rrset_cache.rrset_table, rrset_id(Name, Type)).
delete_rrset(RRsetCache, RRsetID) ->
    ets:delete(RRsetCache#rrset_cache.rrset_table, RRsetID).
destroy(RRsetCache) ->
    ets:delete(RRsetCache#rrset_cache.rrset_table).

%%imple
rrset_expire_time(CheckExpire, RRset) ->
    if 
        CheckExpire -> r9_rrset:ttl(RRset) + r9_util:local_now();
        true -> 0
    end.

