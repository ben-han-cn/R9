-module (r9_rrset_cache).

-export ([create/0,
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

-include("r9_dns.hrl").
-define(MAX_TTL, 864000).


create() ->
    ets:new(rrset_table, [{keypos, #rrset_entry.rrset_id},
                          {read_concurrency, true},
                          {write_concurrency, true}]).

rrset_id(RRset) ->
    rrset_id(r9_rrset:name(RRset), r9_rrset:type(RRset)).
rrset_id(Name, Type) ->
    r9_message_question:id(Name, Type).

%% return MinTTL of inserted RRsets, and ID of each RRset
%% {MinTTL, IDs}
insert_rrset(Cache, RRset) ->
    RRsetID = rrset_id(RRset),
    ets:insert(Cache, #rrset_entry{rrset_id = RRsetID,
                                   rrset = RRset,
                                   expiration = r9_rrset:ttl(RRset) + r9_util:local_now()}),
    {r9_rrset:ttl(RRset), RRsetID}.

insert_rrsets(Cache, RRsets) ->
    lists:foldl(fun(RRset, {MinTTL, IDs}) ->
                    RRsetID = rrset_id(RRset),
                    ets:insert(Cache, #rrset_entry{rrset_id = RRsetID,
                                                   rrset = RRset,
                                                   expiration = r9_rrset:ttl(RRset) + r9_util:local_now()}),
                    {case MinTTL > r9_rrset:ttl(RRset) of
                            true -> r9_rrset:ttl(RRset);
                            false -> MinTTL
                    end,
                    [RRsetID | IDs]}
            end, {?MAX_TTL, []}, RRsets).

find_rrset(Cache, Name, Type) ->
    find_rrset(Cache, r9_message_question:id(Name, Type)).

find_rrset(Cache, RRsetID) ->
    Now = r9_util:local_now(),
    case ets:lookup(Cache, RRsetID) of
        [] -> {not_found};
        [#rrset_entry{rrset = RRset, expiration = ExpireTime}] -> 
            if 
                Now > ExpireTime ->
                    ets:delete(Cache, RRsetID),
                    {not_found};
                true -> {ok, RRset#rrset{ttl = ExpireTime - Now}}
            end
    end.

delete_rrset(Cache, Name, Type) ->
    ets:delete(Cache, rrset_id(Name, Type)).
delete_rrset(Cache, RRsetID) ->
    ets:delete(Cache, RRsetID).
destroy(Cache) ->
    ets:delete(Cache).
