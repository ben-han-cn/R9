-module(r9_util).

-export([ipv4_from_wire/1,
         ipv6_from_wire/1,
         ipv4_from_string/1,
         ipv4_to_string/1
        ]).

-define(TEST, true).

ipv4_from_wire(WireData) ->
    <<IP:32/big>> = WireData,
    IP.

ipv6_from_wire(WireData) ->
    <<IP:128/big>> = WireData,
    IP.

ipv4_from_string(Str) ->
    [Label1, Label2, Label3, Label4] = lists:map(fun(Label) -> 
                                                    element(1, string:to_integer(Label)) 
                                                 end, 
                                                 string:tokens(Str, ".")),
    <<IP:32/big>> = <<Label1:8, Label2:8, Label3:8, Label4:8>>,
    IP.
    

ipv4_to_string(IP) ->
    <<Label1:8, Label2:8, Label3:8, Label4:8>> = <<IP:32/big>>,
    string:join(lists:map(fun(Label)-> integer_to_list(Label) end, [Label1, Label2, Label3, Label4]), ".").


%% Tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ipv4_test() ->
    ?assertEqual(ipv4_to_string(ipv4_from_string("2.2.2.2")), "2.2.2.2"),
    IP = ipv4_from_wire(<<16#ca,16#ad,16#b,16#a>>),
    ?assertEqual(ipv4_to_string(IP), "202.173.11.10").

-endif.
