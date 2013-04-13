-module(r9_util).

-export([ipv4_from_wire/1,
         ipv6_from_wire/1,
         ipv4_to_wire/1,
         ipv6_to_wire/1,
         ipv4_from_string/1,
         ipv4_to_string/1,
         ipv6_to_string/1,
         local_now/0,
         string_to_integer/1,
         read_lines/1
        ]).

-define(TEST, true).

ipv4_from_wire(WireData) ->
    <<IP:32/big>> = WireData,
    IP.

ipv4_to_wire(IP) ->
    <<IP:32/big>>.

ipv6_from_wire(WireData) ->
    <<IP:128/big>> = WireData,
    IP.

ipv6_to_wire(IP) ->
    <<IP:128/big>>.

ipv4_from_string(Str) ->
    [Label1, Label2, Label3, Label4] = lists:map(fun(Label) -> 
                                                    string_to_integer(Label) 
                                                 end,
                                                 string:tokens(Str, ".")),
    <<IP:32/big>> = <<Label1:8, Label2:8, Label3:8, Label4:8>>,
    IP.
    

ipv4_to_string(IP) ->
    <<Label1:8, Label2:8, Label3:8, Label4:8>> = <<IP:32/big>>,
    string:join(lists:map(fun(Label)-> integer_to_list(Label) end, [Label1, Label2, Label3, Label4]), ".").

ipv6_to_string(IP) ->
    <<Label1:16, Label2:16, Label3:16, Label4:16, Label5:16, Label6:16, Label7:16, Label8:16>> = <<IP:128/big>>,
    string:join(lists:map(fun(Label)-> integer_to_list(Label) end, [Label1, Label2, Label3, Label4, Label5, Label6, Label7, Label8 ]), ":").

string_to_integer(Str) ->
    {Num, []} = string:to_integer(Str),
    Num.

%%Time related
local_now() ->
    calendar:datetime_to_gregorian_seconds(calendar:local_time()).

%%io 
read_lines(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    Lines = read_line(File, []),
    file:close(File),
    lists:reverse(Lines).

read_line(File, Lines)  ->
    case io:get_line(File, "") of
        eof  -> Lines;
        Line ->  read_line(File, [string:strip(Line, right, $\n) | Lines])
    end.

%% Tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

ipv4_test() ->
    ?assertEqual(ipv4_to_string(ipv4_from_string("2.2.2.2")), "2.2.2.2"),
    IP = ipv4_from_wire(<<16#ca,16#ad,16#b,16#a>>),
    ?assertEqual(ipv4_to_string(IP), "202.173.11.10").

-endif.

