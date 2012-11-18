-module(r9_message_question).

-export([from_wire/2,
        name/1,
        type/1,
        class/1
        ]).

-include("r9_dns.hrl").

-record(question, {name,
                   type,
                   class}).

name(Question) -> Question#question.name.
type(Question) -> Question#question.type.
class(Question) -> Question#question.class.

from_wire(WireData, CurrentPos) ->
    {Name, NextPos} = r9_wire_name:from_wire(WireData, CurrentPos),
    <<_ParsedData:NextPos/bytes, Type:16/big, CLASS:16/big, _/bits>> = WireData,
    {#question{name = Name, 
              type = Type,
              class = CLASS}, NextPos + 4}.



%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
dig_message_header_test()->
    TwoShortData = <<1>>,
    ?assertException(error, function_clause, from_wire(TwoShortData, 0)).
-endif.