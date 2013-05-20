-module(r9_message_question).

-export([from_wire/2,
        to_wire/1,
        to_string/1,
        name/1,
        type/1,
        class/1,
        id/1,
        id/2]).

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

to_wire(Question) ->
    WireNameBinary = r9_wire_name:to_wire(name(Question)),
    Type = type(Question),
    Class = class(Question),
    <<WireNameBinary/binary, Type:16/big, Class:16/big>>.

id(Question) ->
    id(name(Question), type(Question)).

id(Name, Type) ->
    lists:concat([r9_wire_name:to_string(Name), 
                  "/", 
                  r9_rr:rtype_to_string(Type)]).


to_string(Question) ->
    string:join([r9_wire_name:to_string(name(Question)), 
                 r9_rr:rtype_to_string(type(Question)), 
                 r9_rr:class_to_string(class(Question))], " "). 
 


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
dig_message_header_test()->
    TwoShortData = <<1>>,
    ?assertException(error, function_clause, from_wire(TwoShortData, 0)).
-endif.
