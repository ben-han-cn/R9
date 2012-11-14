-module(r9_wire_name).
-export([from_wire/1,
         from_string/1,
         len/1,
         label_count/1,
         compare/2,
         is_equal/2,
         contains/2,
         to_string/1,
         substract/2,
         concat/2,
         parent/1]).

-define(TEST, true).

-record(label, {len,   %len of str
                str}). % "abc"

-record(wire_name, {len,      %len len(a.example.com) == 14 including the ending zero
                    label_count, %count of lables
                    labels}). %labels in left to right order, so for example.com
                              %[{8, "example"}, {4, "com"}, {1, ""}]

%% For wire name, it's a little confused that, the ending zero stands for one label,
%% so for "a.b.com", there are 4 labels, 
-define(EMPTY_LABEL, #label{len = 1, str = ""}).

labels_from_wire(WireData, Labels) ->
    <<Len:8/integer, _Left/bits>> = WireData,
    case Len of 
        0 -> lists:reverse([?EMPTY_LABEL | Labels]);
        _ -> {<<_Len:8/integer, LabelBinary/bits>>, <<RestLabels/bits>>} = split_binary(WireData, Len + 1),
            labels_from_wire(RestLabels, [#label{len = Len + 1, str = string:to_lower(binary_to_list(LabelBinary))} | Labels])
    end.

label_from_string(Str) ->
    #label{len = length(Str) + 1,
           str = Str}.

labels_from_string(Str) ->
    if
        Str == "." -> [?EMPTY_LABEL]; 
        true -> lists:append(lists:map(fun(LabelStr) -> label_from_string(LabelStr) end, string:tokens(Str, ".")),
                             [?EMPTY_LABEL])
    end.

label_to_string(#label{str = Str} = _Label) ->
    Str.

from_lables(Labels) ->
    #wire_name{len = lists:foldl(fun(#label{len = Len} = _Label, Sum) -> Sum + Len end, 0, Labels), 
               label_count = length(Labels),
               labels = Labels}.

from_wire(WireData) when is_bitstring(WireData) ->
    from_lables(labels_from_wire(WireData, [])).

from_string(Str) ->
    from_lables(labels_from_string(Str)).

to_string(#wire_name{labels = Labels} = _Name) ->
    string:join(lists:map(fun(Label) -> label_to_string(Label) end, Labels), ".").
        
len(#wire_name{len = Len} = _Name) ->
    Len.

label_count(#wire_name{label_count = Count} = _Name) ->
    Count.

labels_is_equal([], []) -> true;
labels_is_equal(Labels1, Labels2) ->
    {[Label1 | Labels1Left], [Label2 | Labels2Left]} = {Labels1, Labels2},
    (Label1#label.len == Label2#label.len) and (Label1#label.str == Label2#label.str) and labels_is_equal(Labels1Left, Labels2Left).
    
is_equal(Name1, Name2) when is_record(Name1, wire_name) and is_record(Name2, wire_name) ->
    (len(Name1) == len(Name2)) and 
    (label_count(Name1) == label_count(Name2)) and 
    labels_is_equal(Name1#wire_name.labels, Name2#wire_name.labels);
is_equal(_, _) -> false.

contains(Longer, Shorter) when is_record(Longer, wire_name) and is_record(Shorter, wire_name) ->
    (len(Longer) >= len(Shorter)) and 
    (label_count(Longer) >= label_count(Shorter)) and 
    labels_is_equal(lists:sublist(Longer#wire_name.labels, label_count(Longer) - label_count(Shorter) + 1, label_count(Shorter)),
                    Shorter#wire_name.labels).

compare_label(Label1, Label2) ->
    Str1 = Label1#label.str,
    Str2 = Label2#label.str,
    if 
        Str1 > Str2 -> 1;
        Str1 < Str2 -> -1;
        true -> 0
    end.


compare_labels(Labels1, Labels2) ->
    LabelCount1 = length(Labels1),
    LabelCount2 = length(Labels2),
    if 
        (LabelCount1 == 0) and (LabelCount2 > 0) -> -1;
        (LabelCount1 > 0) and (LabelCount2 == 0) -> 1;
        (LabelCount1 == 0) and (LabelCount2 == 0) -> 0;
        true -> {[FirstLabel1 | Labels1Left], [FirstLabel2 | Labels2Left] } = {Labels1, Labels2},
            case compare_label(FirstLabel1, FirstLabel2) of
                1 -> 1;
                -1 -> -1;
                0 -> compare_labels(Labels1Left, Labels2Left)
            end
    end.

compare(Name1, Name2) ->
    compare_labels(lists:reverse(Name1#wire_name.labels), lists:reverse(Name2#wire_name.labels)).


substract(Longer, Shorter) when is_record(Longer, wire_name) and is_record(Shorter, wire_name) ->
    left_label_count = label_count(Longer) - label_count(Shorter),
    #wire_name{len = len(Longer) - len(Shorter),
               label_count = left_label_count,
               labels = lists:sublist(Longer#wire_name.labels, left_label_count)}.

concat(Prefix, Suffix) when is_record(Prefix, wire_name) and is_record(Suffix, wire_name) ->
    #wire_name{len = len(Prefix) + len(Suffix),
               label_count = label_count(Prefix) + label_count(Suffix),
               labels = lists:append(Prefix#wire_name.labels, Suffix#wire_name.labels)}.

parent(Name) ->
    case label_count(Name) of
        1 -> null;
        _ -> [FirstLabel | LeftLabels] = Name#wire_name.labels,
             #wire_name{len = Name#wire_name.len - FirstLabel#label.len,
                        label_count = Name#wire_name.label_count - 1,
                        labels = LeftLabels}
    end.


%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
from_wire_test()->
    Com = <<3, $c, $o, $m, 0>>,
    Name = from_wire(Com),
    ?assertEqual(len(Name), 5),
    ?assertEqual(label_count(Name), 2).

from_to_string_test()->
    ACom= <<1, $A, 3, $c, $O, $m, 0 >>,
    Name = from_wire(ACom),
    Str = "a.com.",
    ?assertEqual(to_string(Name), Str),
    NewName = from_string(Str),
    ?assert(is_equal(Name, NewName)),
    Root = from_string("."),
    ?assertEqual(parent(Root), null).

is_equal_test() ->
    ACom= <<1, $A, 3, $c, $O, $m, 0 >>,
    Name1 = from_wire(ACom),
    ACom2 = <<1, $A, 3, $c, $O, $m, 0 >>,
    Name2 = from_wire(ACom2),
    ?assert(is_equal(Name1, Name2)),
    ACom3 = <<1, $A, 3, $C, $O, $m, 0 >>,
    Name3 = from_wire(ACom3),
    ?assert(is_equal(Name1, Name3)),
    ACom4 = <<1, $b, 3, $C, $O, $m, 0 >>,
    Name4 = from_wire(ACom4),
    ?assertNot(is_equal(Name1, Name4)).

parent_test() ->
    ACom= <<1, $A, 3, $c, $O, $m, 0 >>,
    Name = from_wire(ACom),
    Com = parent(Name),
    ?assertEqual(len(Com), 5),
    ?assertEqual(label_count(Com), 2),
    Root = parent(Com),
    ?assertEqual(len(Root), 1),
    ?assertEqual(label_count(Root), 1),
    Empty = parent(Root),
    ?assertEqual(Empty, null).

-endif.


