-module(r9_wire_name).
-export([from_wire/1,
         from_string/1,
         to_string/1,
         to_wire/1,
         len/1,
         label_count/1,
         compare/2,
         is_equal/2,
         contains/2,
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
-define(MAX_LABEL_LEN, 63).
-define(MAX_NAME_LEN, 255).  


%label API
label_from_string(Str) ->
    #label{len = length(Str) + 1,
           str = string:to_lower(Str)}.

label_to_string(#label{str = Str}) ->
    Str.

label_from_wire(<<Len:8/integer, NameStr:Len/bytes, LeftData/bits>>) ->
    Label = if 
                Len == 0 -> ?EMPTY_LABEL;
                (Len < ?MAX_LABEL_LEN)  -> #label{len = Len + 1, 
                                                  str = string:to_lower(binary_to_list(NameStr))}
            end,
    {Label, LeftData}.

label_to_wire(#label{len = Len, str = Str}) ->
    list_to_binary([Len - 1, Str]).

label_is_equal(#label{len = Len1, str = Str1}, #label{len = Len2, str = Str2}) ->
    (Len1 == Len2) and (Str1 == Str2).

label_compare(#label{str = Str1}, #label{str = Str2})->
    if 
        Str1 > Str2 -> 1;
        Str1 < Str2 -> -1;
        true -> 0
    end.


labels_compare([], []) -> 0;
labels_compare([], _) -> -1;
labels_compare(_, []) -> 1;
labels_compare(Labels1, Labels2) ->
    {[FirstLabel1 | Labels1Left], [FirstLabel2 | Labels2Left] } = {Labels1, Labels2},
    case label_compare(FirstLabel1, FirstLabel2) of
        1 -> 1;
        -1 -> -1;
        0 -> labels_compare(Labels1Left, Labels2Left)
    end.


%% labels API
labels_from_wire_helper(WireData, Acc) ->
    {Label, LeftData}  = label_from_wire(WireData),
    case Label#label.len of
        1 -> {[Label | Acc], LeftData};
        _ -> labels_from_wire_helper(LeftData, [Label | Acc])
    end.

labels_from_wire(WireData) ->
    {Labels, LeftData} = labels_from_wire_helper(WireData, []),
    {lists:reverse(Labels), LeftData}.


labels_from_string(Str) ->
    if
        Str == "." -> [?EMPTY_LABEL]; 
        true -> lists:append(lists:map(fun(LabelStr) -> label_from_string(LabelStr) end, string:tokens(Str, ".")),
                             [?EMPTY_LABEL])
    end.

labels_is_equal([], []) -> true;
labels_is_equal(Labels1, Labels2) ->
    io:format("<<<<< ~p ~p >> ~n", [Labels1, Labels2]),
    {[Label1 | Labels1Left], [Label2 | Labels2Left]} = {Labels1, Labels2},
    label_is_equal(Label1, Label2) and labels_is_equal(Labels1Left, Labels2Left).
    
labels_to_string(Labels) ->
    string:join(lists:map(fun(Label) -> label_to_string(Label) end, Labels), ".").

labels_to_wire(Labels) ->
    list_to_binary(lists:map(fun(Label) -> label_to_wire(Label) end, Labels)).

%%wirename API
from_lables(Labels) ->
    #wire_name{len = lists:foldl(fun(#label{len = Len} = _Label, Sum) -> Sum + Len end, 0, Labels), 
               label_count = length(Labels),
               labels = Labels}.

from_wire(WireData) ->
    {Labels, LeftData} = labels_from_wire(WireData),
    {from_lables(Labels), LeftData}.

to_wire(#wire_name{labels = Labels}) ->
    labels_to_wire(Labels).

from_string(Str) ->
    from_lables(labels_from_string(Str)).

to_string(#wire_name{labels = Labels}) ->
    labels_to_string(Labels).
        
len(#wire_name{len = Len} = _Name) ->
    Len.

label_count(#wire_name{label_count = Count}) ->
    Count.

is_equal(Name1, Name2) when is_record(Name1, wire_name) and is_record(Name2, wire_name) ->
    (len(Name1) == len(Name2)) and 
    (label_count(Name1) == label_count(Name2)) and 
    labels_is_equal(Name1#wire_name.labels, Name2#wire_name.labels);
is_equal(_, _) -> false.

contains(Longer, Shorter) when is_record(Longer, wire_name) and is_record(Shorter, wire_name) ->
    (len(Longer) >= len(Shorter)) and 
    (label_count(Longer) >= label_count(Shorter)) and 
    labels_is_equal(lists:sublist(Longer#wire_name.labels, 
                                  label_count(Longer) - label_count(Shorter) + 1, 
                                  label_count(Shorter)),
                    Shorter#wire_name.labels).

compare(Name1, Name2) ->
    labels_compare(lists:reverse(Name1#wire_name.labels), lists:reverse(Name2#wire_name.labels)).


substract(Longer, Shorter) ->
    Left_label_count = label_count(Longer) - label_count(Shorter),
    #wire_name{len = len(Longer) - len(Shorter) + 1,
               label_count = Left_label_count + 1,
               labels = lists:append(lists:sublist(Longer#wire_name.labels, Left_label_count), 
                                    [?EMPTY_LABEL])}.

concat(Prefix, Suffix) ->
    #wire_name{len = len(Prefix) + len(Suffix) - 1,
               label_count = label_count(Prefix) + label_count(Suffix) - 1,
               labels = lists:append(lists:sublist(Prefix#wire_name.labels, label_count(Prefix) - 1), 
                                     Suffix#wire_name.labels)}.

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

from_wire_discard_left(WireData) ->
    element(1, from_wire(WireData)).

from_wire_test()->
    Com = <<3, $c, $o, $m, 0>>,
    Name = from_wire_discard_left(Com),
    ?assertEqual(len(Name), 5),
    ?assertEqual(label_count(Name), 2).

from_to_string_test()->
    ACom= <<1, $A, 3, $c, $O, $m, 0 >>,
    Name = from_wire_discard_left(ACom),
    Str = "a.com.",
    ?assertEqual(to_string(Name), Str),
    NewName = from_string(Str),
    ?assert(is_equal(Name, NewName)),
    Root = from_string("."),
    ?assertEqual(parent(Root), null).

is_equal_test() ->
    ACom= <<1, $A, 3, $c, $O, $m, 0 >>,
    Name1 = from_wire_discard_left(ACom),
    ACom2 = <<1, $A, 3, $c, $O, $m, 0 >>,
    Name2 = from_wire_discard_left(ACom2),
    ?assert(is_equal(Name1, Name2)),
    ACom3 = <<1, $A, 3, $C, $O, $m, 0 >>,
    Name3 = from_wire_discard_left(ACom3),
    ?assert(is_equal(Name1, Name3)),
    ACom4 = <<1, $b, 3, $C, $O, $m, 0 >>,
    Name4 = from_wire_discard_left(ACom4),
    ?assertNot(is_equal(Name1, Name4)).

parent_test() ->
    ACom= <<1, $A, 3, $c, $O, $m, 0 >>,
    Name = from_wire_discard_left(ACom),
    ?assertEqual(to_wire(Name), <<1, $a, 3, $c, $o, $m, 0>>),
    Com = parent(Name),
    ?assertEqual(len(Com), 5),
    ?assertEqual(label_count(Com), 2),
    Root = parent(Com),
    ?assertEqual(len(Root), 1),
    ?assertEqual(label_count(Root), 1),
    Empty = parent(Root),
    ?assertEqual(Empty, null).

substract_concat_test() ->
    COMName = from_string("COM"),
    CNName = from_string("CN"),
    ?assert(is_equal(concat(COMName, CNName), from_string("com.cn"))),
    ?assert(is_equal(substract(from_string("com.cn"), CNName), COMName)).


-endif.


