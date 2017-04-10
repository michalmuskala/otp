%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2017. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%

-module(map_sets).

-export([new/0,is_set/1,size/1,to_list/1,from_list/1]).
-export([is_element/2,add_element/2,del_element/2]).
-export([union/2,union/1,intersection/2,intersection/1]).
-export([is_disjoint/2]).
-export([subtract/2,is_subset/2]).
-export([fold/3,filter/2]).

-export_type([map_set/1]).

-type map_set(T) :: #{T => []}.

-compile(inline_list_funcs).

%% new() -> map_set(_).
%%  Return a new empty set.

-spec new() -> #{}.

new() -> #{}.

%% is_set(Term) -> boolean().
%%  Return 'true' if Term is a MapSet, else 'false'.

-spec is_set(MapSet) -> boolean() when
      MapSet :: term().

is_set(MapSet) when is_map(MapSet) ->
    is_set_1(maps:to_list(MapSet));
is_set(_) ->
    false.

is_set_1([]) ->
    true;
is_set_1([{_Key, Value} | Rest]) ->
    Value =:= [] andalso is_set_1(Rest).

%% size(MapSet) -> int().
%%  Return the number of elements in MapSet.

-spec size(MapSet) -> non_neg_integer() when
      MapSet :: map_set(_).

size(S) -> map_size(S).

%% to_list(MapSet) -> [Elem].
%%  Return the elements in MapSet as a list.

-spec to_list(MapSet) -> List when
      MapSet :: map_set(T),
      List :: [T].

to_list(MapSet) -> maps:keys(MapSet).

%% from_list([Elem]) -> MapSet.
%%  Build an ordered set from the elements in List.

-spec from_list(List) -> MapSet when
      List :: [T],
      MapSet :: map_set(T).

from_list(List) ->
    maps:from_list([{Key, []} || Key <- List]).

%% is_element(Element, MapSet) -> boolean().
%%  Return 'true' if Element is an element of MapSet, else 'false'.

-spec is_element(Element, MapSet) -> boolean() when
      Element :: term(),
      MapSet :: map_set(_).

is_element(Element, MapSet) ->
    maps:is_key(Element, MapSet).

%% add_element(Element, MapSet) -> MapSet.
%%  Return MapSet with Element inserted in it.

-spec add_element(Element, MapSet1) -> MapSet2 when
      Element :: E,
      MapSet1 :: map_set(T),
      MapSet2 :: map_set(T | E).

add_element(Element, MapSet) ->
    maps:put(Element, [], MapSet).

%% del_element(Element, MapSet) -> MapSet.
%%  Return MapSet but with Element removed.

-spec del_element(Element, MapSet1) -> MapSet2 when
      Element :: term(),
      MapSet1 :: map_set(T),
      MapSet2 :: map_set(T).

del_element(Element, MapSet) ->
    maps:remove(Element, MapSet).

%% union(MapSet1, MapSet2) -> MapSet
%%  Return the union of MapSet1 and MapSet2.

-spec union(MapSet1, MapSet2) -> MapSet3 when
      MapSet1 :: map_set(T1),
      MapSet2 :: map_set(T2),
      MapSet3 :: map_set(T1 | T2).

union(MapSet1, MapSet2) ->
    maps:merge(MapSet1, MapSet2).

%% union([MapSet]) -> MapSet
%%  Return the union of the list of ordered sets.

-spec union(MapSetList) -> MapSet when
      MapSetList :: [map_set(T)],
      MapSet :: map_set(T).

union([S1,S2|Ss]) ->
    union1(union(S1, S2), Ss);
union([S]) -> S;
union([]) -> #{}.

union1(S1, [S2|Ss]) -> union1(union(S1, S2), Ss);
union1(S1, []) -> S1.

%% intersection(MapSet1, MapSet2) -> MapSet.
%%  Return the intersection of MapSet1 and MapSet2.

-spec intersection(MapSet1, MapSet2) -> MapSet3 when
      MapSet1 :: map_set(_),
      MapSet2 :: map_set(_),
      MapSet3 :: map_set(_).

intersection(MapSet1, MapSet2) ->
    maps:with(maps:keys(MapSet1), MapSet2).

%% intersection([MapSet]) -> MapSet.
%%  Return the intersection of the list of MapSets.

-spec intersection(MapSetList) -> MapSet when
      MapSetList :: [map_set(_),...],
      MapSet :: map_set(_).

intersection([S1,S2|Ss]) ->
    intersection1(intersection(S1, S2), Ss);
intersection([S]) -> S.

intersection1(S1, [S2|Ss]) ->
    intersection1(intersection(S1, S2), Ss);
intersection1(S1, []) -> S1.

%% is_disjoint(MapSet1, MapSet2) -> boolean().
%%  Check whether MapSet1 and MapSet2 are disjoint.

-spec is_disjoint(MapSet1, MapSet2) -> boolean() when
      MapSet1 :: map_set(_),
      MapSet2 :: map_set(_).

is_disjoint(MapSet1, MapSet2) when map_size(MapSet1) =< map_size(MapSet2) ->
    is_disjoint1(MapSet2, maps:keys(MapSet1));
is_disjoint(MapSet1, MapSet2) ->
    is_disjoint1(MapSet1, maps:keys(MapSet2)).

is_disjoint1(Set, [Elem | Rest]) ->
    not maps:is_key(Elem, Set) andalso is_disjoint1(Set, Rest);
is_disjoint1(_Set, []) ->
    true.

%% subtract(MapSet1, MapSet2) -> MapSet.
%%  Return all and only the elements of MapSet1 which are not also in
%%  MapSet2.

-spec subtract(MapSet1, MapSet2) -> MapSet3 when
      MapSet1 :: map_set(_),
      MapSet2 :: map_set(_),
      MapSet3 :: map_set(_).

%% If the first set is less than twice the size of the second map,
%% it is faster to re-accumulate items in the first set that are not
%% present in the second set. Otherwise, it's faster to simply iterate
%% through each item in the second set, deleting them from the first set.
subtract(MapSet1, MapSet2) when map_size(MapSet1) =< map_size(MapSet2) * 2 ->
    maps:from_list([{Key, []} || Key <- maps:keys(MapSet1),
                                 not maps:is_key(Key, MapSet2)]);
subtract(MapSet1, MapSet2) ->
    maps:without(maps:keys(MapSet2), MapSet1).

%% is_subset(MapSet1, MapSet2) -> boolean().
%%  Return 'true' when every element of MapSet1 is also a member of
%%  MapSet2, else 'false'.

-spec is_subset(MapSet1, MapSet2) -> boolean() when
      MapSet1 :: map_set(_),
      MapSet2 :: map_set(_).

is_subset(MapSet1, _MapSet2) when MapSet1 =:= #{} ->
    true;
is_subset(MapSet1, MapSet2) when map_size(MapSet1) =< map_size(MapSet2) ->
    is_subset1(MapSet2, maps:keys(MapSet1));
is_subset(_MapSet1, _MapSet2) ->
    false.

is_subset1(Set, [Elem | Rest]) ->
    maps:is_key(Elem, Set) andalso is_subset1(Set, Rest);
is_subset1(_Set, []) ->
    true.

%% fold(Fun, Accumulator, MapSet) -> Accumulator.
%%  Fold function Fun over all elements in MapSet and return Accumulator.

-spec fold(Function, Acc0, MapSet) -> Acc1 when
      Function :: fun((Element :: T, AccIn :: term()) -> AccOut :: term()),
      MapSet :: map_set(T),
      Acc0 :: term(),
      Acc1 :: term().

fold(F, Acc, Set) ->
    lists:foldl(F, Acc, maps:keys(Set)).

%% filter(Fun, MapSet) -> MapSet.
%%  Filter MapSet with Fun.

-spec filter(Pred, MapSet1) -> MapSet2 when
      Pred :: fun((Element :: T) -> boolean()),
      MapSet1 :: map_set(T),
      MapSet2 :: map_set(T).

filter(Pred, MapSet) ->
    maps:from_list([{K, []} || K <- maps:keys(MapSet), Pred(K)]).
