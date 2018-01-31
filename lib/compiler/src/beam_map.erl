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
%%

-module(beam_map).

-export([module/2]).
-import(lists, [reverse/1]).

-spec module(beam_utils:module_code(), [compile:option()]) ->
                    {'ok',beam_utils:module_code()}.

module({Mod,Exp,Attr,Fs0,Lc}, _Opt) ->
    Fs = [function(F) || F <- Fs0],
    {ok,{Mod,Exp,Attr,Fs,Lc}}.

function({function,Name,Arity,CLabel,Is0}) ->
    try
        Is = join_maps(Is0),
        {function,Name,Arity,CLabel,Is}
    catch
        Class:Error ->
            Stack = erlang:get_stacktrace(),
            io:fwrite("Function: ~w/~w\n", [Name,Arity]),
            erlang:raise(Class, Error, Stack)
    end.

%% join_maps(Instructions0) -> Instructions
%%  Join get_map_element instructions back into get_map_elements

join_maps(Is) ->
    D = beam_utils:index_labels(Is),
    join_maps_1(Is, D).

join_maps_1([{get_map_element,{f,Lbl}=F,S,K,V}|Is0], D) ->
    case S =/= V orelse is_killed(S, Lbl, Is0, D) of
        true ->
            case K =/= V orelse is_killed(K, Lbl, Is0, D) of
                true ->
                    {KVs,Is} = collect_maps(Is0, Lbl, S, D, [V,K]),
                    [{get_map_elements,F,S,{list,KVs}}|join_maps_1(Is, D)];
                false ->
                    [{get_map_elements,F,S,{list,[K,V]}}|join_maps_1(Is0, D)]
            end;
        false ->
            [{get_map_elements,F,S,{list,[K,V]}}|join_maps_1(Is0, D)]
    end;
join_maps_1([I|Is], D) ->
    [I|join_maps_1(Is, D)];
join_maps_1([], _D) ->
    [].

collect_maps([{get_map_element,{f,Lbl},S,{literal,_}=K,V}=I|Is], Lbl, S, D, Acc) ->
    case S =/= V orelse is_killed(S, Lbl, Is, D) of
        true ->
            case K =/= V orelse is_killed(K, Lbl, Is, D) of
                true ->
                    collect_maps(Is, Lbl, S, D, [V,K|Acc]);
                false ->
                    {reverse(Acc),[I|Is]}
            end;
        false ->
            {reverse(Acc),[I|Is]}
    end;
collect_maps(Is, _Lbl, _S, _D, Acc) ->
    {reverse(Acc),Is}.

is_killed(R, Lbl, Is, D) ->
    beam_utils:is_killed_at(R, Lbl, D) andalso beam_utils:is_killed(R, Is, D).
