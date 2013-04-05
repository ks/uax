-module(uaximpl_list).

-behaviour(uaximpl).

-export([opts/0, args/1, new/1, get/1, put/1, del/1, typecheck/1]).


opts() ->
    {[{key, fun (F) -> is_function(F, 1) end}],
     [{none_tag, fun (_) -> true end, undefined}],
     [{encode, fun (F) -> is_function(F, 1) end},
      {decode, fun (F) -> is_function(F, 1) end}]}.


args(put) -> [none_tag];
args(_) -> [].

get([]) ->
    fun (Pos, List) -> uax_get:try_get(fun lists:nth/2, Pos, List) end.

put([]) ->
    fun (_Key, _Val, _List) -> erlang:error({todo, {put, list}}) end.

new([]) ->
    fun () -> [] end.

del([]) ->
    fun () -> throw(todo) end.

typecheck([]) ->
    fun erlang:is_list/1.
