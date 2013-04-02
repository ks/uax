-module(uaximpl_orddict).

-behaviour(uaximpl).

-export([opts/0, args/1, new/1, get/1, put/1, del/1, typecheck/1]).

opts() ->
    {[],
     [],
     [{key, fun (F) -> is_function(F, 1) end},
      {decode, fun (F) -> is_function(F, 1) end},
      {none_tag, fun (_) -> true end}]}.

args(_) -> [].

get([]) ->
    fun (Key, Orddict) -> uax_get:try_get(fun orddict:fetch/2, Key, Orddict) end.

put([]) ->
    fun orddict:store/3.

new([]) ->
    fun orddict:new/0.

del([]) ->
    fun () -> throw(todo) end.

typecheck([]) ->
    fun erlang:is_list/1.



