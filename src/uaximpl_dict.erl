-module(uaximpl_dict).

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
    fun (Key, Dict) -> uax_get:try_get(fun dict:fetch/2, Key, Dict) end.

put([]) ->
    fun dict:store/3.

new([]) ->
    fun dict:new/0.

del([]) ->
    fun () -> throw(todo) end.

typecheck([]) ->
    fun (D) -> is_tuple(D) andalso element(1, D) == dict andalso size(D) == 9 end.

