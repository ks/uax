-module(uaximpl_array).

%%-behaviour(uaximpl).

-export([opts/0, args/1, new/1, get/1, put/1, del/1, typecheck/1]).

opts() ->
    {[], % required
     [{none_tag, fun (_) -> true end, undefined}], % supplied when not present
     [{key, fun (F) -> is_function(F, 1) end}, % checked when present
      {decode, fun (F) -> is_function(F, 1) end}]}.
                          
args(new) -> [none_tag];
args(del) -> [none_tag];
args(_) -> [].


get([]) ->
    fun array:get/2.

put([]) ->
    fun array:set/3.

new([{none_tag, NoneTag}]) ->
    fun () -> array:new([{default, NoneTag}]) end.

del([{none_tag, NoneTag}]) ->
    fun (Idx, Array) -> array:set(Idx, NoneTag, Array) end.

typecheck([]) ->
    fun array:is_array/1.

