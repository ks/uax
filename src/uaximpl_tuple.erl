-module(uaximpl_tuple).

-behaviour(uaximpl).

-export([opts/0, args/1, new/1, get/1, put/1, del/1, typecheck/1]).

opts() ->
    {[{key, fun (F) -> is_function(F, 1) end}],
     [{none_tag, fun (_) -> true end, undefined}],
     [{decode, fun (F) -> is_function(F, 1) end}]}.


args(del) -> [none_tag];
args(_) -> [].


get([]) ->
    fun (Pos, Tuple) -> uax_get:try_get(fun erlang:element/2, Pos, Tuple) end.

put([]) ->
    fun (_Key, _Val, _Tuple) ->
            erlang:error({todo, {put, tuple}})
    end.

new([]) ->
    fun () -> {} end.

del([{none_tag, _NoneTag}]) ->
    fun () -> throw(todo) end.

typecheck([]) ->
    fun erlang:is_tuple/1.


%% put_tuple_default(_Idx, _Val, _Tuple) ->
%%     erlang:error({todo, put_tuple_default}).
