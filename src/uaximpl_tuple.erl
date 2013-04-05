-module(uaximpl_tuple).

-behaviour(uaximpl).

-export([opts/0, args/1, new/1, get/1, put/1, del/1, typecheck/1]).


opts() ->
    {[{key, fun (F) -> is_function(F, 1) end}],
     [{none_tag, fun (_) -> true end, undefined}],
     [{encode, fun (F) -> is_function(F, 1) end},
      {decode, fun (F) -> is_function(F, 1) end}]}.


args(put) -> [none_tag];
args(del) -> [none_tag];
args(_) -> [].


get([]) ->
    fun (Pos, Tuple) -> uax_get:try_get(fun erlang:element/2, Pos, Tuple) end.

put([{none_tag, NoneTag}]) ->
    fun (Idx, Val, Tuple) -> do_put_default(Idx, Val, Tuple, NoneTag) end.

new([]) ->
    fun () -> {} end.

del([{none_tag, NoneTag}]) ->
    fun (Idx, Tuple) -> do_del_default(Idx, Tuple, NoneTag) end.

typecheck([]) ->
    fun erlang:is_tuple/1.


%%

do_put_default(Idx, Val, Tuple, NoneTag) ->
    (is_integer(Idx) andalso Idx > 0) orelse erlang:error(badarg),
    if Idx =< size(Tuple) ->
            setelement(Idx, Tuple, Val);
       true ->
            Vals = foldr(fun (KV, Acc) -> [KV | Acc] end, [], Tuple),
            erlang:make_tuple(Idx, NoneTag, [{Idx, Val} | Vals])
    end.


do_del_default(Idx, Tuple, NoneTag) ->
    (is_integer(Idx) andalso Idx > 0) orelse erlang:error(badarg),
    if Idx =< size(Tuple) ->
            setelement(Idx, Tuple, NoneTag);
       true ->
            Tuple
    end.


foldl(Fun, Acc, Tuple) ->
    Size = size(Tuple),
    foldl(Fun, Acc, Tuple, 1, Size).

foldl(Fun, Acc, Tuple, Idx, Size) when Idx =< Size ->
    Acc1 = Fun({Idx, element(Idx, Tuple)}, Acc),
    foldl(Fun, Acc1, Tuple, Idx + 1, Size);
foldl(_Fun, Acc, _Tuple, Idx, Size) when Idx > Size ->
    Acc.


foldr(Fun, Acc, Tuple) ->
    foldr(Fun, Acc, Tuple, size(Tuple)).

foldr(Fun, Acc, Tuple, Idx) when Idx > 0 ->
    Acc1 = Fun({Idx, element(Idx, Tuple)}, Acc),
    foldr(Fun, Acc1, Tuple, Idx - 1);
foldr(_Fun, Acc, _Tuple, Idx) when Idx < 1 ->
    Acc.
