-module(uaximpl_tuple).

-behaviour(uaximpl).

-export([opts/0, args/1, new/1, get/1, put/1, del/1, typecheck/1, iter/1]).

-export([foldl/3, foldr/3]).

opts() ->
    {[{key, fun (F) -> is_function(F, 1) end}],
     [{none_tag, fun (_) -> true end, undefined}],
     [{encode, fun (F) -> is_function(F, 1) end},
      {decode, fun (F) -> is_function(F, 1) end}]}.


args(put) -> [none_tag];
args(del) -> [none_tag];
args(iter) -> [none_tag];
args(_) -> [].


get([]) ->
    fun (Pos, Tuple) -> uax_util:try_get(fun erlang:element/2, Pos, Tuple) end.

put([{none_tag, NoneTag}]) ->
    fun (Idx, Val, Tuple) -> do_put_default(Idx, Val, Tuple, NoneTag) end.

new([]) ->
    fun () -> {} end.

del([{none_tag, NoneTag}]) ->
    fun (Idx, Tuple) -> do_del_default(Idx, Tuple, NoneTag) end.

typecheck([]) ->
    fun erlang:is_tuple/1.

iter([{none_tag, NoneTag}]) ->
    VT = fun (X) -> not (X =:= NoneTag) end,
    fun (init, Tuple) -> {ok, {VT, Tuple, 1, size(Tuple)}};
        (next, Iter) -> next(Iter)
    end.
            

%%

next({VT, Tuple, Ix, MaxIx}) ->
    case uax_util:find_elem_ix(VT, Tuple, Ix, MaxIx) of
        {ok, Ix1, Val} -> {ok, {Ix1, Val}, {VT, Tuple, Ix1 + 1, MaxIx}};
        none -> done
    end.

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(EUNIT, true).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

get_test_() ->
    Get1 = uax:mk(get, {{tuple, fun (a) -> 1 end}, [a]}),
    
    [?_assertEqual(val, Get1([a], obj1(1, val)))].

put_test_() ->
    Put1 = uax:mk(put, {{tuple, fun (a) -> 1 end}, [a]}),
    
    Obj0 = {},
    
    [?_assertEqual(obj1(1, val), Put1([a], val, Obj0))].

new_test_() ->
    New1 = uax:mk(new, {{tuple, fun (a) -> 1 end}, [a]}),
    
    [?_assertEqual(obj1(1, val), New1([{a, val}]))].

del_test_() ->
    Del1 = uax:mk(del, {{tuple, fun (a) -> 1 end}, [a]}),
    
    [?_assertEqual({undefined}, Del1([a], obj1(1, val)))].
    
typecheck_test_() ->
    Typecheck = typecheck([]),

    [?_assertEqual(true, Typecheck({})),
     ?_assertEqual(true, Typecheck({something})),
     ?_assertEqual(false, Typecheck(not_tuple))].


iter_test_() ->
    R1 = uaxc:compile({{tuple, fun (X) -> X end}, [{a}]}),
    
    Obj1 = objn([{K, K} || K <- lists:seq(1, 100)]),
    
    Del = fun (K, L) -> do_del_default(K, L, undefined) end,

    [?_assertEqual(list_to_tuple(lists:duplicate(100, undefined)),
                   uax:iter(R1, fun (K, V, State) ->
                                        true = K == V,
                                        {ok, Del(K, State)}
                                end, Obj1, Obj1))].


obj1(K, V) -> do_put_default(K, V, {}, undefined).

objn(KVs) -> 
    lists:foldl(fun ({K, V}, Acc) -> do_put_default(K, V, Acc, undefined) end, {}, KVs).

-endif.
