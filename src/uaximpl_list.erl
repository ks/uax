-module(uaximpl_list).

-behaviour(uaximpl).

-export([opts/0, args/1, new/1, get/1, put/1, del/1, typecheck/1, iter/1]).


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
    fun (Pos, List) -> uax_util:try_get(fun lists:nth/2, Pos, List) end.

put([{none_tag, NoneTag}]) ->
    fun (Pos, Val, List) -> do_put_default(Pos, Val, List, NoneTag) end.

new([]) ->
    fun () -> [] end.

del([{none_tag, NoneTag}]) ->
    fun (Pos, List) -> do_del_default(Pos, List, NoneTag) end.

typecheck([]) ->
    fun erlang:is_list/1.


iter([{none_tag, NoneTag}]) ->
    fun (init, List) -> {ok, {List, 1, NoneTag}};
        (next, Iter) -> next(Iter)
    end.

%%

next({[], _, _}) ->
    done;
next({[NoneTag | Xs], Pos, NoneTag}) ->
    next({Xs, Pos + 1, NoneTag});
next({[X | Xs], Pos, NoneTag}) ->
    {ok, {Pos, X}, {Xs, Pos + 1, NoneTag}}.


do_put_default(Idx, Val, List, NoneTag) ->
    (is_integer(Idx) andalso Idx > 0) orelse erlang:error(badarg),
    {RemXs, PrevReversedXs} =
        uax_util:fold_range(
          fun (_, {[X | Xs], Acc}) -> {Xs, [X | Acc]};
              (_, {[], Acc}) -> {[], [NoneTag | Acc]}
          end, {List, []}, 1, Idx),
    lists:concat([lists:reverse(PrevReversedXs), [Val], 
                  if RemXs == [] -> []; true -> tl(RemXs) end]).
                           

do_del_default(Idx, List, NoneTag) ->
    (is_integer(Idx) andalso Idx > 0) orelse erlang:error(badarg),
    do_put_default(Idx, NoneTag, List, NoneTag).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(EUNIT, true).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

get_test_() ->
    Get1 = uax:mk(get, {{list, fun (a) -> 1 end}, [a]}),
    
    [?_assertEqual(val, Get1([a], obj1(1, val)))].

put_test_() ->
    Put1 = uax:mk(put, {{list, fun (a) -> 1 end}, [a]}),
    
    Obj0 = [],
    
    [?_assertEqual(obj1(1, val), Put1([a], val, Obj0))].

new_test_() ->
    New1 = uax:mk(new, {{list, fun (a) -> 1 end}, [a]}),
    
    [?_assertEqual(obj1(1, val), New1([{a, val}]))].

del_test_() ->
    Del1 = uax:mk(del, {{list, fun (a) -> 1 end}, [a]}),
    
    [?_assertEqual([undefined], Del1([a], obj1(1, val)))].
    
typecheck_test_() ->
    Typecheck = typecheck([]),

    [?_assertEqual(true, Typecheck([])),
     ?_assertEqual(true, Typecheck([something])),
     ?_assertEqual(false, Typecheck(not_list))].


iter_test_() ->
    R1 = uaxc:compile({{list, fun (X) -> X end}, [{a}]}),
    
    Obj1 = objn(lists:seq(1, 100)),
    
    Del = fun (K, L) -> do_del_default(K, L, undefined) end,
    
    [?_assertEqual(lists:duplicate(100, undefined),
                   uax:iter(R1, fun (K, V, State) ->
                                        true = K == V,
                                        {ok, Del(K, State)}
                                end, Obj1, Obj1))].


obj1(K, V) -> do_put_default(K, V, [], undefined).

objn(L) -> L.



-endif.


