-module(uaximpl_gb_tree).

-behaviour(uaximpl).

-export([opts/0, args/1, new/1, get/1, put/1, del/1, typecheck/1, iter/1]).

opts() ->
    {[],
     [],
     [{key, fun (F) -> is_function(F, 1) end},
      {encode, fun (F) -> is_function(F, 1) end},
      {decode, fun (F) -> is_function(F, 1) end},
      {none_tag, fun (_) -> true end}]}.

args(_) -> [].

get([]) ->
    fun (Key, Gbtree) ->
            case gb_trees:lookup(Key, Gbtree) of
                none -> erlang:error({not_found, Key});
                {value, Val} -> Val
            end
    end.

put([]) ->
    fun gb_trees:enter/3.

new([]) ->
    fun gb_trees:empty/0.

del([]) ->
    fun gb_trees:delete/2.

typecheck([]) ->
    fun ({0, nil}) -> true;
        ({C, {_K, _V, _Left, _Right}}) when is_integer(C), C > 0 -> true;
        (_) -> false
    end.

iter([]) ->
    fun (init, Gbtree) -> {ok, gb_trees:iterator(Gbtree)};
        (next, Iter) -> next(Iter)
    end.

%%

next(Iter) ->
    case gb_trees:next(Iter) of
        none -> done;
        {K, V, Iter1} -> {ok, {K, V}, Iter1}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(EUNIT, true).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

get_test_() ->
    Get1 = uax:mk(get, {gb_tree, [a]}),
    %% Get2 = uax:mk(get, {{gb_tree, fun (K) -> atom_to_list(K) end}, [a]}),
    %% Get3 = uax:mk(get, {{gb_tree, [{decode, fun (V) -> binary_to_list(V) end}]}, [a]}),
    %% Get4 = uax:mk(get, {{gb_tree, [{none_tag, none}]}, [a]}),
    %% Get5 = uax:mk(get, {{gb_tree, [{key, fun (K) -> atom_to_list(K) end},
    %%                                {decode, fun (V) -> binary_to_list(V) end}]}, [a]}),
    %% Get6 = uax:mk(get, {{gb_tree, [{key, fun (K) -> atom_to_list(K) end},
    %%                                {none_tag, none}]}, [a]}),
    %% Get7 = uax:mk(get, {{gb_tree, [{decode, fun (V) -> list_to_atom(binary_to_list(V)) end},
    %%                                {none_tag, none}]}, [a]}),
    %% Get8 = uax:mk(get, {{gb_tree, [{key, fun (K) -> atom_to_list(K) end},
    %%                                {decode, fun (V) -> list_to_atom(binary_to_list(V)) end},
    %%                                {none_tag, none}]}, [a]}),
    
    [?_assertEqual(val, Get1([a], obj1(a, val)))].
     %% ?_assertEqual(val, Get2([a], obj1("a", val))),
     %% ?_assertEqual("val", Get3([a], obj1(a, <<"val">>))),
     %% ?_assertError({not_found, a}, Get4([a], obj1(a, none))),
     %% ?_assertEqual("none", Get5([a], obj1("a", <<"none">>))),
     %% ?_assertError({not_found, a}, Get6([a], obj1("a", none))),
     %% ?_assertError({not_found, a}, Get7([a], obj1(a, <<"none">>))),
     %% ?_assertError({not_found, a}, Get8([a], obj1("a", <<"none">>)))].

put_test_() ->
    Put1 = uax:mk(put, {gb_tree, [a]}),
    %% Put2 = uax:mk(put, {{gb_tree, fun (K) -> atom_to_list(K) end}, [a]}),
    %% Put3 = uax:mk(put, {{gb_tree, [{encode, fun (V) -> binary_to_list(V) end}]}, [a]}),
    %% Put5 = uax:mk(put, {{gb_tree, [{key, fun (K) -> atom_to_list(K) end},
    %%                                {encode, fun (V) -> binary_to_list(V) end}]}, [a]}),
    
    Obj0 = gb_trees:empty(),
    
    [?_assertEqual(obj1(a, val), Put1([a], val, Obj0))].
     %% ?_assertEqual(obj1("a", val), Put2([a], val, Obj0)),
     %% ?_assertEqual(obj1(a, "val"), Put3([a], <<"val">>, Obj0)),
     %% ?_assertEqual(obj1("a", "val"), Put5([a], <<"val">>, Obj0))].

new_test_() ->
    New1 = uax:mk(new, {gb_tree, [a]}),
    %% New2 = uax:mk(new, {{gb_tree, fun (K) -> atom_to_list(K) end}, [a]}),
    %% New3 = uax:mk(new, {{gb_tree, [{encode, fun (V) -> binary_to_list(V) end}]}, [a]}),
    %% New5 = uax:mk(new, {{gb_tree, [{key, fun (K) -> atom_to_list(K) end},
    %%                                {encode, fun (V) -> binary_to_list(V) end}]}, [a]}),
    
    [?_assertEqual(obj1(a, val), New1([{a, val}]))].
     %% ?_assertEqual(obj1("a", val), New2([{a, val}])),
     %% ?_assertEqual(obj1(a, "val"), New3([{a, <<"val">>}])),
     %% ?_assertEqual(obj1("a", "val"), New5([{a, <<"val">>}]))].

del_test_() ->
    Del1 = uax:mk(del, {gb_tree, [a]}),
    %% Del2 = uax:mk(del, {{gb_tree, fun (K) -> atom_to_list(K) end}, [a]}),

    Obj0 = gb_trees:empty(),
    
    [?_assertEqual(Obj0, Del1([a], obj1(a, val)))].
     %% ?_assertEqual(Obj0, Del2([a], obj1("a", val)))].
    
typecheck_test_() ->
    Typecheck = typecheck([]),

    [?_assertEqual(true, Typecheck(gb_trees:empty())),
     ?_assertEqual(true, Typecheck(obj1(key, val))),
     ?_assertEqual(false, Typecheck(not_gb_tree))].


iter_test_() ->
    R1 = uaxc:compile({gb_tree, [a]}),
    %% R3 = uaxc:compile({{gb_tree, [{decode, fun (V) -> integer_to_list(V) end}]}, [a]}),
    %% R4 = uaxc:compile({{gb_tree, [{none_tag, none}]}, [a]}),
    %% R7 = uaxc:compile({{gb_tree, [{decode, fun (V) -> binary_to_list(V) end},
    %%                               {none_tag, none}]}, [a]}),

    %% OddNone = fun (X) -> if X rem 2 == 0 -> X; true -> none end end,
                         
    Obj1 = objn([{X, X} || X <- lists:seq(1, 100)]),
    %% Obj3 = objn([{X, X} || X <- lists:seq(1, 100)]),
    %% Obj4 = objn([{X, OddNone(X)} || X <- lists:seq(1, 100)]),
    %% Obj7 = objn([{X, OddNone(X)} || X <- lists:seq(1, 100)]),
    
    Del = fun (K, T) -> gb_trees:delete(K, T) end,
                         
    [?_assertEqual(gb_trees:empty(),
                   uax:iter(R1, fun (K, V, State) ->
                                        true = K == V, 
                                        {ok, Del(K, State)}
                                end, Obj1, Obj1))].
     %% ?_assertEqual(
     %%    gb_trees:empty(),
     %%    uax:iter(R3, fun (K, V, State) ->
     %%                         true = integer_to_list(K) == V,
     %%                         {ok, Del(K, State)}
     %%                 end, Obj3, Obj3)),
     %% ?_assertEqual(
     %%    gb_trees:to_list(objn([{X, none} || X <- lists:seq(1, 100), X rem 2 /= 0])),
     %%    gb_trees:to_list(uax:iter(R4, fun (K, V, State) ->
     %%                                          true = K == V, 
     %%                                          {ok, Del(K, State)}
     %%                                  end, Obj4, Obj4))),
     %% ?_assertEqual(
     %%    gb_trees:to_list(objn([{X, none} || X <- lists:seq(1, 100), X rem 2 /= 0])),
     %%    gb_trees:to_list(uax:iter(R7, fun (K, V, State) ->
     %%                                          true = integer_to_list(K) == V,
     %%                                          {ok, Del(K, State)}
     %%                                  end, Obj7, Obj7)))].


obj1(K, V) -> gb_trees:insert(K, V, gb_trees:empty()).

objn(KVs) ->
    lists:foldl(fun ({K, V}, T) -> gb_trees:enter(K, V, T) end, gb_trees:empty(), KVs).


-endif.
