-module(uaximpl_orddict).

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
    fun (Key, Orddict) ->
            case orddict:find(Key, Orddict) of
                {ok, Val} -> Val;
                error -> erlang:error({not_found, Key})
            end
    end.

put([]) ->
    fun orddict:store/3.

new([]) ->
    fun orddict:new/0.

del([]) ->
    fun orddict:erase/2.

typecheck([]) ->
    fun erlang:is_list/1.

iter([]) ->
    fun (init, Orddict) -> {ok, Orddict};
        (next, []) -> done;
        (next, [{K, V} | KVs]) -> {ok, {K, V}, KVs}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(EUNIT, true).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

get_test_() ->
    Get1 = uax:mk(get, {orddict, [a]}),

    [?_assertEqual(val, Get1([a], obj1(a, val)))].

put_test_() ->
    Put1 = uax:mk(put, {orddict, [a]}),
    
    Obj0 = orddict:new(),
    
    [?_assertEqual(obj1(a, val), Put1([a], val, Obj0))].

new_test_() ->
    New1 = uax:mk(new, {orddict, [a]}),
    
    [?_assertEqual(obj1(a, val), New1([{a, val}]))].

del_test_() ->
    Del1 = uax:mk(del, {orddict, [a]}),

    Obj0 = orddict:new(),
    
    [?_assertEqual(Obj0, Del1([a], obj1(a, val)))].
    
typecheck_test_() ->
    Typecheck = typecheck([]),

    [?_assertEqual(true, Typecheck(orddict:new())),
     ?_assertEqual(true, Typecheck(obj1(key, val))),
     ?_assertEqual(false, Typecheck(not_orddict))].

iter_test_() ->
    R1 = uaxc:compile({orddict, [a]}),
                         
    Obj1 = objn([{X, X} || X <- lists:seq(1, 100)]),
    
    Del = fun (K, T) -> orddict:erase(K, T) end,
                         
    [?_assertEqual(
        orddict:new(),
        uax:iter(R1, fun (K, V, State) ->
                             true = K == V, 
                             {ok, Del(K, State)}
                     end, Obj1, Obj1))].


obj1(K, V) -> orddict:store(K, V, orddict:new()).

objn(KVs) ->
    lists:foldl(fun ({K, V}, T) -> orddict:store(K, V, T) end, orddict:new(), KVs).


-endif.



