-module(uaximpl_proplist).

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
    fun (Key, Proplist) ->
            Default = make_ref(),
            case proplists:get_value(Key, Proplist, Default) of
                Default -> erlang:error({not_found, Key});
                Val -> Val
            end
    end.

put([]) ->
    fun (Key, Val, Proplist) -> [{Key, Val} | Proplist] end.

new([]) ->
    fun () -> [] end.

del([]) ->
    fun proplists:delete/2.

typecheck([]) ->
    fun erlang:is_list/1.

iter([]) ->
    fun (init, Proplist) -> {ok, Proplist};
        (next, []) -> done;
        (next, [K | Xs]) when is_atom(K) -> {ok, {K, true}, Xs};
        (next, [{K, V} | Xs]) -> {ok, {K, V}, Xs}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(EUNIT, true).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

get_test_() ->
    Get1 = uax:mk(get, {proplist, [a]}),
    
    [?_assertEqual(val, Get1([a], obj1(a, val)))].

put_test_() ->
    Put1 = uax:mk(put, {proplist, [a]}),
    
    Obj0 = [],
    
    [?_assertEqual(obj1(a, val), Put1([a], val, Obj0))].

new_test_() ->
    New1 = uax:mk(new, {proplist, [a]}),
    
    [?_assertEqual(obj1(a, val), New1([{a, val}]))].

del_test_() ->
    Del1 = uax:mk(del, {proplist, [a]}),

    Obj0 = [],
    
    [?_assertEqual(Obj0, Del1([a], obj1(a, val)))].
    
typecheck_test_() ->
    Typecheck = typecheck([]),

    [?_assertEqual(true, Typecheck([])),
     ?_assertEqual(true, Typecheck([{key, val}])),
     ?_assertEqual(false, Typecheck(not_proplist))].


iter_test_() ->
    R1 = uaxc:compile({proplist, [a]}),
                         
    Obj1 = objn([{X, X} || X <- lists:seq(1, 100)]),
    
    Del = fun (K, T) -> proplists:delete(K, T) end,
    
    [?_assertEqual([], uax:iter(R1, fun (K, V, State) ->
                                            true = K == V, 
                                            {ok, Del(K, State)}
                                    end, Obj1, Obj1))].


obj1(K, V) -> [{K, V}].

objn(KVs) -> KVs.


-endif.
