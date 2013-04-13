%% KVList differ from other impls, that the value it returns with default schema (without {valpos, pos_integer()})
%% returns whole tuple (with the key) as value to not lose information if tuple contains other elemens.
%% 
%% To get same behavior as other impls, use {valpos, pos_integer()} which works with a specific tuple element instead.

-module(uaximpl_kvlist). 

-behaviour(uaximpl).

-export([opts/0, args/1, new/1, get/1, put/1, del/1, typecheck/1, iter/1]).

opts() ->
    {[],
     [{keypos, fun (X) -> is_integer(X) andalso X > 0 end, 1},
      {fill_tag, fun (_) -> true end, undefined}],
     [{key, fun (F) -> is_function(F, 1) end},
      {valpos, fun (X) -> is_integer(X) andalso X > 0 end},
      {none_tag, fun (_) -> true end},
      {encode, fun (F) -> is_function(F, 1) end},
      {decode, fun (F) -> is_function(F, 1) end}]}.


args(get) -> [keypos, valpos];
args(put) -> [keypos, valpos, fill_tag];
args(del) -> [keypos];
args(iter) -> [keypos, valpos];
args(_) -> [].


get([{keypos, Keypos}]) ->
    fun (Key, KVlist) ->
            case lists:keyfind(Key, Keypos, KVlist) of
                false -> erlang:error({not_found, Key});
                Tuple -> Tuple
            end
    end;
get([{keypos, Keypos}, {valpos, Valpos}]) ->
    fun (Key, KVlist) ->
            case lists:keyfind(Key, Keypos, KVlist) of
                false -> erlang:error({not_found, Key});
                Tuple -> element(Valpos, Tuple)
            end
    end.


put([{keypos, Keypos}, {fill_tag, _FillTag}]) ->
    fun (Key, Val, KVlist) when element(Keypos, Val) =:= Key -> [Val | KVlist] end;
put([{keypos, 1}, {valpos, 2}, {fill_tag, _FillTag}]) ->
    fun (Key, Val, KVlist) -> [{Key, Val} | KVlist] end;
put([{keypos, Keypos}, {valpos, Valpos}, {fill_tag, FillTag}]) ->
    Size = max(Keypos, Valpos),
    fun (Key, Val, KVlist) ->
            [erlang:make_tuple(Size, FillTag, [{Keypos, Key}, {Valpos, Val}]) | KVlist]
    end.


new([]) ->
    fun () -> [] end.


del([{keypos, Keypos}]) ->
    fun (Key, KVlist) -> lists:keydelete(Key, Keypos, KVlist) end.


iter([{keypos, Keypos}]) ->
    fun (init, KVlist) -> {ok, KVlist};
        (next, []) -> done;
        (next, [X | Xs]) -> {ok, {element(Keypos, X), X}, Xs}
    end;
iter([{keypos, Keypos}, {valpos, Valpos}]) ->
    fun (init, KVlist) -> {ok, KVlist};
        (next, []) -> done;
        (next, [X | Xs]) -> {ok, {element(Keypos, X), element(Valpos, X)}, Xs}
    end.


typecheck([]) ->
    fun erlang:is_list/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(EUNIT, true).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

get_test_() ->
    Get1 = uax:mk(get, {kvlist, [a]}),                  %% returns whole tuple
    Get2 = uax:mk(get, {{kvlist, [{valpos, 2}]}, [a]}), %% returns specific tuple element as value
    
    [?_assertEqual({a, val}, Get1([a], obj1(a, val))),
     ?_assertEqual(val, Get2([a], obj1(a, val)))].

put_test_() ->
    Put1 = uax:mk(put, {kvlist, [a]}),
    Put2 = uax:mk(put, {{kvlist, [{valpos, 2}]}, [a]}),
    
    Obj0 = [],
    
    [?_assertEqual(obj1(a, val), Put1([a], {a, val}, Obj0)),
     ?_assertEqual(obj1(a, val), Put2([a], val, Obj0))].

new_test_() ->
    New1 = uax:mk(new, {kvlist, [a]}),
    New2 = uax:mk(new, {{kvlist, [{valpos, 2}]}, [a]}),
    
    [?_assertEqual(obj1(a, val), New1([{a, {a, val}}])), %% value here must contain the key, Put function checks that (otherwise it will become unreachable)
     ?_assertEqual(obj1(a, val), New2([{a, val}]))].

del_test_() ->
    Del1 = uax:mk(del, {kvlist, [a]}),

    Obj0 = [],
    
    [?_assertEqual(Obj0, Del1([a], obj1(a, val)))].
    

typecheck_test_() ->
    Typecheck = typecheck([]),

    [?_assertEqual(true, Typecheck([])),
     ?_assertEqual(true, Typecheck([{key, val}])),
     ?_assertEqual(false, Typecheck(not_kvlist))].


iter_test_() ->
    R1 = uaxc:compile({kvlist, [a]}),
    R2 = uaxc:compile({{kvlist, [{valpos, 2}]}, [a]}),
                         
    Obj1 = objn([{X, X} || X <- lists:seq(1, 100)]),
    
    Del = fun (K, T) -> lists:keydelete(K, 1, T) end,
    
    [?_assertEqual([], uax:iter(R1, fun (K, V, State) ->
                                            true = {K, K} == V,
                                            {ok, Del(K, State)}
                                    end, Obj1, Obj1)),
     ?_assertEqual([], uax:iter(R2, fun (K, V, State) ->
                                            true = K == V, 
                                            {ok, Del(K, State)}
                                    end, Obj1, Obj1))].


obj1(K, V) -> [{K, V}].

objn(KVs) -> KVs.


-endif.
