-module(uaximpl_dict).

-behaviour(uaximpl).

-export([opts/0, args/1, new/1, get/1, put/1, del/1, typecheck/1, iter/1]).

%% from dict.erl
-record(dict, {size, n, maxn, bso, exp_size, con_size, empty, segs}).
%%

opts() ->
    {[],
     [],
     [{key, fun (F) -> is_function(F, 1) end},
      {encode, fun (F) -> is_function(F, 1) end},
      {decode, fun (F) -> is_function(F, 1) end},
      {none_tag, fun (_) -> true end}]}.

args(_) -> [].

get([]) ->
    fun (Key, Dict) -> uax_util:try_get(fun dict:fetch/2, Key, Dict) end.

put([]) ->
    fun dict:store/3.

new([]) ->
    fun dict:new/0.

del([]) ->
    fun dict:erase/2.

typecheck([]) ->
    fun (D) -> is_tuple(D) andalso element(1, D) == dict andalso size(D) == 9 end.

iter([]) ->
    fun (init, #dict{segs = Segs}) -> 
            Seg = element(1, Segs),
            KVs = element(1, Seg),
            {ok, {KVs, Seg, 1, Segs, 1}};
        (next, Iter) -> 
            next(Iter)
    end.

%%

next({[], Seg, SegIx, Segs, SegsIx}) ->
    up({Seg, SegIx, Segs, SegsIx});
next({[[K|V] | KVs], Seg, SegIx, Segs, SegsIx}) ->
    {ok, {K, V}, {KVs, Seg, SegIx, Segs, SegsIx}}.

up({Seg, SegIx, Segs, SegsIx}) ->
    TSeg = fun ([]) -> false; (_) -> true end,
    case uax_util:find_elem_ix(TSeg, Seg, SegIx + 1, size(Seg)) of
        none ->
            case uax_util:find_elem_ix(fun is_tuple/1, Segs, SegsIx + 1, size(Segs)) of
                none -> done;
                {ok, SegsIx1, Seg1} -> up({Seg1, 0, Segs, SegsIx1})
            end;
        {ok, SegIx1, KVs} ->
            next({KVs, Seg, SegIx1, Segs, SegsIx})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(EUNIT, true).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

get_test_() ->
    Get1 = uax:mk(get, {dict, [a]}),

    [?_assertEqual(val, Get1([a], obj1(a, val)))].

put_test_() ->
    Put1 = uax:mk(put, {dict, [a]}),
    
    Obj0 = dict:new(),
    
    [?_assertEqual(obj1(a, val), Put1([a], val, Obj0))].

new_test_() ->
    New1 = uax:mk(new, {dict, [a]}),
    
    [?_assertEqual(obj1(a, val), New1([{a, val}]))].

del_test_() ->
    Del1 = uax:mk(del, {dict, [a]}),

    Obj0 = dict:new(),
    
    [?_assertEqual(Obj0, Del1([a], obj1(a, val)))].
    
typecheck_test_() ->
    Typecheck = typecheck([]),

    [?_assertEqual(true, Typecheck(dict:new())),
     ?_assertEqual(true, Typecheck(obj1(key, val))),
     ?_assertEqual(false, Typecheck(not_a_dict))].

iter_test_() ->
    R1 = uaxc:compile({dict, [a]}),
                         
    Obj1 = objn([{X, X} || X <- lists:seq(1, 100)]),
    
    Del = fun (K, T) -> dict:erase(K, T) end,
                         
    [?_assertEqual(
        dict:new(),
        uax:iter(R1, fun (K, V, State) ->
                             true = K == V, 
                             {ok, Del(K, State)}
                     end, Obj1, Obj1))].


obj1(K, V) -> dict:store(K, V, dict:new()).

objn(KVs) ->
    lists:foldl(fun ({K, V}, T) -> dict:store(K, V, T) end, dict:new(), KVs).


-endif.



