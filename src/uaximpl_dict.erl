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
