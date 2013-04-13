-module(uaximpl_array).

-behaviour(uaximpl).

-export([opts/0, args/1, new/1, get/1, put/1, del/1, typecheck/1, iter/1]).

%% from array.erl
-record(array, {size, max, default, elements}).
-define(LEAFSIZE, 10).
-define(NODEPATTERN(S), {_,_,_,_,_,_,_,_,_,_,S}).
%%

opts() ->
    {[{key, fun (F) -> is_function(F, 1) end}], % required
     [{none_tag, fun (_) -> true end, undefined}], % supplied when not present
     [{encode, fun (F) -> is_function(F, 1) end}, % checked when present
      {decode, fun (F) -> is_function(F, 1) end}]}.

                          
args(new) -> [none_tag];
args(del) -> [none_tag];
args(iter) -> [none_tag];
args(_) -> [].


get([]) ->
    fun array:get/2.

put([]) ->
    fun array:set/3.

new([{none_tag, NoneTag}]) ->
    fun () -> array:new([{default, NoneTag}]) end.

del([{none_tag, NoneTag}]) ->
    fun (Idx, Array) -> array:set(Idx, NoneTag, Array) end.

typecheck([]) ->
    fun array:is_array/1.

iter([{none_tag, NoneTag}]) ->
    NT = fun (N) when N =:= NoneTag -> false;
             (I) when is_integer(I) -> false;
             (T) when is_tuple(T) -> true
         end,
    VT = fun (V) -> not (V =:= NoneTag) end,
    fun (init, #array{elements = Es}) -> {ok, {0, Es, 1, [], NT, VT}};
        (next, Iter) -> down(Iter)
    end.

%%

down({AIx, Es = ?NODEPATTERN(NCap), EIx, Prev, NT, VT}) ->
    case uax_util:find_elem_ix(NT, Es, EIx, ?LEAFSIZE) of
        {ok, EIx1, E} ->
            AIx1 = AIx + ((EIx1 - 1) * NCap),
            down({AIx1, E, 1, [{Es, EIx1, AIx} | Prev], NT, VT});
        none ->
            up(Prev, NT, VT)
    end;

down({AIx, Es, EIx, Prev, NT, VT}) when is_tuple(Es) ->
    case uax_util:find_elem_ix(VT, Es, EIx, ?LEAFSIZE) of
        {ok, EIx1, Val} ->
            KV = {AIx + EIx1 - 1, Val},
            {ok, KV, {AIx, Es, EIx1 + 1, Prev, NT, VT}};
        none ->
            up(Prev, NT, VT)
    end.

up(Prev, NT, VT) ->
    case Prev of
        [] -> 
            done;
        [{P, PEIx, PAIx} | Prev1] ->
            down({PAIx, P, PEIx + 1, Prev1, NT, VT})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
