-module(uax_util).

-compile(export_all).


maptree(Down, Up, Tree) ->
    maptree(Down, Up, Tree, []).
maptree(Down, Up, Tree, Prev) ->
    Up(case Down(Tree, Prev) of
           {ok, Val} ->
               {leaf, Val};
           {ok, Val, Kids} when is_list(Kids) ->
               Prev1 = [Val | Prev],
               Kids1 = [maptree(Down, Up, K, Prev1) || K <- Kids],
               {node, Val, Kids1}
       end, Prev).

identity(X) -> X.


position(Elem, List) when is_list(List) ->
    position1(Elem, List, 1).

position1(Elem, [], _I) ->
    erlang:error({not_found, Elem});
position1(Elem, [Elem | _], I) ->
    I;
position1(Elem, [_ | Xs], I) ->
    position1(Elem, Xs, I + 1).


try_call(F, [Arg1, Arg2], Error) ->
    try
        F(Arg1, Arg2)
    catch
        _:_ -> erlang:error(Error)
    end.


try_get(Get, K, KVs) ->
    try Get(K, KVs) of
        V -> V
    catch
        _:_ -> erlang:error({not_found, K})
    end.

%% check_key_fun(F) when is_function(F, 1) -> F;
%% check_key_fun(F) -> erlang:error({schema_error, {key, F}}).
     
    
fold_range(Fun, Acc, From, To) ->
    fold_range(Fun, Acc, From, To, 1).

fold_range(Fun, Acc, Idx, Limit, Step) when Idx < Limit ->
    fold_range(Fun, Fun(Idx, Acc), Idx + Step, Limit, Step);
fold_range(_Fun, Acc, Idx, Limit, _Step) when Idx >= Limit ->
    Acc.


keyselect(Keys, KVs) ->
    lists:foldr(
      fun (K, KVs0) ->
              case lists:keyfind(K, 1, KVs) of
                  {K, V} -> [{K, V} | KVs0];
                  false -> KVs0
              end
      end, [], Keys).



find_elem_ix(_Test, _Tuple, Ix, MaxIx) when Ix > MaxIx -> 
    none;
find_elem_ix(Test, Tuple, Ix, MaxIx) ->
    E = element(Ix, Tuple),
    case Test(E) of
        true  -> {ok, Ix, E};
        false -> find_elem_ix(Test, Tuple, Ix + 1, MaxIx)
    end.
       

%% 

path_next({Elem, Id}, Kids) when is_atom(Elem) ->
    {Id, lists:keyfind({Elem}, 2, Kids)};
path_next(Elem, Kids) when is_atom(Elem) ->
    {Elem, lists:keyfind(Elem, 2, Kids)};
path_next(Elem, _Kids) ->
    erlang:error({path_error, Elem}).
