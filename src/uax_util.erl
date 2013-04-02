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


try_call(F, [Arg1, Arg2], Error) ->
    try
        F(Arg1, Arg2)
    catch
        _:_ -> erlang:error(Error)
    end.


%% check_key_fun(F) when is_function(F, 1) -> F;
%% check_key_fun(F) -> erlang:error({schema_error, {key, F}}).
     
    
