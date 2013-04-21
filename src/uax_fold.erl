-module(uax_fold).

%%% FOLD evaluator here:
%%% Just flowing forward to the leaves using funs to drive and collect state

-include("uax.hrl").

-export([eval/4]).

%%%%%%%%%%

-record(op, {root, obj, lstate, code, tmp}).


eval(#uaxn{type = Type, typecheck = Typecheck} = X, PFs, {InitLState, GState}, Obj)
  when is_list(PFs) ->
    case Typecheck(Obj) of
        true ->
            case PFs of
                [] -> 
                    GState;
                [_ | _] ->
                    [#op{} = Op | Ops] = prepare(X, PFs, InitLState),
                    eval0([Op#op{obj = Obj} | Ops], [], GState)
            end;
        false ->
            erlang:error({type_error, Type, Obj})
    end.



prepare(_, [], _) ->
    [];

prepare(#uaxn{}, [Fun, _ | Q], _InitLState) when is_function(Fun) ->
    erlang:error({bad_query, Q});

prepare(#uaxn{} = Root, [Fun], InitLState) when is_function(Fun) ->
    [#op{root = Root, 
         lstate = InitLState, 
         code = {iter_node, wrap_user_fun(Fun), undefined}}];

prepare(#uaxl{} = Leaf, [Fun], _) when is_function(Fun) ->
    [#op{root = Leaf, code = {value_stop, wrap_user_fun(Fun)}}];

prepare(#uaxn{kids = Kids} = Root, [{Fun, Choices}], InitLState) 
  when is_function(Fun) andalso is_list(Choices) ->
    Choices1 = [{Elem, prepare(next_root(Elem, Kids), SubQ, undefined)} ||
                   {Elem, SubQ} <- Choices],
    [#op{root = Root, 
         lstate = InitLState, 
         code = {iter_node, wrap_user_fun(Fun), Choices1}}];

prepare(#uaxn{kids = Kids} = Root, [Elem | Q], InitLState) ->
    {_Id, NR} = next(Elem, Kids),
    [#op{root = Root, 
         code = {get, Elem}, 
         lstate = InitLState} | if Q == [] -> [];
                                   true -> prepare(NR, Q, InitLState)
                                end].
    

next({Elem, Id}, Kids) when is_atom(Elem) ->
    {Id, next_root({Elem}, Kids)};
next(Elem, Kids) when is_atom(Elem) ->
    {Elem, next_root(Elem, Kids)};
next(Elem, _Kids) ->
    erlang:error({path_error, Elem}).
next_root(Elem, Kids) ->
    case lists:keyfind(Elem, 2, Kids) of
        false -> erlang:error({path_error, Elem});
        Tuple -> Tuple
    end.


%% DOWN for {get, Elem} ops
eval0([#op{code = {get, Elem}} = Op | Ops], PrevOps, GState) ->
    #op{root = #uaxn{get = Get}, obj = Obj} = Op,
    Id = if is_tuple(Elem) -> element(2, Elem); true -> Elem end, 
    try Get(Id, Obj) of
        Obj1 ->
            case Ops of
                [] ->
                    eval2(PrevOps, GState);
                [#op{} = Op1 | Ops1] ->
                    Op11 = Op1#op{obj = Obj1, lstate = Op#op.lstate},
                    eval0([Op11 | Ops1], [Op | PrevOps], GState)
            end
    catch
        error:{not_found, _} ->
            eval2(PrevOps, GState);
        _:Err ->
            erlang:error(Err)
    end;

eval0([#op{code = {value_stop, Fun}} = Op | _], PrevOps, GState) ->
    #op{root = Root, lstate = LState, obj = Obj} = Op,
    case result(value_stop, Fun, Obj, LState, GState, Root) of
        {stop, GState1} -> eval2(PrevOps, GState1);
        {done, GState1} -> GState1
    end;

eval0([#op{code = {iter_node, _Fun, _Choices}} = Op], PrevOps, GState) ->
    #op{root = Root, lstate = LState, obj = Obj} = Op,
    {ok, Iter, IterState} = uaxc_iter:mk(Root, Obj),
    eval1([Op#op{lstate = LState, tmp = {Iter, IterState}} | PrevOps], GState).


eval1([#op{code = {iter_node, Fun, _Choices}, tmp = {Iter, IterState}} = Op | PrevOps], GState) ->
    #op{root = Root, lstate = LState} = Op,
    case Iter(IterState) of
        {ok, {_, Obj1} = KV, IterState1} ->
            case result(iter_node, Fun, KV, LState, GState, Root) of
                {ok, GState1} ->
                    eval1([Op#op{tmp = {Iter, IterState1}} | PrevOps], GState1);
                {stop, GState1} ->
                    eval2(PrevOps, GState1);
                {done, GState1} ->
                    GState1;
                {move, Key, LState1, GState1} ->
                    Op1 = Op#op{tmp = {Iter, IterState1}},
                    choose(Key, Op1, PrevOps, Obj1, LState1, GState1)
            end;
        done ->
            eval2(PrevOps, GState)
    end.
     
%% BACKTRACK until iter_node is found
eval2([], GState) ->
    GState;
eval2([#op{code = {get, _}} | PrevOps], GState) ->
    eval2(PrevOps, GState);
eval2([#op{code = {value_stop, _}} | PrevOps], GState) ->
    eval2(PrevOps, GState);
eval2([#op{code = {iter_node, _, _}} | _] = PrevOps, GState) ->
    eval1(PrevOps, GState).


%%%%%%%%%%
        
wrap_user_fun(F) ->
    {arity, A} = erlang:fun_info(F, arity),
    case A of
        1 -> fun (Val, _LState, _GState, _Node) -> F(Val) end;
        2 -> fun (Val, LState, _GState, _Node) -> F(Val, LState) end;
        3 -> fun (Val, LState, GState, _Node) -> F(Val, LState, GState) end;
        4 -> fun (Val, LState, GState, Node) -> F(Val, LState, GState, Node) end
    end.

call_user_fun(F, Val, LState, GState, Node) ->
    {arity, A} = erlang:fun_info(F, arity),
    case A of
        1 -> F(Val);
        2 -> F(Val, LState);
        3 -> F(Val, LState, GState);
        4 -> F(Val, LState, GState, Node)
    end.

choose(Key, #op{code = {iter_node, _, [_ | _] = Choices}} = Op,
       PrevOps, Obj, LState, GState) ->
    case lists:keyfind(Key, 1, Choices) of
        {_, []} ->
            eval1([Op | PrevOps], GState);
        {_, [ChOp1 | ChOps1]} ->
            ChOps2 = [ChOp1#op{obj = Obj, lstate = LState} | ChOps1],
            eval0(ChOps2, [Op | PrevOps], GState);
        false ->
            erlang:error({bad_choice, Key})
    end;
choose(_Key, #op{code = {iter_node, _, BadChoices}}, 
       _PrevOps, _Obj, _LState, _GState) ->
    erlang:error({bad_choices, BadChoices}).


result(iter_node, F, KV, LState, GState, Root) ->
    Result = F(KV, LState, GState, Root),
    case Result of
        ok -> 
            {ok, GState};
        {ok, GState1} -> 
            {ok, GState1};
        {move, Elem} -> 
            {move, Elem, LState, GState};
        {move, Elem, LState1} -> 
            {move, Elem, LState1, GState};
        {move, Elem, LState1, GState1} -> 
            {move, Elem, LState1, GState1};
        stop ->
            {stop, GState};
        {stop, GState1} ->
            {stop, GState1};
        done ->
            {done, GState};
        {done, GState1} ->
            {done, GState1};
        _ ->
            erlang:error({bad_result, Result})
    end;

result(value_stop, F, Val, LState, GState, Root) ->
    Result = F(Val, LState, GState, Root),
    case Result of
        ok              -> {stop, GState};
        {ok, GState1}   -> {stop, GState1};
        stop            -> {stop, GState};
        {stop, GState1} -> {stop, GState1};
        done            -> {done, GState};
        {done, GState1} -> {done, GState1};
        _               -> erlang:error({bad_result, Result})
    end.
            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(EUNIT, true).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
    
basic_test_() ->
    Schema = {{tuple, fun (g) -> 1 end},
              [{g, {gb_tree,
                    [{{v}, {{tuple, [{key, fun (attr) -> 1; (e_out) -> 2 end}]},
                            [attr, 
                             {e_out, {gb_tree,
                                      [{{e}, {{tuple, [{key, fun (tgt) -> 1; (attr) -> 2 end}]},
                                              [tgt,
                                               attr]}}]}}]}}]}}]},

    #uax{} = X = uax:mk(Schema),
        
    Obj = uax:new(X, [{g, [{{v},
                            [{<<"v-1">>,
                              [{attr, <<"v-attr-1">>},
                               {e_out, [{{e},
                                         [{<<"e-out-1">>,
                                           [{tgt, <<"v-2">>},
                                            {attr, <<"e-out-attr-1">>}]},
                                          {<<"e-out-2">>,
                                           [{tgt, <<"v-1">>},
                                            {attr, <<"e-out-attr-2">>}]}]}]}]},
                             {<<"v-2">>,
                              [{attr, <<"v-attr-2">>},
                               {e_out, [{{e},
                                         [{<<"e-out-3">>,
                                           [{tgt, <<"v-1">>},
                                            {attr, <<"e-out-attr-3">>}]}]}]}]}]}]}]),
    
    [%% collecting vertex ids
     ?_assertEqual(
        [[<<"v-2">>],[<<"v-1">>]],
        uax:fold(X, [g, fun ({K, _V}, LS, GS) -> 
                                {ok, [[K | LS] | GS]} %% OK result -> goes to next sibling
                        end],
                 {[], []}, Obj)),
     
     %% collecting phys keys (tuple indices) of all properties (e_out and attr) of all vertices
     ?_assertEqual(
        [2, 1, 2, 1],
        uax:fold(X, [g, {fun (_) -> {move, {v}} end, %% MOVE result -> goes down to the child
                         [{{v}, [fun ({K, _}, _LS, GS) -> {ok, [K | GS]} end]}]}], %% iterating over vertex props
                 {none, []}, Obj)),
        
     %% [<<"v-attr-2">>,<<"v-attr-1">>]
     %% uax:fold(X, [g, {fun ({K, V}, LS, GS) -> {move, {v}} end, [{{v}, [attr, fun (Val, _LS, GS) -> {ok, [Val | GS]} end]}]}], {[], []}, Obj).

     %% collecting attributes of all vertices
     ?_assertEqual(
        [<<"v-attr-2">>, <<"v-attr-1">>],
        uax:fold(X, [g, {fun (_) -> {move, {v}} end,
                         [{{v}, [attr, fun (Val, _LS, GS) -> {ok, [Val | GS]} end]}]}],
                 {[], []}, Obj))

    ].


-endif.

    
