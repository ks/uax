-module(uax_do).

-compile(export_all).

%%%%%%%%%%

compile(Schema) ->
    uax_compile:c(Schema, {c_flags(leaf), fun mk_fun/3}, {c_flags(node), fun mk_fun/3}).

c_flags(leaf) -> [encode, decode];
c_flags(node) -> [type, typecheck, key, encode, decode, none_tag, get, put, new, del].


mk_fun({node, undefined, Kids}, _, Flags) ->
    Node = mk_node(Flags),
    %% result of compilation, to use by user:
    fun (PathCode, Obj, State) when is_list(PathCode) ->
            eval(PathCode, Obj, State, {root, Node, Kids})
    end;

mk_fun({node, Elem, Kids}, _, Flags) ->
    {node, Elem, mk_node(Flags), Kids};

mk_fun({leaf, Elem}, _, Flags) ->
    I = fun uax_util:identity/1,
    EncDec = [proplists:get_value(X, Flags, I) || X <- [encode, decode]],
    {leaf, Elem, list_to_tuple(EncDec)}.


mk_node([{type, Type}, {typecheck, Typecheck} | _] = Flags) ->
    {Type, Typecheck, 
     uax_new:mk_new_fun(Flags),
     uax_get:mk_get_fun(Flags), 
     uax_put:mk_put_fun(Flags), 
     uax_del:mk_del_fun(Flags)}.
     

%%%%%%%%%%

eval([], Obj, State, {root, {Type, Typecheck, _, _, _, _}, _Kids}) ->
    case Typecheck(Obj) of
        true -> {Obj, State};
        false -> erlang:error({type_error, Type, Obj})
    end;

eval(PathCode, Obj, State, {root, {Type, Typecheck, _, _, _, _} = N, Kids}) ->
    case Typecheck(Obj) of
        true -> eval1(PathCode, Obj, State, {node, undefined, N, Kids});
        false -> erlang:error({type_error, Type, Obj})
    end.


eval1([PC | PCs], Obj, State, {node, _NodeKey, {_, _, _, Get, _, _}, Kids} = Node) ->
    io:format("////////// PC = ~p (~p)~n", [PC, State]),

    case {PCs, dispatch(PC, Kids)} of
        {[], {path, {Id, {node, _, _, _}}}} ->
            Get(Id, Obj);
        {_, {path, {Id, {node, _, _, _} = Next}}} ->
            eval1(PCs, Get(Id, Obj), State, Next);
        {[], {path, {Id, {leaf, _, Decode}}}} ->
            Decode(Get(Id, Obj));
        {[], {code, {Resolution, Verb, Fun}}} ->
            io:format("////////// CODE = ~p ~p ~p~n", [Resolution, Verb, Fun]),
            code({Resolution, Verb, Fun}, Obj, State, Node);
        {_, _} ->
            erlang:error({path_error, PC})
    end.



dispatch(F, _Kids) when is_function(F) ->
    {code, {elem, get, F}};
dispatch({Resolution, Verb, F}, _Kids) ->
    {code, {Resolution, Verb, F}};
dispatch({Elem, Id}, Kids) when is_atom(Elem) ->
    {path, {Id, lists:keyfind({Elem}, 2, Kids)}};
dispatch(Elem, Kids) when is_atom(Elem) ->
    {path, {Elem, lists:keyfind(Elem, 2, Kids)}};
dispatch(Elem, _Kids) ->
    erlang:error({path_error, Elem}).


code({elem, get, F}, Obj, State, Node) ->
    todo.


%%%%%%%%%%


schema1() ->
    ESchema = {{tuple, fun (out) -> 1 end},
               [{out, {gb_tree,
                       [{{id}, {{tuple, fun (tgt) -> 1; (attr) -> 2 end},
                                [tgt, attr]}}]}}]},
    VSchema = {gb_tree,
               [{{id}, {{tuple, fun (attr) -> 1; (e) -> 2 end},
                        [attr, {e, ESchema}]}}]},
    {{tuple, fun (v) -> 1 end}, [{v, VSchema}]}.


schema2() ->
    ESchema = {gb_tree,
               [{{id}, {{tuple, fun (tgt) -> 1; (attr) -> 2 end},
                        [tgt, attr]}}]},
    VSchema = {gb_tree,
               [{{id}, {{tuple, fun (attr) -> 1; (e_out) -> 2 end},
                        [attr, {e_out, ESchema}]}}]},
    {{tuple, fun (v) -> 1 end}, [{v, VSchema}]}.



t(1) ->
    Schema = schema1(),
    New = uax:mk(new, Schema),
    _Do = uax:mk(do, Schema),
    _Obj = New([{v, [{{id}, [{<<"root">>, [{attr, <<"v-attr">>}, 
                                           {e, []}]}]}]}]);



t(2) ->
    Schema = schema2(),
    New = uax:mk(new, Schema),
    Do = uax:mk(do, Schema),
    Obj = New([{v, [{{id}, [{<<"root">>, [{attr, <<"v-attr">>}, {e_out, []}]}]}]}]),
    

    Do([v, {id, <<"root">>}, e_out, fun (_Key, Val, _State) -> {ok, Val} end], Obj, []).

