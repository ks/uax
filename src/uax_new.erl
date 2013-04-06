-module(uax_new).

-compile(export_all).

%%%%%%%%%%

compile(Schema) ->
    uax_compile:c(Schema, {c_flags(leaf), fun mk_fun/3}, {c_flags(node), fun mk_fun/3}).

c_flags(leaf) -> [encode];
c_flags(node) -> [type, typecheck, key, encode, none_tag, put, new].


mk_fun({node, undefined, Kids}, _, [{type, Type}, {typecheck, Typecheck} | _] = Flags) ->
    Node = mk_node(Flags),
    %% result of compilation, to use by user:
    fun ({value, Val}) ->
            case Typecheck(Val) of
                true -> Val;
                false -> erlang:error({type_error, Type, Val})
            end;
        (PathVals) when is_list(PathVals) -> 
            eval(PathVals, {root, Node, Kids})
    end;

mk_fun({node, Elem, Kids}, _, Flags) ->
    {node, Elem, mk_node(Flags), Kids};

mk_fun({leaf, Elem}, _, Flags) ->
    {leaf, Elem, proplists:get_value(encode, Flags, fun uax_util:identity/1)}.


mk_node([{type, Type}, {typecheck, Typecheck} | _] = Flags) ->
    {Type, Typecheck, uax_put:mk_put_fun(Flags), mk_new_fun(Flags)}.

mk_new_fun(Flags) -> proplists:get_value(new, Flags).

%%%%%%%%%%

eval(PathVals, {root, {_, _, _, _} = Node, Kids}) ->
    eval(PathVals, {node, undefined, Node, Kids});

eval(PVs, {node, _, {_, _, _, New}, _} = Node) when is_list(PVs) ->
    lists:foldl(fun (X, Obj) -> eval(X, Obj, Node) end, New(), PVs);
eval({value, Val}, {node, _, {Type, Typecheck, _, _}, _}) ->
    case Typecheck(Val) of
        true -> Val;
        false -> erlang:error({type_error, Type, Val})
    end.

eval({P, V}, Obj, {node, _, {_, _, Put, _}, Kids}) ->
    case {child(P, Kids), V} of
        {false, _} ->
            erlang:error({path_error, P});
        {{leaf, {_}, Encode}, Vs1} ->
            is_list(Vs1) orelse erlang:error({value_error, Vs1}),
            lists:foldl(fun ({K1, V1}, Obj1) -> Put(K1, Encode(V1), Obj1) end, Obj, Vs1);
        {{leaf, _, Encode}, _} ->
            Put(P, Encode(V), Obj);
        {{node, _, {Type1, Typecheck1, _, _}, _}, {value, V1}} ->
            case Typecheck1(V1) of
                true -> Put(P, V1, Obj);
                false -> erlang:error({type_error, Type1, V1})
            end;
        {{node, {_}, {_, _, _, _}, _} = Next, Vs1} ->
            is_list(Vs1) orelse erlang:error({type_error, Vs1, kvlist}),
            lists:foldl(fun ({K1, V1}, Obj1) -> Put(K1, eval(V1, Next), Obj1) end, Obj, Vs1);
        {{node, _, _, _} = Next, Vs1} ->
            is_list(Vs1) orelse erlang:error({path_error, P}),
            Put(P, eval(Vs1, Next), Obj)
    end.


child({Elem}, Kids) when is_atom(Elem) ->
    lists:keyfind({Elem}, 2, Kids);
child(Elem, Kids) when is_atom(Elem) ->
    lists:keyfind(Elem, 2, Kids);
child(Elem, _Kids) ->
    erlang:error({path_error, Elem}).


%%%%%%%%%%

t() -> [t(X) || X <- [1, 2, 3, 4, 5, 6, 7]].
    
t(1) ->
    Schema = {proplist, [a, b, c]},
    New = uax:mk(new, Schema),
    [{c, 3}, {b, 2}, {a, 1}] = New([{a, 1}, {b, 2}, {c, 3}]);

t(2) ->
    Schema = {proplist, [a, {b}]},
    New = uax:mk(new, Schema),
    [{yy,3},{xx,2},{a,1}] = New([{a, 1}, {{b}, [{xx, 2}, {yy, 3}]}]);

t(3) ->
    Schema = {gb_tree, [{{a}, {proplist, [{b}]}}]},
    New = uax:mk(new, Schema),
    Obj = New([{{a}, [{xx, [{{b}, [{v1, 1}, {v2, 2}]}]},
                      {yy, [{{b}, [{v3, 3}, {v4, 4}]}]}]}]),
    Get = uax:mk(get, Schema),
    1 = Get([{a, xx}, {b, v1}], Obj),
    2 = Get([{a, xx}, {b, v2}], Obj),
    3 = Get([{a, yy}, {b, v3}], Obj),
    4 = Get([{a, yy}, {b, v4}], Obj),
    Obj;
    
t(4) ->
    Schema = {gb_tree, [{a, {proplist, [{b, {dict, [c]}}]}}]},
    New = uax:mk(new, Schema),
    Obj = New([{a, [{b, [{c, 1}]}]}]),
    Get = uax:mk(get, Schema),
    1 = Get([a, b, c], Obj);

t(5) ->
    Schema = {gb_tree, [{a, {proplist, [{b, {dict, [c]}}]}}]},
    New = uax:mk(new, Schema),
    {0, nil} = New({value, gb_trees:empty()}),
    {'EXIT', {{type_error, gb_tree, ""}, _}} = (catch New({value, ""})),
    ok;
    
t(6) ->
    Schema = {gb_tree, [{a, {proplist, [{b, {dict, [c]}}]}}]},
    New = uax:mk(new, Schema),
    Obj = New([{a, [{b, {value, dict:from_list([{c, ccc}])}}]}]),
    {'EXIT', {{type_error, dict, ""}, _}} = (catch New([{a, [{b, {value, ""}}]}])),
    Get = uax:mk(get, Schema),
    ccc = Get([a, b, c], Obj);

t(7) ->
    ESchema = {{tuple, fun (out) -> 1; (in) -> 2 end},
               [{out, {gb_tree,
                       [{{id}, {{tuple, fun (tgt) -> 1; (attr) -> 2 end},
                                [tgt, attr]}}]}}]},
    VSchema = {gb_tree,
               [{{id}, {{tuple, fun (attr) -> 1; (e) -> 2 end},
                        [attr, {e, ESchema}]}}]},
    Schema = {{tuple, fun (v) -> 1 end},
               [{v, VSchema}]},
    New = uax:mk(new, Schema),
    New([{v, [{{id}, [{<<"vtx-undefined">>, []},
                      {<<"vtx1">>, [{attr, <<"vtx1-attr">>},
                                    {e, [{out, [{{id}, [{<<"e-id1">>, 
                                                         [{tgt, <<"tgt-vertex-id">>},
                                                          {attr, <<"e-attr">>}]}]}]}]}]}]}]}]).
    
