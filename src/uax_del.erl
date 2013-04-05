-module(uax_del).

-compile(export_all).

%%%%%%%%%%

compile(Schema) ->
    uax_compile:c(Schema, {c_flags(leaf), fun mk_fun/3}, {c_flags(node), fun mk_fun/3}).

c_flags(leaf) -> [encode, decode];
c_flags(node) -> [type, typecheck, key, get, decode, encode, none_tag, put, new, del].


mk_fun({node, undefined, Kids}, _, Flags) ->
    Node = mk_node(Flags),
    %% result of compilation, to use by user:
    fun (Path, Obj) when is_list(Path) -> 
            eval(Path, Obj, {root, Node, Kids})
    end;

mk_fun({node, Elem, Kids}, _, Flags) ->
    {node, Elem, mk_node(Flags), Kids};

mk_fun({leaf, Elem}, _, Flags) ->
    I = fun uax_util:identity/1,
    EncDec = [proplists:get_value(X, Flags, I) || X <- [encode, decode]],
    {leaf, Elem, list_to_tuple(EncDec)}.

mk_node([{type, Type}, {typecheck, Typecheck} | _] = Flags) ->
    {Type, Typecheck, 
     mk_get_fun(Flags), mk_put_fun(Flags), mk_new_fun(Flags), mk_del_fun(Flags)}.


mk_get_fun(Flags) ->
    GetFlags1 = uax_compile:select_flags(uax_get:c_flags(node), Flags),
    GetFlags2 = lists:foldl(fun proplists:delete/2, GetFlags1, [type, typecheck]),
    uax_get:mk_get_fun(GetFlags2).


mk_put_fun(Flags) ->
    mk_put_fun1(uax_compile:select_flags([put, encode], Flags)).

mk_put_fun1([{put, Put}, {encode, Encode}]) ->
    fun (Id, Val, Obj) -> Put(Id, Encode(Val), Obj) end;
mk_put_fun1([{put, Put}]) ->
    Put.

mk_new_fun(Flags) -> proplists:get_value(new, Flags).

mk_del_fun(Flags) -> proplists:get_value(del, Flags).

%%%%%%%%%%

eval([], Obj, {root, {Type, Typecheck, _FinalGet, _FinalPut, New, _Del}, _Kids}) ->
    case Typecheck(Obj) of
        true -> New();
        false -> erlang:error({type_error, Type, Obj})
    end;

eval(Ps, Obj, {root, {Type, Typecheck, _, _, _, _} = Node, Kids}) ->
    case Typecheck(Obj) of
        true -> eval(Ps, Obj, {node, root, Node, Kids});
        false -> erlang:error({type_error, Type, Obj})
    end;

eval([P | Ps], Obj, {node, _, {_Type, _Typecheck, FinalGet, FinalPut, _New, Del} = _Flags, Kids}) ->
    case {Ps, id_child(P, Kids)} of
        {[], {Id, _}} ->
            Del(Id, Obj);
        {_, {Id, {node, _, _, _} = Next}} ->
            Obj1 = FinalGet(Id, Obj),
            RObj = eval(Ps, Obj1, Next),
            FinalPut(Id, RObj, Obj);
        {_, {_Id, _}} ->
            erlang:error({path_error, P})
    end.


do_get_or_new(Id, Obj, Get, New) ->
    try 
        Get(Id, Obj)
    catch
        error:{not_found, _} -> New()
    end.

do_put(Id, Val, Obj, {_Type, _Typecheck, FinalGet, FinalPut, New}) ->
    FinalPut(Id, Val, do_get_or_new(Id, Obj, FinalGet, New)).


id_child({Elem, Id}, Kids) when is_atom(Elem) ->
    {Id, lists:keyfind({Elem}, 2, Kids)};
id_child(Elem, Kids) when is_atom(Elem) ->
    {Elem, lists:keyfind(Elem, 2, Kids)};
id_child(Elem, _Kids) ->
    erlang:error({path_error, Elem}).