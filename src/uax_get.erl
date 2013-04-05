-module(uax_get).

-compile(export_all).

%%%%%%%%%%

compile(Schema) ->
    uax_compile:c(Schema, {c_flags(leaf), fun mk_fun/3}, {c_flags(node), fun mk_fun/3}).

c_flags(leaf) -> [decode];
c_flags(node) -> [type, typecheck, key, get, decode, none_tag].


mk_fun({node, undefined, Kids}, _,
       [{type, Type}, {typecheck, Typecheck} | Flags]) ->
    Get = mk_get_fun(Flags),
    Root = {Type, Typecheck, Get},
    %% result of compilation, to use by user:
    fun (Path, Obj) when is_list(Path) -> 
            eval(Path, Obj, {root, Root, Kids})
    end;

mk_fun({node, Elem, Kids}, _, [{type, _}, {typecheck, _} | Flags]) ->
    {node, Elem, mk_get_fun(Flags), Kids};

mk_fun({leaf, Elem}, _, [{decode, Decode}]) ->
    {leaf, Elem, Decode};
mk_fun({leaf, Elem}, _, []) ->
    {leaf, Elem, fun uax_util:identity/1}.


mk_get_fun([{key, Key}, {get, Get}, {decode, Decode}, {none_tag, NoneTag}]) ->
    fun (Id, Obj) -> maybe_none_tag(Id, Decode(Get(Key(Id), Obj)), NoneTag) end;
%% {k,g,d} {k,g,n} {g,d,n}
mk_get_fun([{key, Key}, {get, Get}, {decode_, Decode}]) ->
    fun (Id, Obj) -> Decode(Get(Key(Id), Obj)) end;
mk_get_fun([{key, Key}, {get, Get}, {none_tag, NoneTag}]) ->
    fun (Id, Obj) -> maybe_none_tag(Id, Get(Key(Id), Obj), NoneTag) end;
mk_get_fun([{get, Get}, {decode, Decode}, {none_tag, NoneTag}]) ->
    fun (Id, Obj) -> maybe_none_tag(Id, Decode(Get(Id, Obj)), NoneTag) end;
%% {k,g} {g,d} {g,n}
mk_get_fun([{key, Key}, {get, Get}]) ->
    fun (Id, Obj) -> Get(Key(Id), Obj) end;
mk_get_fun([{get, Get}, {decode, Decode}]) ->
    fun (Id, Obj) -> Decode(Get(Id, Obj)) end;
mk_get_fun([{get, Get}, {none_tag, NoneTag}]) ->
    fun (Id, Obj) -> maybe_none_tag(Id, Get(Id, Obj), NoneTag) end;
%% {g}
mk_get_fun([{get, Get}]) ->
    Get.


maybe_none_tag(Id, Val, NoneTag) ->
    (Val =:= NoneTag andalso erlang:error({not_found, Id})) orelse Val.

%%%%%%%%%%

eval([], Obj, {root, {Type, Typecheck, _Get}, _Kids}) ->
    case Typecheck(Obj) of
        true -> Obj;
        false -> erlang:error({type_error, Type, Obj})
    end;

eval(Ps, Obj, {root, {Type, Typecheck, Get}, Kids}) ->
    case Typecheck(Obj) of
        true -> eval(Ps, Obj, {node, root, Get, Kids});
        false -> erlang:error({type_error, Type, Obj})
    end;

eval([P | Ps], Obj, {node, _, Get, Kids}) ->
    case {Ps, id_child(P, Kids)} of
        {[], {Id, {node, _, _, _}}} ->
            Get(Id, Obj);
        {_, {Id, {node, _, _, _} = Next}} ->
            eval(Ps, Get(Id, Obj), Next);
        {[], {Id, {leaf, _, LeafDecode}}} ->
            LeafDecode(Get(Id, Obj));
        {_, {_Id, _}} ->
            erlang:error({path_error, P})
    end.


id_child({Elem, Id}, Kids) when is_atom(Elem) ->
    {Id, lists:keyfind({Elem}, 2, Kids)};
id_child(Elem, Kids) when is_atom(Elem) ->
    {Elem, lists:keyfind(Elem, 2, Kids)};
id_child(Elem, _Kids) ->
    erlang:error({path_error, Elem}).


try_get(F, Id, Obj) ->
    try F(Id, Obj) of
        Val -> Val
    catch
        _:_ -> erlang:error({not_found, Id})
    end.

