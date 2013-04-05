-module(uax_compile).

-compile(export_all).


c(Schema, NodeComp, LeafComp) ->
    uax_util:maptree(fun mk_node_env/2,
                     fun (Node, Prev) -> mk_node_fun(Node, Prev, NodeComp, LeafComp) end,
                     Schema).

c_env(Schema) ->
    uax_util:maptree(fun mk_node_env/2, fun (Node, _Prev) -> Node end, Schema).

operations() -> [new, get, put, del, typecheck].
     
%%

%% nodes
mk_node_env({{Type, Opt}, Kids}, []) ->
    {ok, {undefined, mk_env({node, Type, Opt})}, Kids};
mk_node_env({Type, Kids}, []) ->
    {ok, {undefined, mk_env({node, Type, undefined})}, Kids};
mk_node_env({Key, {{Type, Opt}, Kids}}, [_ | _]) when is_atom(Key) ->
    {ok, {Key, mk_env({node, Type, Opt})}, Kids};
mk_node_env({{Key}, {{Type, Opt}, Kids}}, [_ | _]) when is_atom(Key) ->
    {ok, {{Key}, mk_env({node, Type, Opt})}, Kids};
mk_node_env({Key, {Type, Kids}}, [_ | _]) when is_atom(Key) ->
    {ok, {Key, mk_env({node, Type, undefined})}, Kids};
mk_node_env({{Key}, {Type, Kids}}, [_ | _]) when is_atom(Key) ->
    {ok, {{Key}, mk_env({node, Type, undefined})}, Kids};

%% leaves
mk_node_env({Key, Opt}, [_ | _]) when is_atom(Key) ->
    {ok, {Key, mk_env({leaf, Opt})}};
mk_node_env({{Key}, Opt}, [_ | _]) when is_atom(Key) ->
    {ok, {{Key}, mk_env({leaf, Opt})}};
mk_node_env(Key, [_ | _]) when is_atom(Key) ->
    {ok, {Key, mk_env({leaf, undefined})}};
mk_node_env({Key}, [_ | _]) when is_atom(Key) ->
    {ok, {{Key}, mk_env({leaf, undefined})}};

mk_node_env(BadSchema, _Prev) ->
    erlang:error({schema_error, BadSchema}).

%%

mk_node_fun({leaf, {Elem, Env}}, Prev, {Flags, CompFun}, _NodeComp) ->
    CompFun({leaf, Elem}, Prev, select_flags(Flags, Env));

mk_node_fun({node, {Elem, Env}, Kids}, Prev, _LeafComp, {Flags, CompFun}) ->
    CompFun({node, Elem, Kids}, Prev, select_flags(Flags, Env)).


select_flags(Flags, Env) ->
    lists:foldr(
      fun (Flag, Fs) ->
              case lists:keyfind(Flag, 1, Env) of
                  {Flag, V} -> [{Flag, V} | Fs];
                  false -> Fs
              end
      end, [], Flags).


%%

mk_env({leaf, undefined}) ->
    [];
mk_env({leaf, Fun}) when is_function(Fun, 1) ->
    [{decode, Fun}];
mk_env({leaf, Opts}) when is_list(Opts) ->
    OK = ensure_known_opts([encode, decode], Opts),
    Check = fun (F) -> is_function(F, 1) end,
    check_opts(OK, [{encode, Check}, {decode, Check}], fun (_, Env) -> Env end);

mk_env({node, Type, Opt}) when not is_list(Opt) ->
    mk_env({node, Type, if is_function(Opt, 1) -> [{key, Opt}];
                           Opt == undefined -> [];
                           true -> erlang:error({schema_error, {invalid, Opt}})
                        end});
mk_env({node, Type, Opts}) when is_list(Opts) ->
    Mod = uax:impl_mod(Type),
    MValidators = [_, _, _] = tuple_to_list(Mod:opts()),
    Known = lists:concat([[element(1, T) || T <- Vs] || Vs <- MValidators]),
    Opts1 = ensure_known_opts(Known, Opts),
    MOpts = [[O || {K, _} = O <- Opts1, proplists:is_defined(K, Validators)] ||
                Validators <- MValidators],
    NoRequired = fun ({K, _}, _) -> erlang:error({schema_error, {missing, K}}) end,
    NoSupplied = fun ({K, _, Default}, Env) -> [{K, Default} | Env] end,
    NoOptional = fun (_, Env) -> Env end,
    FailFuns = [NoRequired, NoSupplied, NoOptional],
    Env0 = [{type, Type}] ++
        lists:concat([check_opts(SOpts, Validators, FailFun) || 
                         {SOpts, Validators, FailFun} <-
                             lists:zip3(MOpts, MValidators, FailFuns)]),
    Env0 ++
        [begin
             ArgKeys = Mod:args(F),
             Args = select_flags(ArgKeys, Env0),
             case proplists:get_value(F, Opts1) of
                 undefined -> 
                     {F, Mod:F(Args)};
                 Fun when is_function(Fun, 1) ->
                     {F, Fun(Args)}
             end
         end || F <- operations()].


ensure_known_opts(Known, Opts) ->
    {OK, Unk} = 
        lists:partition(
          fun ({K, _}) -> lists:member(K, Known); (_) -> false end, Opts),
    Unk == [] orelse erlang:error({schema_error, {unknown, Unk}}),
    OK.


check_opts(Opts, Validators, Fail) ->
    lists:foldl(
      fun (Tup, Env) ->
              [K, Validator | _] = tuple_to_list(Tup),
              case lists:keyfind(K, 1, Opts) of
                  false ->
                      Fail(Tup, Env);
                  {K, V} ->
                      case Validator(V) of
                          true -> 
                              [{K, V} | Env];
                          false -> 
                              erlang:error({schema_error, {invalid, K, V}})
                      end
              end
      end, [], Validators).
