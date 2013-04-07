-module(uaxc).

-include("uax.hrl").

-callback compile_keys() -> [atom()].
-callback c(#uaxn{}, CompileKVs :: [{atom(), any()}]) -> #uaxn{}.

%% -compile(export_all).

-export([compile/1, c/2, 
         env/1, sub_env/2, 
         ops/0, op_mod/1, 
         types/0, type_mod/1]).

%%%%%%%%%%

compile(Schema) ->
    uax_util:maptree(fun parse/2, fun construct/2, Schema).

%% nodes
parse({{Type, Opt}, Kids}, []) ->
    Env = env({node, Type, Opt}),
    N = #uaxn{mode = single, env = Env, type = Type, kids = Kids},
    {ok, N, Kids};
parse({Type, Kids}, []) ->
    Env = env({node, Type, undefined}),
    N = #uaxn{mode = single, env = Env, type = Type, kids = Kids},
    {ok, N, Kids};
parse({Key, {{Type, Opt}, Kids}}, [_ | _]) when is_atom(Key), Key /= undefined ->
    Env = env({node, Type, Opt}),
    N = #uaxn{id = Key, mode = single, env = Env, type = Type, kids = Kids},
    {ok, N, Kids};
parse({{Key}, {{Type, Opt}, Kids}}, [_ | _]) when is_atom(Key), Key /= undefined ->
    Env = env({node, Type, Opt}),
    N = #uaxn{id = {Key}, mode = multi, env = Env, type = Type, kids = Kids},
    {ok, N, Kids};
parse({Key, {Type, Kids}}, [_ | _]) when is_atom(Key), Key /= undefined ->
    Env = env({node, Type, undefined}),
    N = #uaxn{id = Key, mode = single, env = Env, type = Type, kids = Kids},
    {ok, N, Kids};
parse({{Key}, {Type, Kids}}, [_ | _]) when is_atom(Key), Key /= undefined ->
    Env = env({node, Type, undefined}),
    N = #uaxn{id = {Key}, mode = multi, env = Env, type = Type, kids = Kids},
    {ok, N, Kids};

%% leaves
parse({Key, Opt}, [_ | _]) when is_atom(Key), Key /= undefined ->
    {ok, #uaxl{id = Key, mode = single, env = env({leaf, Opt})}};
parse({{Key}, Opt}, [_ | _]) when is_atom(Key), Key /= undefined ->
    {ok, #uaxl{id = {Key}, mode = multi, env = env({leaf, Opt})}};
parse(Key, [_ | _]) when is_atom(Key), Key /= undefined ->
    {ok, #uaxl{id = Key, mode = single, env = env({leaf, undefined})}};
parse({Key}, [_ | _]) when is_atom(Key), Key /= undefined ->
    {ok, #uaxl{id = {Key}, mode = multi, env = env({leaf, undefined})}};

parse(BadSchema, _Prev) ->
    erlang:error({schema_error, BadSchema}).

%% %%

construct({node, #uaxn{env = E} = X, Kids}, _Prev) ->
    Typecheck = proplists:get_value(typecheck, E),
    [New, Get, Put, Del] = [c(Op, X) || Op <- ops()],
    X#uaxn{kids = Kids, 
           typecheck = Typecheck,
           new = New, get = Get, put = Put, del = Del};

construct({leaf, #uaxl{env = E} = X}, _Prev) ->
    I = fun uax_util:identity/1,
    Ops = [encode, decode],
    [Enc, Dec] = [proplists:get_value(Op, E, I) || Op <- Ops],
    X#uaxl{encode = Enc, decode = Dec}.


c(Op, #uaxn{} = X) -> (op_mod(Op)):c(X, sub_env(Op, X)).

sub_env(Op, #uaxn{env = Env}) ->
    uax_util:keyselect((op_mod(Op)):compile_keys(), Env).


ops() -> [new, get, put, del].

op_mod(Op) when is_atom(Op) ->
    list_to_existing_atom("uaxc_" ++ atom_to_list(Op)).


types() ->
    [uaximpl_array, uaximpl_dict, uaximpl_gb_tree, uaximpl_list,
     uaximpl_orddict, uaximpl_proplist, uaximpl_tuple, uaximpl_kvlist].

type_mod(Type) when is_atom(Type) ->
    list_to_existing_atom("uaximpl_" ++ atom_to_list(Type)).

%%

env({leaf, undefined}) ->
    [];
env({leaf, Fun}) when is_function(Fun, 1) ->
    [{decode, Fun}];
env({leaf, Opts}) when is_list(Opts) ->
    OK = ensure_known_opts([encode, decode], Opts),
    Check = fun (F) -> is_function(F, 1) end,
    check_opts(OK, [{encode, Check}, {decode, Check}], fun (_, Env) -> Env end);

env({node, Type, Opt}) when not is_list(Opt) ->
    env({node, Type, if is_function(Opt, 1) -> [{key, Opt}];
                           Opt == undefined -> [];
                           true -> erlang:error({schema_error, {invalid, Opt}})
                        end});
env({node, Type, Opts}) when is_list(Opts) ->
    Mod = type_mod(Type),
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
             Args = uax_util:keyselect(ArgKeys, Env0),
             case proplists:get_value(F, Opts1) of
                 undefined -> 
                     {F, Mod:F(Args)};
                 Fun when is_function(Fun, 1) ->
                     {F, Fun(Args)}
             end
         end || F <- [typecheck | ops()]].


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


