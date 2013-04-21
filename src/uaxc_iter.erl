-module(uaxc_iter).

-behaviour(uaxc).

-export([compile_keys/0, c/2]).

-export([eval/4, mk/2]).

-include("uax.hrl").

%%%%%%%%%%

%% This is for a compilation of Iter over particular #uaxn{}, not a generic loop over whole schema

compile_keys() -> [iter, decode, none_tag].

check(Val, NoneTag) ->
    if Val == NoneTag -> skip;
       true -> {ok, Val}
    end.

decode_check(Val, Decode, NoneTag) -> 
    case Decode(Val) of
        NoneTag -> skip;
        Val1 -> {ok, Val1}
    end.

key_fun(#uaxn{}) -> fun (PhysKey) -> PhysKey end.


c(#uaxn{type = Type} = X, Env) ->
    c1(X, case lists:member(none_tag, (uaxc:type_mod(Type)):args(iter)) of
              true  -> proplists:delete(none_tag, Env); %% backend impl is handling none_tag, so we don't have to
              false -> Env
          end).

c1(#uaxn{} = X, [{iter, Iter}]) ->
    {Iter, key_fun(X), fun (RawVal) -> {ok, RawVal} end};
c1(#uaxn{} = X, [{iter, Iter}, {decode, Decode}]) ->
    {Iter, key_fun(X), fun (RawVal) -> {ok, Decode(RawVal)} end};
c1(#uaxn{} = X, [{iter, Iter}, {none_tag, NoneTag}]) ->
    {Iter, key_fun(X), fun (RawVal) -> check(RawVal, NoneTag) end};
c1(#uaxn{} = X, [{iter, Iter}, {decode, Decode}, {none_tag, NoneTag}]) ->
    {Iter, key_fun(X), fun (RawVal) -> decode_check(RawVal, Decode, NoneTag) end}.

%% One-shot operations like get, put, new, del don't have a continuation state,
%% but Iter over elements in particular layer needs evaluator using {Iter, KeyFun, ValFun} to drive
%% the iteration process.
eval(#uaxn{iter = {Iter, KeyFun, ValFun}}, Fun, State, Obj) ->
    {arity, A} = erlang:fun_info(Fun, arity),
    Fun1 = case A of
               1 -> fun (K, _V, _State) -> Fun(K) end;
               2 -> fun (K, V, _State) -> Fun(K, V) end;
               3 -> Fun
           end,
    {ok, IterState} = Iter(init, Obj),
    eval0(Iter, IterState, KeyFun, ValFun, Fun1, State).
   
eval0(Iter, IterState, KeyFun, ValFun, Fun, State) ->
    case Iter(next, IterState) of
        {ok, {PhysKey, RawVal}, IterState1} ->
            case ValFun(RawVal) of
                {ok, Val} ->
                    case Fun(KeyFun(PhysKey), Val, State) of
                        ok             -> eval0(Iter, IterState1, KeyFun, ValFun, Fun, State);
                        {ok, State1}   -> eval0(Iter, IterState1, KeyFun, ValFun, Fun, State1);
                        {stop, State1} -> State1;
                        stop           -> State;
                        Unk            -> erlang:error({iter_callback_result, Unk})
                    end;
                skip ->
                    eval0(Iter, IterState1, KeyFun, ValFun, Fun, State)
            end;
        done ->
            State
    end.


%% alternative - returns higher-level, more user friendly iterator wrapping key fun, val fun:
mk(#uax{root = Root}, Obj) ->
    mk(Root, Obj);
mk(#uaxn{iter = {Iter, KeyFun, ValFun}}, Obj) ->
    {ok, IterState0} = Iter(init, Obj),
    {ok, fun (IterState) -> iterate(Iter, IterState, KeyFun, ValFun) end, IterState0}.

iterate(Iter, IterState, KeyFun, ValFun) ->
    case Iter(next, IterState) of
        {ok, {PhysKey, RawVal}, IterState1} ->
            case ValFun(RawVal) of
                {ok, Val} -> {ok, {KeyFun(PhysKey), Val}, IterState1};
                skip -> iterate(Iter, IterState1, KeyFun, ValFun)
            end;
        done ->
            done
    end.

