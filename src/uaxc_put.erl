-module(uaxc_put).

-behaviour(uaxc).

-export([compile_keys/0, c/2]).
-export([eval/4, eval0/4]).

-include("uax.hrl").

%%%%%%%%%%

compile_keys() -> [put, key, encode].

c(_, [{put, Put}, {key, Key}, {encode, Encode}]) ->
    fun (Id, Val, Obj) -> Put(Key(Id), Encode(Val), Obj) end;
c(_, [{put, Put}, {key, Key}]) ->
    fun (Id, Val, Obj) -> Put(Key(Id), Val, Obj) end;
c(_, [{put, Put}, {encode, Encode}]) ->
    fun (Id, Val, Obj) -> Put(Id, Encode(Val), Obj) end;
c(_, [{put, Put}]) ->
    Put.

%%%%%%%%%%

eval(#uaxn{type = Type, typecheck = Typecheck}, [], Val, Obj) ->
    case Typecheck(Obj) of
        true ->
            case Typecheck(Val) of
                true -> Val;
                false -> erlang:error({type_error, Type, Val})
            end;
        false ->
            erlang:error({type_error, Type, Obj})
    end;

eval(#uaxn{type = Type, typecheck = Typecheck} = X, Ps, Val, Obj) ->
    case Typecheck(Obj) of
        true -> eval0(X, Ps, Val, Obj);
        false -> erlang:error({type_error, Type, Obj})
    end.

eval0(#uaxn{get = Get, put = Put, new = New, kids = Kids}, [P | Ps], Val, Obj) ->
    case {Ps, uax_util:path_next(P, Kids)} of
        {[], {Id, #uaxn{type = NextType, typecheck = NextTypecheck}}} ->
            case NextTypecheck(Val) of
                true -> do_put(Id, Val, Obj, {Get, Put, New});
                false -> erlang:error({type_error, NextType, Val})
            end;
        {[], {Id, #uaxl{encode = Encode}}} ->
            Put(Id, Encode(Val), Obj);
        {_, {Id, #uaxn{} = Next}} ->
            Obj1 = do_get_or_new(Id, Obj, Get, New),
            RObj = eval0(Next, Ps, Val, Obj1),
            Put(Id, RObj, Obj);
        {_, {_Id, _}} ->
            erlang:error({path_error, P})
    end.


do_get_or_new(Id, Obj, Get, New) ->
    try 
        Get(Id, Obj)
    catch
        error:{not_found, _} -> New()
    end.

do_put(Id, Val, Obj, {Get, Put, New}) ->
    Put(Id, Val, do_get_or_new(Id, Obj, Get, New)).
