-module(uaxc_del).

-behaviour(uaxc).

-export([compile_keys/0, c/2]).
-export([eval/3, eval0/3]).

-include("uax.hrl").

%%%%%%%%%%

compile_keys() -> [del, key].
    
c(_, [{del, Del}, {key, Key}]) ->
    fun (Id, Obj) -> Del(Key(Id), Obj) end;
c(_, [{del, Del}]) ->
    fun (Id, Obj) -> Del(Id, Obj) end.

%%%%%%%%%%

eval(#uaxn{type = Type, typecheck = Typecheck, new = New}, [], Obj) ->
    case Typecheck(Obj) of
        true -> New();
        false -> erlang:error({type_error, Type, Obj})
    end;

eval(#uaxn{type = Type, typecheck = Typecheck} = X, Ps, Obj) ->
    case Typecheck(Obj) of
        true -> eval0(X, Ps, Obj);
        false -> erlang:error({type_error, Type, Obj})
    end.

eval0(#uaxn{get = Get, put = Put, del = Del, kids = Kids}, [P | Ps], Obj) ->
    case {Ps, uax_util:path_next(P, Kids)} of
        {[], {Id, _}} ->
            Del(Id, Obj);
        {_, {Id, #uaxn{} = Next}} ->
            Obj1 = Get(Id, Obj),
            RObj = eval0(Next, Ps, Obj1),
            Put(Id, RObj, Obj);
        {_, {_Id, _}} ->
            erlang:error({path_error, P})
    end.
