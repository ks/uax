-module(uaxc_get).

-behaviour(uaxc).

-export([compile_keys/0, c/2]).
-export([eval/3, eval0/3]).

-include("uax.hrl").

%%%%%%%%%%

compile_keys() -> [key, get, decode, none_tag].

c(_, [{key, Key}, {get, Get}, {decode, Decode}, {none_tag, NoneTag}]) ->
    fun (Id, Obj) -> maybe_none_tag(Id, Decode(Get(Key(Id), Obj)), NoneTag) end;
%% {k,g,d} {k,g,n} {g,d,n}
c(_, [{key, Key}, {get, Get}, {decode_, Decode}]) ->
    fun (Id, Obj) -> Decode(Get(Key(Id), Obj)) end;
c(_, [{key, Key}, {get, Get}, {none_tag, NoneTag}]) ->
    fun (Id, Obj) -> maybe_none_tag(Id, Get(Key(Id), Obj), NoneTag) end;
c(_, [{get, Get}, {decode, Decode}, {none_tag, NoneTag}]) ->
    fun (Id, Obj) -> maybe_none_tag(Id, Decode(Get(Id, Obj)), NoneTag) end;
%% {k,g} {g,d} {g,n}
c(_, [{key, Key}, {get, Get}]) ->
    fun (Id, Obj) -> Get(Key(Id), Obj) end;
c(_, [{get, Get}, {decode, Decode}]) ->
    fun (Id, Obj) -> Decode(Get(Id, Obj)) end;
c(_, [{get, Get}, {none_tag, NoneTag}]) ->
    fun (Id, Obj) -> maybe_none_tag(Id, Get(Id, Obj), NoneTag) end;
%% {g}
c(_, [{get, Get}]) ->
    Get.

maybe_none_tag(Id, Val, NoneTag) ->
    (Val =:= NoneTag andalso erlang:error({not_found, Id})) orelse Val.

%%%%%%%%%%

eval([], Obj, #uaxn{type = Type, typecheck = Typecheck}) ->
    case Typecheck(Obj) of
        true -> Obj;
        false -> erlang:error({type_error, Type, Obj})
    end;

eval(Ps, Obj, #uaxn{type = Type, typecheck = Typecheck} = X) ->
    case Typecheck(Obj) of
        true -> eval0(Ps, Obj, X);
        false -> erlang:error({type_error, Type, Obj})
    end.

eval0([P | Ps], Obj, #uaxn{get = Get, kids = Kids}) ->
    case {Ps, uax_util:path_next(P, Kids)} of
        {[], {Id, #uaxn{}}} ->
            Get(Id, Obj);
        {_, {Id, #uaxn{} = Next}} ->
            eval0(Ps, Get(Id, Obj), Next);
        {[], {Id, #uaxl{decode = Decode}}} ->
            Decode(Get(Id, Obj));
        {_, {_Id, _}} ->
            erlang:error({path_error, P})
    end.
