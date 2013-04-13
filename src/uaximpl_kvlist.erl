%% KVList differ from other impls, that the value it returns with default schema (without {valpos, pos_integer()})
%% returns whole tuple (with the key) as value to not lose information if tuple contains other elemens.
%% 
%% To get same behavior as other impls, use {valpos, pos_integer()} which works with a specific tuple element instead.

-module(uaximpl_kvlist). 

-behaviour(uaximpl).

-export([opts/0, args/1, new/1, get/1, put/1, del/1, typecheck/1, iter/1]).

opts() ->
    {[],
     [{keypos, fun (X) -> is_integer(X) andalso X > 0 end, 1},
      {fill_tag, fun (_) -> true end, undefined}],
     [{key, fun (F) -> is_function(F, 1) end},
      {valpos, fun (X) -> is_integer(X) andalso X > 0 end},
      {none_tag, fun (_) -> true end},
      {encode, fun (F) -> is_function(F, 1) end},
      {decode, fun (F) -> is_function(F, 1) end}]}.


args(get) -> [keypos, valpos];
args(put) -> [keypos, valpos, fill_tag];
args(del) -> [keypos];
args(iter) -> [keypos, valpos];
args(_) -> [].


get([{keypos, Keypos}]) ->
    fun (Key, KVlist) ->
            case lists:keyfind(Key, Keypos, KVlist) of
                false -> erlang:error({not_found, Key});
                Tuple -> Tuple
            end
    end;
get([{keypos, Keypos}, {valpos, Valpos}]) ->
    fun (Key, KVlist) ->
            case lists:keyfind(Key, Keypos, KVlist) of
                false -> erlang:error({not_found, Key});
                Tuple -> element(Valpos, Tuple)
            end
    end.


put([{keypos, Keypos}, {fill_tag, _FillTag}]) ->
    fun (Key, Val, KVlist) when element(Keypos, Val) =:= Key -> [Val | KVlist] end;
put([{keypos, 1}, {valpos, 2}, {fill_tag, _FillTag}]) ->
    fun (Key, Val, KVlist) -> [{Key, Val} | KVlist] end;
put([{keypos, Keypos}, {valpos, Valpos}, {fill_tag, FillTag}]) ->
    Size = max(Keypos, Valpos),
    fun (Key, Val, KVlist) ->
            [erlang:make_tuple(Size, FillTag, [{Keypos, Key}, {Valpos, Val}]) | KVlist]
    end.


new([]) ->
    fun () -> [] end.


del([{keypos, Keypos}]) ->
    fun (Key, KVlist) -> lists:keydelete(Key, Keypos, KVlist) end.


iter([{keypos, Keypos}]) ->
    fun (init, KVlist) -> {ok, KVlist};
        (next, []) -> done;
        (next, [X | Xs]) -> {ok, {element(Keypos, X), X}, Xs}
    end;
iter([{keypos, Keypos}, {valpos, Valpos}]) ->
    fun (init, KVlist) -> {ok, KVlist};
        (next, []) -> done;
        (next, [X | Xs]) -> {ok, {element(Keypos, X), element(Valpos, X)}, Xs}
    end.


typecheck([]) ->
    fun erlang:is_list/1.
