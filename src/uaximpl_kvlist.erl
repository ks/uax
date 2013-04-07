-module(uaximpl_kvlist).

-behaviour(uaximpl).

-export([opts/0, args/1, new/1, get/1, put/1, del/1, typecheck/1]).

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


put([{keypos, _Keypos}, {fill_tag, _FillTag}]) ->
    fun (_Key, Val, KVlist) -> [Val | KVlist] end;
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


typecheck([]) ->
    fun erlang:is_list/1.
