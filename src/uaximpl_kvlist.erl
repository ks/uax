-module(uaximpl_kvlist).

-behaviour(uaximpl).

-export([opts/0, args/1, new/1, get/1, put/1, del/1, typecheck/1]).

opts() ->
    {[],
     [{keypos, fun (X) -> is_integer(X) andalso X > 0 end, 1}, 
      {valpos, fun (X) -> is_integer(X) andalso X > 0 end, 2},
      {none_tag, fun (_) -> true end, undefined}],
     [{key, fun (F) -> is_function(F, 1) end},
      {encode, fun (F) -> is_function(F, 1) end},
      {decode, fun (F) -> is_function(F, 1) end}]}.


args(get) -> [keypos, valpos];
args(put) -> [keypos, valpos, none_tag];
args(del) -> [keypos];
args(_) -> [].

     
get([{keypos, Keypos}, {valpos, Valpos}]) ->
    fun (Key, Proplist) ->
            case lists:keyfind(Key, Keypos, Proplist) of
                false -> erlang:error({not_found, Key});
                Tuple -> element(Valpos, Tuple)
            end
    end.

put([{keypos, 1}, {valpos, 2} | _]) ->
    fun (Key, Val, Proplist) -> [{Key, Val} | Proplist] end;
put([{keypos, Keypos}, {valpos, Valpos}, {none_tag, NoneTag}]) ->
    Size = max(Keypos, Valpos),
    fun (Key, Val, Proplist) ->
            [erlang:make_tuple(Size, NoneTag, [{Keypos, Key}, {Valpos, Val}]) | Proplist]
    end.

new([]) ->
    fun () -> [] end.

del([{keypos, Keypos}]) ->
    fun (Key, Proplist) -> lists:keydelete(Key, Keypos, Proplist) end.

typecheck([]) ->
    fun erlang:is_list/1.
