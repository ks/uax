-module(uaximpl_orddict).

-behaviour(uaximpl).

-export([opts/0, args/1, new/1, get/1, put/1, del/1, typecheck/1, iter/1]).

opts() ->
    {[],
     [],
     [{key, fun (F) -> is_function(F, 1) end},
      {encode, fun (F) -> is_function(F, 1) end},
      {decode, fun (F) -> is_function(F, 1) end},
      {none_tag, fun (_) -> true end}]}.

args(_) -> [].

get([]) ->
    fun (Key, Orddict) ->
            case orddict:find(Key, Orddict) of
                {ok, Val} -> Val;
                error -> erlang:error({not_found, Key})
            end
    end.

put([]) ->
    fun orddict:store/3.

new([]) ->
    fun orddict:new/0.

del([]) ->
    fun orddict:erase/2.

typecheck([]) ->
    fun erlang:is_list/1.

iter([]) ->
    fun (init, Orddict) -> {ok, Orddict};
        (next, []) -> done;
        (next, [{K, V} | KVs]) -> {ok, {K, V}, KVs}
    end.




