-module(uaximpl_proplist).

-behaviour(uaximpl).

-export([opts/0, args/1, new/1, get/1, put/1, del/1, typecheck/1]).

opts() ->
    {[],
     [],
     [{key, fun (F) -> is_function(F, 1) end},
      {encode, fun (F) -> is_function(F, 1) end},
      {decode, fun (F) -> is_function(F, 1) end},
      {none_tag, fun (_) -> true end}]}.

args(_) -> [].
     
get([]) ->
    fun (Key, Proplist) ->
            Default = make_ref(),
            case proplists:get_value(Key, Proplist, Default) of
                Default -> erlang:error({not_found, Key});
                Val -> Val
            end
    end.

put([]) ->
    fun (Key, Val, Proplist) -> [{Key, Val} | Proplist] end.

new([]) ->
    fun () -> [] end.

del([]) ->
    fun proplists:delete/2.

typecheck([]) ->
    fun erlang:is_list/1.
