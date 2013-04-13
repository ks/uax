-module(uaximpl_gb_tree).

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
    fun (Key, Gbtree) ->
            case gb_trees:lookup(Key, Gbtree) of
                none -> erlang:error({not_found, Key});
                {value, Val} -> Val
            end
    end.

put([]) ->
    fun gb_trees:enter/3.

new([]) ->
    fun gb_trees:empty/0.

del([]) ->
    fun gb_trees:delete/2.

typecheck([]) ->
    fun ({0, nil}) -> true;
        ({C, {_K, _V, _Left, _Right}}) when is_integer(C), C > 0 -> true;
        (_) -> false
    end.

iter([]) ->
    fun (init, Gbtree) -> {ok, gb_trees:iterator(Gbtree)};
        (next, Iter) -> next(Iter)
    end.

%%

next(Iter) ->
    case gb_trees:next(Iter) of
        none -> done;
        {K, V, Iter1} -> {ok, {K, V}, Iter1}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
