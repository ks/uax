-module(uaximpl_list).

-behaviour(uaximpl).

-export([opts/0, args/1, new/1, get/1, put/1, del/1, typecheck/1]).


opts() ->
    {[{key, fun (F) -> is_function(F, 1) end}],
     [{none_tag, fun (_) -> true end, undefined}],
     [{encode, fun (F) -> is_function(F, 1) end},
      {decode, fun (F) -> is_function(F, 1) end}]}.


args(put) -> [none_tag];
args(del) -> [none_tag];
args(_) -> [].


get([]) ->
    fun (Pos, List) -> uax_get:try_get(fun lists:nth/2, Pos, List) end.

put([{none_tag, NoneTag}]) ->
    fun (Idx, Val, List) -> do_put_default(Idx, Val, List, NoneTag) end.

new([]) ->
    fun () -> [] end.

del([{none_tag, NoneTag}]) ->
    fun (Idx, List) -> do_del_default(Idx, List, NoneTag) end.

typecheck([]) ->
    fun erlang:is_list/1.


%%

do_put_default(Idx, Val, List, NoneTag) ->
    (is_integer(Idx) andalso Idx > 0) orelse erlang:error(badarg),
    {RemXs, PrevReversedXs} =
        uax_util:fold_range(
          fun (_, {[X | Xs], Acc}) -> {Xs, [X | Acc]};
              (_, {[], Acc}) -> {[], [NoneTag | Acc]}
          end, {List, []}, 1, Idx),
    lists:concat([lists:reverse(PrevReversedXs), [Val], RemXs]).


do_del_default(Idx, List, NoneTag) ->
    (is_integer(Idx) andalso Idx > 0) orelse erlang:error(badarg),
    do_put_default(Idx, NoneTag, List, NoneTag).


