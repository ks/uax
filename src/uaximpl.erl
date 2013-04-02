-module(uaximpl).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{opts, 0},
     {args, 1},
     {new, 1}, 
     {get, 1}, 
     {put, 1},
     {del, 1},
     {typecheck, 1}];

behaviour_info(_) ->
    undefined.
