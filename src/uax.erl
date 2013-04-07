%%%% Universal Accessor DSL?
%%%% 
-module(uax).

-export([mk/1, mk/2, record_key/1]).

-export([new/2, get/3, put/4, del/3]).

-include("uax.hrl").

%%%%%%%%%%

mk(Schema) ->
    Root = uaxc:compile(Schema),
    #uax{root = Root,
         new = mk_fun(new, Root),
         get = mk_fun(get, Root),
         put = mk_fun(put, Root),
         del = mk_fun(del, Root)}.

mk(new, Schema) -> mk_fun(new, uaxc:compile(Schema));
mk(get, Schema) -> mk_fun(get, uaxc:compile(Schema));
mk(put, Schema) -> mk_fun(put, uaxc:compile(Schema));
mk(del, Schema) -> mk_fun(del, uaxc:compile(Schema)).

record_key(Fields) -> 
    fun (Key) -> uax_util:position(Key, Fields) + 1 end. %% TODO: make faster version by compiling from abstract form? 

new(PathVals, Root) -> uaxc_new:eval(PathVals, Root).
get(Path, Obj, Root) -> uaxc_get:eval(Path, Obj, Root).
put(Path, Val, Obj, Root) -> uaxc_put:eval(Path, Val, Obj, Root).
del(Path, Obj, Root) -> uaxc_del:eval(Path, Obj, Root).
    
%%%%%%%%%%

mk_fun(new, Root) -> fun (PathVals) -> new(PathVals, Root) end;
mk_fun(get, Root) -> fun (Path, Obj) -> get(Path, Obj, Root) end;
mk_fun(put, Root) -> fun (Path, Val, Obj) -> put(Path, Val, Obj, Root) end;
mk_fun(del, Root) -> fun (Path, Obj) -> del(Path, Obj, Root) end.
                             

