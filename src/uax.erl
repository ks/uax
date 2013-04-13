%%%% Universal Accessor DSL?
%%%% 
-module(uax).

-export([mk/1, mk/2, record_key/1]).

-export([new/2, get/3, put/4, del/3]).

-export([iter/4]).

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


new(#uax{root = Root}, PathVals) -> new(Root, PathVals);
new(#uaxn{} = Root, PathVals) -> uaxc_new:eval(Root, PathVals).

get(#uax{root = Root}, Path, Obj) -> get(Root, Path, Obj);
get(#uaxn{} = Root, Path, Obj) -> uaxc_get:eval(Root, Path, Obj).

put(#uax{root = Root}, Path, Val, Obj) -> put(Root, Path, Val, Obj);
put(#uaxn{} = Root, Path, Val, Obj) -> uaxc_put:eval(Root, Path, Val, Obj).

del(#uax{root = Root}, Path, Obj) -> del(Root, Path, Obj);
del(#uaxn{} = Root, Path, Obj) -> uaxc_del:eval(Root, Path, Obj).

iter(#uax{root = Root}, Fun, State, Obj) -> iter(Root, Fun, State, Obj);
iter(#uaxn{} = Root, Fun, State, Obj) -> uaxc_iter:eval(Root, Fun, State, Obj).
    
%%%%%%%%%%

%%
mk_fun(new, Root) -> fun (PathVals) -> new(Root, PathVals) end;
mk_fun(get, Root) -> fun (Path, Obj) -> get(Root, Path, Obj) end;
mk_fun(put, Root) -> fun (Path, Val, Obj) -> put(Root, Path, Val, Obj) end;
mk_fun(del, Root) -> fun (Path, Obj) -> del(Root, Path, Obj) end.

                               
                             

