TODO: DOCS!!!

UAX (universal accessor)
===

Needs Schema with recursive data structures layout description (check src/uax_test.erl for now).


UAX is an embedded DSL in Erlang for doing nifty things:

(this is just GET, there's also PUT, DEL and NEW uax:mk/2 functions)

(uax@xmd)4> Schema = {proplist, [key]}.  
{proplist,[key]}
(uax@xmd)5> Get = uax:mk(get, Schema). 
#Fun<uax_get.2.31293845>
(uax@xmd)6> Obj = [{key, val}, {some_other_key, other_val}].
[{key,val},{some_other_key,other_val}]
(uax@xmd)7> Get([key], Obj).
val
(uax@xmd)8> Get([], Obj).
[{key,val},{some_other_key,other_val}]
(uax@xmd)10> Get([accessor_not_in_schema], Obj).             
** exception error: {path_error,accessor_not_in_schema}
     in function  uax_get:eval/3 (src/uax_get.erl, line 79)




(uax@xmd)13> Schema = {proplist, [key, {id}]}.               
{proplist,[key,{id}]}
(uax@xmd)14> Get = uax:mk(get, Schema).                      
#Fun<uax_get.2.31293845>
(uax@xmd)16> Obj = [{key, val}, {some_other_key, other_val}].
[{key,val},{some_other_key,other_val}]
(uax@xmd)17> Get([id], Obj).                                 
** exception error: {path_error,id}
     in function  uax_get:eval/3 (src/uax_get.erl, line 79)
(uax@xmd)18> Get([{id, some_other_key}], Obj).
other_val
(uax@xmd)19> Get([{id, non_existing}], Obj).  
** exception error: {not_found,non_existing}
     in function  uaximpl_proplist:'-get/1-fun-0-'/2 (src/uaximpl_proplist.erl, line 19)
     in call from uax_get:eval/3 (src/uax_get.erl, line 77)




 

