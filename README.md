# UAX

UAX is a declarative eDSL for specifying recursive, hash-table like data structures by Schema.

Schema describes a set of map like layers, where each layer:
- has type (implementation) - (array, dict, gb_tree, list, orddict, proplist, tuple)
- says if it expects a key or key + id combination
- allows to tweak a layer behavior with a set of parameters (key, encode, decode functions and none_tag)
- describes final keys or sub layers for next 

A basic, not very useful example, manipulating a Box with 3 fixed keys (a, b, c) backed by proplist: 

```erlang
%% Root layer definition - {Type, Kids}
(uax@xmd)1> Schema = {proplist, [a, b, c]}.
{proplist,[a,b,c]}

%% Creating a Get function for fetching values from Box
(uax@xmd)2> Get = uax:mk(get, Schema).
#Fun<uax_get.2.31293845>

%% Creating a New function for constructing the Box
(uax@xmd)3> New = uax:mk(new, Schema).
#Fun<uax_new.2.59012730>

%% Populating the Box with New function 
(uax@xmd)4> Box = New([{a, 1}, {b, 2}, {c, 3}]).
[{c,3},{b,2},{a,1}]

%% Fetching keys defined by Schema
(uax@xmd)6> Get([a], Obj).
1
(uax@xmd)7> Get([b], Obj).
2
(uax@xmd)8> Get([c], Obj).
3

%% Fetching invalid key, not in Schema
(uax@xmd)9> Get([d], Obj).
** exception error: {path_error,d}
     in function  uax_get:eval/3 (src/uax_get.erl, line 79)
```

A more complex schema showing a mix of fixed keys and variable keys stored in the same layer, backed by gb_tree:

```erlang
(uax@xmd)11> Schema = {gb_tree, [key1, key2, {other}]}.
{gb_tree,[key1,key2,{other}]}

(uax@xmd)12> New = uax:mk(new, Schema).
#Fun<uax_new.2.59012730>

(uax@xmd)13> Box = New([{key1, val1}, {key2, <<"VAL2">>}, {{other}, [{<<"otherkey3">>, val3}, {val4, 444}]}]).
{4,
 {key1,val1,nil,
       {key2,<<"VAL2">>,nil,
             {<<"otherkey3">>,val3,{val4,444,nil,nil},nil}}}}

(uax@xmd)14> Get = uax:mk(get, Schema).
#Fun<uax_get.2.31293845>

(uax@xmd)16> Get([key1], Box).
val1

(uax@xmd)17> Get([key2], Box).
<<"VAL2">>

%% accessing variable keys
(uax@xmd)20> Get([{other, val4}], Box).
444
(uax@xmd)21> Get([{other, <<"otherkey3">>}], Box).
val3

%% when missing:
(uax@xmd)22> Get([{other, <<"not there">>}], Box).
** exception error: {not_found,<<"not there">>}
     in function  uaximpl_gb_tree:'-get/1-fun-0-'/2 (src/uaximpl_gb_tree.erl, line 19)
     in call from uax_get:eval/3 (src/uax_get.erl, line 77)

%% fetching the root layer of the Box
(uax@xmd)23> Get([], Box).                        
{4,
 {key1,val1,nil,
       {key2,<<"VAL2">>,nil,
             {<<"otherkey3">>,val3,{val4,444,nil,nil},nil}}}}
```


This allows to abstract...




 
