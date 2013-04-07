-module(uax_test).
-compile(export_all).

    
tt() ->
    [tt(X) || X <- [1, 2, 3, 4, 5]].

tt(1) ->
    Schema = {dict,
              [{a, {{array, fun (b) -> 0 end},
                    [{b, {proplist,
                          [{c, {{tuple, [{key, fun (d) -> 1 end}]},
                                [{d, {{list, fun (e) -> 1 end},
                                      [{e, {gb_tree,
                                            [{f, {orddict,
                                                  [{key}]}}]}}]}}]}}]}}]}}]},
    
    Get = uax:mk(get, Schema),
    
    Obj = dict:from_list(
            [{a, array:from_list(
                   [[{c, {[gb_trees:insert(
                             f, orddict:from_list([{k1, 1}, {k2, 2}]),
                             gb_trees:empty())]}}]])}]),
    
    1 = Get([a, b, c, d, e, f, {key, k1}], Obj),
    2 = Get([a, b, c, d, e, f, {key, k2}], Obj),
    
    {'EXIT', {{not_found, xxx}, _}} = (catch Get([a, b, c, d, e, f, {key, xxx}], Obj)),
    {'EXIT', {{path_error, xxx}, _}} = (catch Get([a, xxx], Obj)),
    
    ok;

tt(2) ->
    Obj = gb_trees:from_orddict(orddict:from_list([{X, random:uniform(1000)} || X <- lists:seq(1, 100)])),

    Get1 = uax:mk(get, {gb_tree, [{{id}, fun (R) -> integer_to_list(R) end}]}),
    
    [true = (integer_to_list(gb_trees:get(Id, Obj)) == Get1([{id, Id}], Obj)) || Id <- [10, 20, 30, 40, 50, 60]],
        
    Get2 = uax:mk(get, {{proplist, fun (Key) -> io:format("KEY = ~p~n", [Key]), Key end},
                        [{kk, {gb_tree, [{id}]}}]}),
    
    Obj2 = [{kk, Obj}],
    
    [true = (gb_trees:get(Id, Obj) == Get2([kk, {id, Id}], Obj2)) || Id <- [5, 10, 15, 20, 50, 80]],
    
    Get2A = uax:mk(get, {{proplist, [{none_tag, this_is_none_value}]},
                         [{kk, {gb_tree, [{id}]}}]}),
    
    {'EXIT', {{not_found, kk}, _}} = (catch Get2A([kk], [{kk, this_is_none_value}])),
    
    1 = Get2A([kk, {id, a}], [{kk, gb_trees:enter(a, 1, gb_trees:empty())}]),
    
    ok;


tt(3) ->
    Schema = {proplist, [{aa, {gb_tree, [bb]}}]},
    
    Obj = [{aa, gb_trees:insert(bb, 1, gb_trees:empty())}],

    Put = uax:mk(put, Schema),
    
    Put([aa, bb], xxxx, Obj);


tt(4) ->
    Schema = {proplist, [{aa, {gb_tree, [bb]}}, cc]},
    
    Obj = [{aa, gb_trees:insert(bb, 1, gb_trees:empty())},
           {cc, <<"xxx">>}],
    
    Put = uax:mk(put, Schema),
    Get = uax:mk(get, Schema),

    Obj2 = Put([aa, bb], 2, Obj),
    2 = Get([aa, bb], Obj2),
    <<"xxx">> = Get([cc], Obj2);

tt(5) ->
    Schema = {gb_tree, [{{x}, {proplist, [y]}}]},
    
    Obj = gb_trees:insert(aaa, [{y, 100}], gb_trees:empty()),
    
    Put = uax:mk(put, Schema),
    
    {1,{aaa,[{y,300},{y,100}],nil,nil}} = Put([{x, aaa}, y], 300, Obj);

%% tt(5) ->
%%     Schema = {gb_tree, [{a}]},
    
%%     Obj = gb_trees:insert(key, 10, gb_trees:empty()),
    
%%     Get = FinalGet(

tt(6) ->
    %% Schema = {proplist, [{a1, {proplist, 
    %%                            [{b1, {proplist, [c1, c2, c3]}}, 
    %%                             {b2, {gb_tree, [c1, c2, c3]}}]}},
    %%                      a2]},

    %% Obj = [{a1,[{b1,[{c1,xxx},{c2,yyy},{c3,zzz}]},
    %%             {b2,{1,{c1,100,nil,nil}}}]},
    %%        {a2,fff}],
    
    %% [Get, Put, Del] = [uax:mk(Op, Schema) || Op <- [get, put, del]].

    Schema = {{tuple, fun (a) -> 1; (b) -> 2 end},
              [{a, {gb_tree, [{id}]}},
               {b, {proplist, [cc]}}]},

    New = uax:mk(new, Schema),
    
    New([{a, [{{id}, [{<<"xxx1">>, 11},
                      {<<"yyy1">>, 22}]}]},
         {b, [{cc, valval}]}]).
    
    %% N2 = New([{a, {value, [{{id}, [{<<"xxx1">>, 11},
    %%                                {<<"yyy1">>, 22}]}]}},
    %%           {b, {value, [{cc, valval}]}}]).



schema1() ->
    ESchema = {{tuple, fun (out) -> 1; (in) -> 2 end},
               [{out, {gb_tree,
                       [{{id}, {{tuple, fun (tgt) -> 1; (attr) -> 2 end},
                                [tgt, attr]}}]}}]},
    
    VSchema = {gb_tree,
               [{{id}, {{tuple, fun (attr) -> 1; (e) -> 2 end},
                        [attr, {e, ESchema}]}}]},
    
    GSchema = {{tuple, fun (v) -> 1 end},
               [{v, VSchema}]},
    
    GSchema.


%% ttt() ->
    
%%     %% Schema = schema1(),
    
%%     %% New = uax:mk(new, Schema),
    
%%     %% New([{v, [{{id}, [{<<"vtx1">>, {value, {}}}]}]}]).


