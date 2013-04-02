%%%% Universal Accessor DSL?
%%%% 
-module(uax).
-compile(export_all).

%%%%%%%%%%

%% -record(uax, {get, put, delete}).

%%

mk(new, Schema) -> uax_new:compile(Schema);
mk(get, Schema) -> uax_get:compile(Schema);
mk(put, Schema) -> uax_put:compile(Schema);
mk(del, Schema) -> uax_del:compile(Schema).


%%
             
impls() ->
    [uaximpl_array, uaximpl_dict, uaximpl_gb_tree, uaximpl_list,
     uaximpl_orddict, uaximpl_proplist, uaximpl_tuple].

impl_mod(Type) when is_atom(Type) ->
    list_to_existing_atom("uaximpl_" ++ atom_to_list(Type)).


