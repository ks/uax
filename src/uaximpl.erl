-module(uaximpl).

-type operation() :: get | put | new | del | typecheck.

-type modifier() :: key | encode | decode | none_tag.

-type c_env() :: [{atom(), any()}]. %% as argument to new, get, ..., tuples are in order given by args/1 function


%% @doc Specifies, how are the modifiers checked when creating compilation 
%% environment for particular node representing layer.
-callback opts() ->
    {Required :: [{modifier(), Validator :: fun((any()) -> boolean())}],
     Supplied :: [{modifier(), Validator :: fun((any()) -> boolean()), DefaultVal :: any()}],
     Optional :: [{modifier(), Validator :: fun((any()) -> boolean())}]}.


%% @doc Specifies, what modifiers are fetched from full node environment 
%% for compilation of a particular operation.
-callback args(operation()) -> [modifier()].

                                    
%% @doc Selects best function according to compilation environment,
%% which creates empty box for a type implemented by backing module.
-callback new(c_env()) -> fun (() -> EmptyBox :: any()).
     

%% @doc Selects best function according to compilation environment,
%% which fetches a value for a given key from the box.
-callback get(c_env()) -> fun ((Key :: any(), Box :: any()) -> Value :: any()).
     

%% @doc Selects best function according to compilation environment,
%% which associates a given key with a value and returns updated box.
-callback put(c_env()) -> fun ((Key :: any(), Val :: any(), Box :: any()) -> NewBox :: any()).
     

%% @doc Selects best function according to compilation environment,
%% which removes a key (when present) from a box, returning updated box.
-callback del(c_env()) -> fun ((Key :: any(), Box :: any()) -> NewBox :: any()).
    

%% @doc Selects best function according to compilation environment,
%% which removes a key (when present) from a box, returning updated box.
-callback typecheck(c_env()) -> fun ((Box :: any()) -> boolean()).
