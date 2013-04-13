
-record(uaxl, {id :: atom() | {atom()},
               mode :: single | multi,
               env :: [{atom(), any()}],
               
               encode :: fun (),
               decode :: fun ()}).

-record(uaxn, {id :: undefined | atom() | {atom()},
               mode :: single | multi,
               env :: [{atom(), any()}],
               type :: atom(),
               kids :: [#uaxn{} | #uaxl{}],
               
               typecheck :: fun (),
               new :: fun (),
               get :: fun (),
               put :: fun (),
               del :: fun (),
               iter  :: fun ()}).

-record(uax, {root :: #uaxn{},
              new :: fun (() -> EmptyBox :: any()),
              get :: fun ((Id :: any(), Box :: any()) -> Val :: any()),
              put :: fun ((Id :: any(), Val :: any(), Box :: any()) -> NewBox :: any()),
              del :: fun ((Id :: any(), Box :: any()) -> NewBox :: any())}).



