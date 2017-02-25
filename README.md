ets_manager
===========

Separate management of ets tables to prevent lost data.

## Description

This library will allow tables to be created that will
have it's 'heir' as the manger process. It will then assign
the table to the calling process. If the process dies
unexpectedly the table returns to the manager process.

Add the library to your rebar config

```
{
  deps,
  [
   {ets_manager, {git, "https://github.com/erlsci/ets_manager.git",
     {tag, "v0.4.0"}}},
   ....
  ]
}.

```
Then just use `rebar3` to automatically download and compile as part
or your project

```bash
$ rebar3 compile

```


## Breaking Changes

Note that in v0.4.0, the function `give_me` was renamed to `create_or_return`.
If you need the old name, be sure to use the v0.3.0 release.


## Usage

### API

```
application:start(ets_manager).
P = spawn(
        fun() ->
            Name = foo,
            {ok, Name} = ets_manager:create_or_return(Name),
            receive
                {'ETS-TRANSFER', Tid, _Pid, new_table} -> some_function(Tid)
            end
         end.
    ),
%%kill and see if reassigns.
exit(P, zeds_dead_baby),
%%Reissue the table
Z = spawn(
        fun() ->
           Name = foo,
           {ok, Name} = ets_manager:create_or_return(Name),
           receive
               {'ETS-TRANSFER', Tid, _Pid, reissued} -> some_function(Tid)
           end
        end.
    ).

```
