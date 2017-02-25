-module(ets_manager_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ets_manager/include/state.hrl").

-spec create_or_return_test() -> none().
create_or_return_test() ->
    Name = foo,
    Opts = [set, named_table, protected, {keypos,1}, {heir,self(),[]},
            {write_concurrency,false}, {read_concurrency,false}],
    State = #state{opts=Opts},
    P = spawn( new_check(Name) ),
    {ok,Name} = ets_manager:create_or_return(Name, P, State),
    %%kill and see if reassigns.
    exit(P, zeds_dead_baby),
    return_check(Name, P),
    Z = spawn( reissue_check(Name) ),
    {ok,Name} = ets_manager:create_or_return(Name, Z, State).

new_check(Name) ->
    fun() ->
        receive
            {'ETS-TRANSFER', Tid, _Pid, new_table} ->
                ?assertEqual( Name, Tid );
            Error -> ?assertEqual(ok,Error)
        end
    end.

return_check(Name, Pid) ->
    receive
        {'ETS-TRANSFER', Tid, Pid, []} ->
            ?assertEqual( Name, Tid );
        Error -> ?assertEqual(ok, Error)
    end.

reissue_check(Name) ->
    fun() ->
        receive
            {'ETS-TRANSFER', Tid, _Pid, reissued} ->
                ?assertEqual( Name, Tid );
            Error -> ?assertEqual(ok,Error)
        end
    end.

-spec test () -> term().
