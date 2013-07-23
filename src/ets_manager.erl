-module(ets_manager).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%%TODO Avoid using named_tables allow for flexibilty. Log
%%TODO Log reassignments.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, give_me/1, give_me/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-record(state, {opts::term()}).
-type state()::#state{}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link () -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec give_me (ets:tid())
  -> {ok, ets:tid()} | {error, cant_give_away} | {error, already_own_table}.
give_me(Name) ->
    gen_server:call(?MODULE, {give_me, Name}).

-spec give_me (ets:tid(), [term()])
  -> {ok, ets:tid()} | {error, cant_give_away} | {error, already_own_table}.
give_me(Name, Opts) ->
    gen_server:call(?MODULE, {give_me, Name, Opts}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State} |
%%          {ok, State, Timeout} |
%%          ignore |
%%          {stop, Reason}
%%----------------------------------------------------------------------
-spec init (term()) -> {ok, state()}.
init([]) ->
    Opts = [set, named_table, protected, {keypos,1}, {heir,self(),[]},
            {write_concurrency,false}, {read_concurrency,false}],
    {ok, #state{opts=Opts}}.

-spec handle_call(term(), {pid(), term()}, State::state()) -> {reply, term(), State::state()}.
handle_call({give_me, Name}, {Pid, _Tag}, State) ->
    Return = give_me(Name, Pid, State),
    {reply, Return, State};
handle_call({give_me, Name, Opts}, {Pid, _Tag}, State) ->
    Return = give_me(Name, Opts, Pid, State),
    {reply, Return, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% receive message from dead process. Note returned table.
-spec handle_cast(term(), State::state()) -> {noreply, State::state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(timeout() | term(), State::state()) -> {noreply, State::state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate (term(), term()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change (term() | {down, term()}, State::state(), term()) -> {ok, State::state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @spec (Name, Pid, State) -> { ok, Tab } | {error, Reason }
%%        Name = atom()
%%        State = state()
%%        Pid = pid()
%%        Tab = ets:tab()
%%        Reason = atom()
%% @doc Create or return an ets table for use. It will make the ets_manager
%% a heir on the table so that on failure it is returned to it.
give_me(Name, Pid, State) ->
    give_me(Name, [], Pid, State).

-spec give_me (atom(), [term()], pid(), state())
  -> {ok, ets:tid()} | {error, cant_give_away} | {error, already_own_table}.
give_me(Name, Opts, Pid, State) ->
    Me = self(),
    case ets:info(Name) of
        undefined -> Tid = ets:new(Name, State#state.opts ++ Opts),
                     case ets:give_away(Tid, Pid, new_table) of
                         true -> {ok, Tid};
                         false -> {error, cant_give_away}
                     end;
        _Found -> case ets:info(Name, owner) of
                      Pid -> {error, already_own_table};
                      Me  -> case ets:give_away(Name, Pid, reissued) of
                                 true -> {ok, Name}; %% Name =:= Tid
                                 false -> {error, cant_give_away}
                             end
                  end
    end.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-spec test () -> term().
-spec give_me_test () -> none().
give_me_test() ->
    Name = foo,
    Opts = [set, named_table, protected, {keypos,1}, {heir,self(),[]},
            {write_concurrency,false}, {read_concurrency,false}],
    State = #state{opts=Opts},
    P = spawn( new_check(Name) ),
    {ok,Name} = give_me(Name, P, State),
    %%kill and see if reassigns.
    exit(P, zeds_dead_baby),
    return_check(Name, P),
    Z = spawn( reissue_check(Name) ),
    {ok,Name} = give_me(Name, Z, State).

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

-endif.
