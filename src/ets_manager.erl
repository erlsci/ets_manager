-module(ets_manager).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%%TODO Avoid using named_tables allow for flexibilty. Log
%%TODO Log reassignments.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
        create_or_return/1, create_or_return/2,
        create_or_return/3, create_or_return/4]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% Record Definitions
%% ------------------------------------------------------------------

-include_lib("ets_manager/include/state.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link () -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec create_or_return (Name::ets:tab())
  -> {ok, ets:tid()} | {error, already_own_table}.
create_or_return(Name) ->
    gen_server:call(?MODULE, {create_or_return, Name}).

-spec create_or_return (Name::ets:tab(), Opts::[term()])
  -> {ok, ets:tid()} | {error, already_own_table}.
create_or_return(Name, Opts) ->
    gen_server:call(?MODULE, {create_or_return, Name, Opts}).

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
init(_) ->
    Opts = [set, named_table, protected, {keypos,1}, {heir,self(),[]},
            {write_concurrency,false}, {read_concurrency,false}],
    {ok, #state{opts=Opts}}.

-spec handle_call(term(), {pid(), term()}, State::state()) -> {reply, term(), State::state()}.
handle_call({create_or_return, Name}, {Pid, _Tag}, State) ->
    Return = create_or_return(Name, Pid, State),
    {reply, Return, State};
handle_call({create_or_return, Name, Opts}, {Pid, _Tag}, State) ->
    Return = create_or_return(Name, Opts, Pid, State),
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
%% Utility Function Definitions
%% ------------------------------------------------------------------

%% @doc Create or return an ets table for use. It will make the ets_manager
%% a heir on the table so that on failure it is returned to it.
-spec create_or_return (Name::ets:tab(), pid(), state()) ->
    {ok, ets:tid()} | {error, term()}.
create_or_return(Name, Pid, State) ->
    create_or_return(Name, [], Pid, State).

-spec create_or_return (Name::ets:tab(), [term()], pid(), state()) ->
    {ok, ets:tab()} | {error, already_own_table}.
create_or_return(Name, Opts, Pid, State) ->
    Me = self(),
    case ets:info(Name) of
        undefined ->
            Tid = ets:new(Name, State#state.opts ++ Opts),
            true = ets:give_away(Tid, Pid, new_table),
            {ok, Tid};
        Found when is_list(Found) ->
            case ets:info(Name, owner) of
                Pid -> {error, already_own_table};
                Me  ->
                    true = ets:give_away(Name, Pid, reissued),
                    {ok, Name} %% Name =:= Tid
                end
    end.
