%%%-------------------------------------------------------------------
%% @doc pollution top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pollution_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  ets:new(backup, [set, public, named_table]),
  ets:insert(backup, {val, pollution:createMonitor()}),
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 2,
                 period => 3},
    ChildSpecs = [
      #{id => 'pollution_server',
        start => {pollution_gen_server, start, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [pollution_gen_server]}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
