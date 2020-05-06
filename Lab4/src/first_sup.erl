%%%-------------------------------------------------------------------
%% @doc first top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(first_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
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
      #{id => 'var_srv',
        start => {pow, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [pow]}
    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
