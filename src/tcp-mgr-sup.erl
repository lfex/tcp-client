-module('tcp-mgr-sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(MGR, 'tcp-mgr-socket').
-define(CHILD(Host, Port, Options),
  #{id => ?MGR,
    start => {?MGR, start_link, [Host, Port, Options]},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [?MGR]
   }).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  {ok, Server} = application:get_env('tcp-mgr', server),
  {host, Host} = lists:keyfind(host, 1, Server),
  {port, Port} = lists:keyfind(port, 1, Server),
  {options, Options} = lists:keyfind(options, 1, Server),
  SupFlags = #{strategy => one_for_one,
               intensity => 5,
               period => 10},
  Child = ?CHILD(Host, Port, Options),
  {ok, {SupFlags, [Child]}}.
