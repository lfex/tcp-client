-module('tcp-client-app').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(SUP, 'tcp-client-sup').

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec start(term(), term()) -> {error, term()} | {ok, pid()}.
start(_Type, _Args) ->
  apply(?SUP, start_link, []).

-spec stop(term()) -> ok.
stop(_State) ->
  ok.
