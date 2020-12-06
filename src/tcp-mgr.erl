-module('tcp-mgr').

%% API
-export([call_msg/1, cast_msg/1]).

-define(MGR, {global, 'tcp-mgr-socket'}).

%%%===================================================================
%%% API
%%%===================================================================

call_msg(Data) ->
    gen_statem:call(?MGR, {request, Data}).

cast_msg(Data) ->
    gen_statem:cast(?MGR, {request, Data}).
