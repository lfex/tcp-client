-module('tcp-client-mgr').

-behaviour(gen_statem).

-export([start_link/3, request/2]).
-export([callback_mode/0, init/1]).
-export([disconnected/3, connected/3]).
-export([parse_response/2, report/1]).

-define(SERVER, ?MODULE).
-define(STATEM_OPTS, []).

%% Public API.

start_link(Host, Port, Opts) ->
    gen_statem:start_link({global, ?SERVER}, ?MODULE, {Host, Port, Opts}, ?STATEM_OPTS).

request(Pid, Request) ->
    gen_statem:call(Pid, {request, Request}).

%% gen_statem callbacks

callback_mode() -> [state_functions, state_enter].

init({Host, Port, Opts}) ->
    {tcp, TcpOpts} = lists:keyfind(tcp, 1, Opts),
    {parser, Parser} = lists:keyfind(parser, 1, Opts),
    {reporter, Reporter} = lists:keyfind(reporter, 1, Opts),
    Data = #{host => Host,
             port => Port,
             parser => Parser,
             reporter => Reporter,
             tcp_opts => TcpOpts,
             requests => #{},
             from => undefined},
    Actions = [{next_event, internal, connect}],
    {ok, disconnected, Data, Actions}.

%% Disconnected state

disconnected(enter, disconnected, _Data) -> keep_state_and_data;

disconnected(enter, connected, #{requests := Requests} = Data) ->
    io:format("Connection closed~n"),

    lists:foreach(fun({_, From}) -> gen_statem:reply(From, {error, disconnected}) end,
                  Requests),

    Data1 = maps:put(socket, undefined, Data),
    Data2 = maps:put(requests, #{}, Data1),

    Actions = [{{timeout, reconnect}, 500, undefined}],
    {keep_state, Data2, Actions};

disconnected(internal, connect, #{host := Host, port := Port, tcp_opts := Opts} = Data) ->
    case gen_tcp:connect(Host, Port, Opts) of
        {ok, Socket} ->
            Data1 = maps:put(socket, Socket, Data),
            {next_state, connected, Data1};
        {error, Error} ->
            io:puts("Connection failed: ~ts~n", [inet:format_error(Error)]),
            keep_state_and_data
    end;

disconnected({timeout, reconnect}, _, Data) ->
    Actions = [{next_event, internal, connect}],
    {keep_state, Data, Actions};

disconnected({call, From}, {request, _}, _Data) ->
    Actions = [{reply, From, {error, disconnected}}],
    {keep_state_and_data, Actions}.

%% Connected state

connected(enter, _OldState, _Data) ->
    keep_state_and_data;

connected(info, {tcp_closed, Socket}, #{socket := Socket} = Data) ->
    {next_state, disconnected, Data};

connected({call, From}, {request, Request}, #{socket := Socket} = Data) ->
    %%io:format("Call from ~p~n", [From]),
    %% XXX Setting 'from' in the map / Data like this is probably horribly
    %% unsafe; let's fix it ...
    send(Socket, Request, maps:put(from, From, Data));

connected(cast, {request, Request}, #{socket := Socket} = Data) ->
    send(Socket, Request, maps:put(from, undefined, Data));

connected(info, {tcp, Socket, Packet}, #{socket := Socket, from := From, parser := {Pmod, Pfun}, reporter := Reporter} = Data) ->
    case apply(Pmod, Pfun, [Packet, Reporter]) of
        [Response] -> Response;
        Response -> Response
    end,
    %% XXX Nasty hack!
    case From =/= undefined of
        true ->
            gen_statem:reply(From, Response);
        _ ->
            ok
    end,
    {keep_state, Data}.

%%%===================================================================
%%% Private functions
%%%===================================================================

%% XXX Move these next three functions into undertone / Extempore support
%%     and change these to be very general / pass-through fns ...
parse_response(Packet, {Rmod, Rfun}) ->
    Elements = [ convert_type(E) || E <- binary:split(Packet, <<0>>, [global]), E =/= <<>> ],
    [ apply(Rmod, Rfun, [E]) || E <- Elements ],
    Elements.

convert_type(Data) ->
    case Data of
        <<"#t">> ->
            true;
        <<"#f">> ->
            false;
        <<"NIL">> ->
            nil;
        _ -> Data
    end.

report(Data) ->
    lfe_io:format("~p~n", [Data]).

send(Socket, Request, Data) ->
    case gen_tcp:send(Socket, Request) of
        ok ->
            {keep_state, Data};
        {error, _} ->
            ok = gen_tcp:close(Socket),
            {next_state, disconnected, Data}
    end.
