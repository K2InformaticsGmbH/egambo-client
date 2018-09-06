-module(egambo_client).

-bahavior(gen_server).

-export([connect/2, disconnect/0, send/1]).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2]).

-define(L(_F, _A),
	io:format("~p:~p:~p "_F"~n", [?MODULE, ?FUNCTION_NAME, ?LINE | _A])
).

connect(Ip, Port) ->
	application:ensure_all_started(ssl),
	{ok, SslSocket} = ssl:connect(Ip, Port, [{active, false}, {packet, 4},
	                                         {mode, binary}]),
	{ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, SslSocket, []),
	ok = ssl:controlling_process(SslSocket, Pid).

disconnect() -> gen_server:call(?MODULE, die).

send(Json) when is_map(Json) ->
	JsonBin = jsx:encode(Json),
	?L("TX: ~s", [jsx:prettify(JsonBin)]),
	gen_server:call(?MODULE, {send, JsonBin}).

init(SslSocket) ->
	ok = ssl:setopts(SslSocket, [{active, true}]),
	{ok, SslSocket}.

handle_call(die, _From, SslSocket) -> {stop, normal, dying, SslSocket};
handle_call({send, JsonBin}, _From, SslSocket) ->
	{reply,
		case ssl:send(SslSocket, JsonBin) of
			ok -> {sent, byte_size(JsonBin)};
			{error, Error} -> Error
		end,
	 SslSocket}.
	
handle_info({ssl, SslSocket, JsonBin}, SslSocket) ->
	try
		?L("RX : ~p", [jsx:decode(JsonBin, [return_maps])]),
		?L("~s", [jsx:prettify(JsonBin)])
	catch _:Exception ->
		?L("RX ERROR : ~p~n~p", [Exception, JsonBin])
	end,
	{noreply, SslSocket};
handle_info({ssl_closed, SslSocket}, SslSocket) -> {stop, normal, SslSocket};
handle_info({ssl_error, SslSocket, Reason}, SslSocket) ->
	?L("ERROR : ~p", [Reason]),
	{noreply, SslSocket}.

handle_cast(Request, SslSocket) ->
	?L("cast not supported : ~p", [Request]),
	{noreply, SslSocket}.

terminate(Reason, SslSocket) ->
	ok = ssl:close(SslSocket),
	?L("terminating : ~p", [Reason]).