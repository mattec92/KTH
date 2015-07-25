-module(rudy).
-export([start/1, stop/0]).

start(Port) ->
    register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
    exit(whereis(rudy), "time to die").

init(Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:listen(Port, Opt) of
	{ok, Listen} ->
	    handler(Listen),			%: to be filled in
	    gen_tcp:close(Listen),
	    ok;
	{error, Error} ->
	    error
    end.

handler(Listen) ->
    case gen_tcp:accept(Listen) of
	{ok, Client} ->
	    request(Client),			%: to be filled in
	    handler(Listen);
	{error, Error} ->
	    error
    end.

request(Client) ->
    Recv = gen_tcp:recv(Client, 0),
    case Recv of
	{ok, Str} ->
	    Request = http:parse_request(Str),		%: to be filled in
	    Response = reply(Request),
	    gen_tcp:send(Client, Response);
	{error, Error} ->
	    io:format("rudy: error: ~w~n", [Error])
    end,
    gen_tcp:close(Client).

reply({{get, URI, _}, _, _}) ->
    timer:sleep(40),
    http:ok(URI).		      	%: to be filled in


