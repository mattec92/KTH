-module(test).
-export([bench/2]).

bench(Host, Port) ->
    Start = now(),
    run(100, Host, Port),
    Finish = now(),
    timer:now_diff(Finish, Start).

run(N, Host, Port) ->
    if
	N == 0 ->
	    ok;
	true ->
	    spawn(fun() -> request(Host, Port) end),
	    run(N-1, Host, Port)
    end.

request(Host, Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    {ok, Server} = gen_tcp:connect(Host, Port, Opt),
    gen_tcp:send(Server, http:get("foo")),
    Recv = gen_tcp:recv(Server, 0),
    case Recv of
	{ok, _} ->
	    ok;
	{error, Error} ->
	    io:format("test: error: ~w~n", [Error])
    end,
    gen_tcp:close(Server).
