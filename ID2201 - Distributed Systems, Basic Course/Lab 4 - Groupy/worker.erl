-module(worker).
-export([start/4, start/5]).
-define(change, 20).
-define(color, {0,0,0}).

start(Id, Module, Rnd, Sleep) ->
    spawn(fun() -> init(Id, Module, Rnd, Sleep) end).

init(Id, Module, Rnd, Sleep) ->
    {ok, Cast} = apply(Module, start, [Id]),
    Color = ?color,
    init_cont(Id, Rnd, Cast, Color, Sleep).

start(Id, Module, Rnd, Peer, Sleep) ->
    spawn(fun() -> init(Id, Module, Rnd, Peer, Sleep) end).

init(Id, Module, Rnd, Peer, Sleep) ->
    {ok, Cast} = apply(Module, start, [Id, Peer]),
    {ok, Color} = join(Id, Cast),
    init_cont(Id, Rnd, Cast, Color, Sleep).

join(Id, Cast) ->
    receive
        {view, _} ->
            Ref = make_ref(),
            Cast ! {mcast, {state_request, Ref}},
            state(Id, Ref);
        {error, Reason} ->
            {error, Reason}
    end.

state(Id, Ref) ->
    receive
        {state_request, Ref} ->
            receive
                {state, Ref, Color} ->
                    {ok, Color}
            end;
        _Ignore ->
            state(Id, Ref)
    end.

init_cont(Id, Rnd, Cast, Color, Sleep) ->
    random:seed(Rnd, Rnd, Rnd),
    Title = "Worker: " ++ integer_to_list(Id),
    Gui = gui:start(Title, self()),
    Gui ! {color, Color},
    worker(Id, Cast, Color, Gui, Sleep),
    Cast ! stop,
    Gui ! stop.

worker(Id, Cast, Color, Gui, Sleep) ->
    Wait = if Sleep == 0 -> 0; true -> random:uniform(Sleep) end,
    receive
        {change, N} ->
            Color2 = change_color(N, Color),
            Gui ! {color, Color2},
            worker(Id, Cast, Color2, Gui, Sleep);
        {state_request, Ref} ->
            Cast ! {mcast, {state, Ref, Color}},
            worker(Id, Cast, Color, Gui, Sleep);
        {state, _, _} ->
            worker(Id, Cast, Color, Gui, Sleep);
        {join, Peer, Gms} ->
            Cast ! {join, Peer, Gms},
            worker(Id, Cast, Color, Gui, Sleep);
        {view, _} ->
            worker(Id, Cast, Color, Gui, Sleep);
        stop -> 
            ok;
        Error ->
            io:format("strange message: ~w~n", [Error]),
            worker(Id, Cast, Color, Gui, Sleep)
    after Wait ->
        Cast !  {mcast, {change, random:uniform(?change)}},
        worker(Id, Cast, Color, Gui, Sleep)
    end.

%% rotate RGB and add N
change_color(N, {R,G,B}) ->
    {G, B, ((R+N) rem 256)}.
