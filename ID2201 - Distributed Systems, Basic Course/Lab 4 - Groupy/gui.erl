-module(gui).
-export([start/2]).
-define(width, 200).
-define(height, 200).
-define(bg, black).
start(Id, Master) ->
spawn_link(fun() -> init(Id, Master) end).
init(Id, Master) ->
Win = gs:window(gs:start(),[{map,true},{title, Id}, {bg, ?bg},
{width,?width},{height,?height}]),
loop(Win, Master).
loop(Win, Master)->
receive
{color, Color} ->
color(Win, Color),
loop(Win, Master);
{gs,_,destroy,[],[]} ->
Master ! stop,
ok;
stop ->
ok;
Error ->
io:format("gui: strange message ~w ~n", [Error]),
loop(Win, Master)
end.
color(Win, Color) ->
gs:config(Win, [{bg, Color}]).
