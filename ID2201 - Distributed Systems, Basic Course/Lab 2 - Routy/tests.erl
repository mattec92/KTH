-module(tests).
-export([start/0]).
-include_lib("eunit/include/eunit.hrl").

start() ->
    ?assertEqual(map:update(berlin, [london, paris], []), [{berlin,[london,paris]}]),
    ?assertEqual(map:reachable(berlin, [{berlin,[london,paris]}]), [london,paris]),
    ?assertEqual(map:reachable(london, [{berlin,[london,paris]}]), []),
    ?assertEqual(map:all_nodes([{berlin,[london,paris]}]), [paris,berlin,london]),
    ?assertEqual(map:update(berlin, [madrid], [{berlin,[london,paris]}]), [{berlin, [madrid]}]),
    ?assertEqual(dijkstra:update(london, 2, amsterdam, []), []),
    ?assertEqual(dijkstra:update(london, 2, amsterdam, [{london, 2, paris}]), [{london,2,paris}]),
    ?assertEqual(dijkstra:update(london, 1, stockholm,[{berlin, 2, paris}, {london, 3, paris}]),[{london,1,stockholm}, {berlin, 2, paris}]),
    ?assertEqual(dijkstra:iterate([{paris, 0, paris}, {berlin, inf, unknown}],[{paris, [berlin]}], []), [{berlin,paris},{paris,paris}]),
    ?assertEqual(dijkstra:table([paris, madrid], [{madrid,[berlin]}, {paris, [rome,madrid]}]), [{berlin,madrid},{rome,paris},{madrid,madrid},{paris,paris}]).

