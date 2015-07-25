-module(http).
-export([parse_request/1, ok/1, get/1]).

parse_request(R0) ->
    {Request, R1} = request_line(R0),
    {Headers, R2} = headers(R1),
    {Body, _} = message_body(R2),
    {Request, Headers, Body}.

request_line([$G, $E, $T, 32 |R0]) ->
    {URI, R1} = request_uri(R0),
    {Ver, R2} = http_version(R1),
    [13,10|R3] = R2,                  % 10, 10 is \r\n, just consume them.
    {{get, URI, Ver}, R3}.

						% When we find a space, we are finished parsing the request url.
request_uri([32|R0])->
    {[], R0};
						% Add the first character to the beginning of the list, 
						% and fill upp the rest with a recursive response to request_uri.
						% This will put all characters in order and break when we find a space.
request_uri([C|R0]) ->
    {Rest, R1} = request_uri(R0),
    {[C|Rest], R1}.

						% Just match on http/1.0 or http/1.1
http_version([$H, $T, $T, $P, $/, $1, $., $1 | R0]) ->
    {v11, R0};
http_version([$H, $T, $T, $P, $/, $1, $., $0 | R0]) ->
    {v10, R0}.

						% Parses each header until \r\n is reached
						% and individual headers until \r}n is reached
						% in the same fashion as request_uri.
headers([13,10|R0]) ->
    {[],R0};
headers(R0) ->
    {Header, R1} = header(R0),
    {Rest, R2} = headers(R1),
    {[Header|Rest], R2}.

header([13,10|R0]) ->
    {[], R0};
header([C|R0]) ->
    {Rest, R1} = header(R0),
    {[C|Rest], R1}.

message_body(R) ->
    {R, []}.

ok(Body) ->
    "HTTP/1.1 200 OK\r\n" ++ "\r\n" ++ Body.

get(URI) ->
    "GET " ++ URI ++ " HTTP/1.1\r\n" ++ "\r\n".
