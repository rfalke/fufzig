%% @author Raimar Falke
%% @copyright 2013 Raimar Falke
% Distrbuted under GNU General Public License version 2

-module(support).

-export([timestamp/0, format_int_with_thousand_separator/2, format_bytes/1, add_index/1, binary_join/1, format_rate/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

timestamp() ->
    {Mega, Sec, Micro} = os:timestamp(),
    (Mega * 1000000 + Sec) + Micro / 1000000.0.

helper(List, Sep) ->
    case List of
        [H1, H2, H3, H4 | T] -> [H1, H2, H3] ++ Sep ++ helper([H4 | T], Sep);
        _ -> List
    end.

format_int_with_thousand_separator(N, Sep) ->
    S = integer_to_list(N),
    lists:reverse(helper(lists:reverse(S), Sep)).

format_bytes(N) ->
    format_int_with_thousand_separator(N, ",") ++ " bytes".

format_rate(_Bytes, TimeInSec) when TimeInSec < 0.1->
    "--.- K/s";
format_rate(Bytes, TimeInSec) ->
    BytesPerSecond = float(Bytes)/TimeInSec,
    KBPS = BytesPerSecond/1024.0,
    lists:flatten(io_lib:format("~.1f K/s", [KBPS])).

add_index(List)->
    add_index(List, 1).

add_index([H|T], N) ->
    [{N,H} | add_index(T, N+1)];
add_index([],_) -> [].

binary_join(List) ->
    F = fun(A, B) -> <<A/binary, B/binary>> end,
    lists:foldr(F, <<>>, List).

-ifdef(TEST).

format_int_with_thousand_separator_test_() ->
    [?_assertEqual("0", format_int_with_thousand_separator(0,",")),
     ?_assertEqual("1", format_int_with_thousand_separator(1,",")),
     ?_assertEqual("12", format_int_with_thousand_separator(12,",")),
     ?_assertEqual("123", format_int_with_thousand_separator(123,",")),
     ?_assertEqual("1,234", format_int_with_thousand_separator(1234,",")),
     ?_assertEqual("12,345", format_int_with_thousand_separator(12345,",")),
     ?_assertEqual("123,456", format_int_with_thousand_separator(123456,",")),
     ?_assertEqual("1,234,567", format_int_with_thousand_separator(1234567,","))
    ].

add_index_test_() ->
    [?_assertEqual([{1,first}, {2, second}], add_index([first,second]))
    ].

binary_join_test_() ->
    [?_assertEqual( <<1,2,3,4>> , binary_join([ <<1,2>> , <<3,4>> ]))
    ].

format_rate_test_() ->
    [?_assertEqual("--.- K/s", format_rate(12345, 0)),
     ?_assertEqual("12.3 K/s", format_rate(12595, 1)),
     ?_assertEqual("6.2 K/s", format_rate(6.2*2048, 2))
    ].

-endif.
