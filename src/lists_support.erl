-module(lists_support).

-export([n_length_chunks/2, n_length_chunks_back/2, join/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

n_length_chunks([],_) -> 
    [];

n_length_chunks(List,Len) when Len > length(List) ->
    [List];

n_length_chunks(List,Len) ->
    {Head,Tail} = lists:split(Len, List),
    [Head | n_length_chunks(Tail,Len)].

n_length_chunks_back(List,Len) -> 
    Lists = n_length_chunks(lists:reverse(List), Len),
    [lists:reverse(X) || X<-lists:reverse(Lists)].

join(List, Delimiter) ->
    join(List, Delimiter, []).

join([], _Delimiter, Res) ->
    lists:flatten(lists:reverse(Res));

join([String | Tail], Delimiter, []) ->
    join(Tail, Delimiter, [String]);

join([String | Tail], Delimiter, Res) ->
    join(Tail, Delimiter, [String, Delimiter | Res]).


-ifdef(TEST).
n_length_chunks_test_() ->
    [?_assertEqual([], n_length_chunks("",42)),

     ?_assertEqual(["a","b","c","d","e"], n_length_chunks("abcde",1)),
     ?_assertEqual(["ab","cd","e"], n_length_chunks("abcde",2)),
     ?_assertEqual(["abc", "de"], n_length_chunks("abcde",3)),
     ?_assertEqual(["abcd", "e"], n_length_chunks("abcde",4)),
     ?_assertEqual(["abcde"], n_length_chunks("abcde",5)),
     ?_assertEqual(["abcde"], n_length_chunks("abcde",6)),
     ?_assertEqual(["abcde"], n_length_chunks("abcde",600))
    ].

n_length_chunks_back_test_() ->
    [?_assertEqual([], n_length_chunks_back("",42)),

     ?_assertEqual(["a","b","c","d","e"], n_length_chunks_back("abcde",1)),
     ?_assertEqual(["a","bc","de"], n_length_chunks_back("abcde",2)),
     ?_assertEqual(["ab", "cde"], n_length_chunks_back("abcde",3)),
     ?_assertEqual(["a", "bcde"], n_length_chunks_back("abcde",4)),
     ?_assertEqual(["abcde"], n_length_chunks_back("abcde",5)),
     ?_assertEqual(["abcde"], n_length_chunks_back("abcde",6)),
     ?_assertEqual(["abcde"], n_length_chunks_back("abcde",600))
    ].

join_test_() ->
    [?_assertEqual([], join([],",")),
     ?_assertEqual("a,b,c", join(["a", "b", "c"],",")),
     ?_assertEqual("a", join(["a"],","))
    ].

-endif.
