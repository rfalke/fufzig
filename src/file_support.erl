-module(file_support).

-export([write_response_to_file/3]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
% for file_info
-include_lib("kernel/include/file.hrl").
-endif.

write_response_to_file(BasePath, Content, Url) ->
    {ok,_Proto,Host,_Port,Path} = url:split_full(Url),
    Fname = lists:concat([BasePath,"/",Host,Path]),
    Fname2 = case lists:suffix("/", Fname) of
	true -> Fname++"index.html";
	false -> Fname
    end,
    %io:format("Will write to ~p ~n", [Fname2]),
    ok = filelib:ensure_dir(Fname2),
    ok = file:write_file(Fname2, Content),
    io:format("  saved to ~s~n", [Fname2]),
    ok.

%% ================ Begin test code
-ifdef(TEST).
get_file_type(Path)->
    {ok, #file_info{type=Type}} = file:read_link_info(Path),
    Type.

is_dir(Path)->
    get_file_type(Path)==directory.

mktemp()->
    {A,B,C}=now(),
    N=node(),
    lists:flatten(io_lib:format("/tmp/~p-~p.~p.~p",[N,A,B,C])).

% inspired by http://docs.python.org/2/library/os.path.html#os.path.walk
walk(Path, VisitBefore, VisitAfter, Arg) ->
    {ok, Names} = file:list_dir(Path),
    Filenames = [X || X<-Names, not is_dir(Path++"/"++X)],
    Dirnames = [X || X<-Names, is_dir(Path++"/"++X)],

    VisitBefore(Arg, Path, Filenames, Dirnames),
    [walk(Path ++ "/"++X, VisitBefore, VisitAfter, Arg) || X <- Dirnames],
    VisitAfter(Arg, Path, Filenames, Dirnames),
    ok.

rm_r(Root) ->
    DelOneFile = fun(X) -> 
			 ok=file:delete(X) 
		 end, 
    Before = fun(_Arg, _Path, _Files, _Dirs) -> ok end,
    After = fun(_Arg, Path, Files, _Dirs) -> 
		    [DelOneFile(Path++"/"++X) || X<-Files],
		    ok = file:del_dir(Path),
		    ok
	    end,
    walk(Root, Before, After, []),
    ok.

write_test() ->
    Path = mktemp(),
    try
	write_response_to_file(Path, "foobar", "http://www.example.com/abc/def.html"),
	{ok,Content} = file:read_file(Path++"/www.example.com/abc/def.html"),
	?_assertEqual("foobar", binary_to_list(Content))
    after
	rm_r(Path)
    end.

write2_test() ->
    Path = mktemp(),
    try
	write_response_to_file(Path, "foobar", "https://www.example.com:80/abc/def.html"),
	{ok,Content} = file:read_file(Path++"/www.example.com/abc/def.html"),
	?_assertEqual("foobar", binary_to_list(Content))
    after
	rm_r(Path)
    end.
-endif.
