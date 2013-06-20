%% @author Raimar Falke
%% @copyright 2013 Raimar Falke
% Distrbuted under GNU General Public License version 2

-module(file_support).

-export([write_response_to_file/3]).

% for file_info
-include_lib("kernel/include/file.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

adjust_file_name(Fname)->
    case lists:suffix("/", Fname) of
        true ->
	    Result = Fname ++ "index.html",
	    case file_info_of(Result)==directory of
		true->Result ++ ".alternative";
		false->Result
	    end;
        false -> case file_info_of(Fname)==directory of
		     true->Fname ++ "/index.html";
		     false->Fname
		 end
    end.

write_response_to_file(BasePath, Content, Url) ->
    {ok, _Proto, Host, _Port, Path, Query} = url:split_full(Url),
    Fname = BasePath++ "/"++ Host++Path++Query,
    Fname2 = adjust_file_name(Fname),
    ok = ensure_parent_dir_exists(Fname2),
    Dir = filename:dirname(Fname2),
    Name = filename:basename(Fname2),
    Name2 = lists:sublist(Name,100),
    Fname3 = Dir++"/"++Name2,
    ok = file:write_file(Fname3, Content),
    Fname3.

file_info_of(Path) ->
    case file:read_link_info(Path) of
	{ok, #file_info{type=Type}} -> Type;
	{error, enoent} -> enoent;
    {error, enotdir} -> enoent;
    {error, enametoolong} -> enoent
    end.

ensure_parent_dir_exists(Fname)->
    ensure_dir(filename:dirname(Fname)).

ensure_dir(Dir) ->
    case file_info_of(Dir) of
	enoent ->
	    Parent = filename:dirname(Dir),
	    ensure_dir(Parent),
	    ok=file:make_dir(Dir);
	directory ->
	    ok;
	regular ->
	    ok=file:rename(Dir, Dir++".tmp"),
	    ok=file:make_dir(Dir),
	    ok=file:rename(Dir++".tmp",Dir++"/index.html"),
	    ok;
	_->error("Unexepected case for "++Dir)
    end.

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
	write_response_to_file(Path, "foobar", "http://www.example.com/abc/def.html?foo=bar"),
	{ok,Content} = file:read_file(Path++"/www.example.com/abc/def.html?foo=bar"),
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

write3_test() ->
    Path = mktemp(),
    try
	write_response_to_file(Path, "content1", "https://www.example.com:80/abc"),
	write_response_to_file(Path, "content2", "https://www.example.com:80/abc/def.html"),
	{ok,Content1} = file:read_file(Path++"/www.example.com/abc/index.html"),
	?_assertEqual("conent1", binary_to_list(Content1)),
	{ok,Content2} = file:read_file(Path++"/www.example.com/abc/def.html"),
	?_assertEqual("content2", binary_to_list(Content2))
    after
	rm_r(Path)
    end.

write4_test() ->
    Path = mktemp(),
    try
	write_response_to_file(Path, "content1", "https://www.example.com:80/abc/def.html"),
	write_response_to_file(Path, "content2", "https://www.example.com:80/abc"),
	{ok,Content1} = file:read_file(Path++"/www.example.com/abc/def.html"),
	?_assertEqual("conent1", binary_to_list(Content1)),
	{ok,Content2} = file:read_file(Path++"/www.example.com/abc/index.html"),
	?_assertEqual("content2", binary_to_list(Content2))
    after
	rm_r(Path)
    end.

write5_test() ->
    Path = mktemp(),
    try
	write_response_to_file(Path, "content1", "https://www.example.com:80/abc/index.html/foo"),
	write_response_to_file(Path, "content2", "https://www.example.com:80/abc/"),
	{ok,Content1} = file:read_file(Path++"/www.example.com/abc/index.html/foo"),
	?assertEqual("content1", binary_to_list(Content1)),
	{ok,Content2} = file:read_file(Path++"/www.example.com/abc/index.html.alternative"),
	?assertEqual("content2", binary_to_list(Content2))
    after
	rm_r(Path)
    end.
-endif.
