%% @author Raimar Falke
%% @copyright 2013 Raimar Falke
% Distrbuted under GNU General Public License version 2
% Based on http://stackoverflow.com/questions/3685987/erlang-semaphore-mutex

-module(semaphore).

-export([start/1, obtain/1, release/1, stats/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start(Max)->
    spawn(fun() -> run(Max, Max, sets:new()) end).

obtain(SemaPid)->
    SemaPid!{obtain,self()},
    receive
	granted -> ok
    end.

release(SemaPid)->
    SemaPid!{release,self()},
    receive
	granted-> ok
    end.

stats(SemaPid)->
    SemaPid!{stats,self()},
    receive
	{activePids,List}-> List
    end.

internalObtain(Free, Max, ActivePids, Pid)->
    Pid!granted,
    run(Free-1, Max, sets:add_element(Pid, ActivePids)).

internalRelease(Free, Max, ActivePids, Pid)->    
    Pid!granted,
    run(Free+1, Max, sets:del_element(Pid, ActivePids)).

internalStats(Free, Max, ActivePids, Pid)-> 
    Pid!{activePids, sets:to_list(ActivePids)},
    run(Free, Max, ActivePids).

run(0, Max, ActivePids)->
    receive
	{stats, Pid} -> internalStats(0, Max, ActivePids, Pid);
	{release, Pid} -> internalRelease(0, Max, ActivePids, Pid)
    end;
run(Max, Max, ActivePids)->
    receive
	{stats, Pid} -> internalStats(Max, Max, ActivePids, Pid);
	{obtain, Pid} -> internalObtain(Max, Max, ActivePids, Pid)
    end;
run(Free, Max, ActivePids)->
    receive
	{stats, Pid} -> internalStats(Free, Max, ActivePids, Pid);
	{obtain, Pid} -> internalObtain(Free, Max, ActivePids, Pid);
	{release, Pid} -> internalRelease(Free, Max, ActivePids, Pid)
    end.

-ifdef(TEST).

a_test() ->
    Pid=start(2),
    ok=obtain(Pid),
    ok=obtain(Pid),
    ok=release(Pid),
    ok=release(Pid).

-endif.
