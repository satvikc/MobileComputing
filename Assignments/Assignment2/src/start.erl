-module(start).
-compile(export_all).
-import(mobile).
-import(bs).
-import(bsc).
-import(msc).
-export([start_normal/0,state/1]).


%% Implement flush
start_normal() ->
    {ok,MSC}  = msc:start_link(),
    register(msc,MSC),				
    {ok,BSC1} = bsc:start_link({MSC}),
    {ok,BSC2} = bsc:start_link({MSC}),
    register(bsc1,BSC1),
    register(bsc2,BSC2),
    {ok,BS1} = bs:start_link({BSC1}),
    {ok,BS2} = bs:start_link({BSC2}),
    register(bs1,BS1),
    register(bs2,BS2),
    {ok,MS} = mobile:start_link({BS1,[BS2]}),
    register(ms,MS),
    timer:apply_after(6000,?MODULE,startHand,[]).

startHand() ->
    ms ! {self(),set,signal,0}.

state(Whose) ->
    case lists:member(Whose,[ms,ms2,bs1,bsc1,msc,bsc2,bs2]) of
	true ->
	    Whose ! {self(),get,state},
	    receive
		I ->
		    io:format("State = ~p~n",[I])
	    end;
	false -> io:format("Error: Wrong State Request")
    end.

start_failure() ->
    {ok,MSC}  = msc:start_link(),
    register(msc,MSC),				
    {ok,BSC1} = bsc:start_link({MSC}),
    {ok,BSC2} = bsc:start_link({MSC}),
    register(bsc1,BSC1),
    register(bsc2,BSC2),
    {ok,BS1} = bs:start_link({BSC1}),
    {ok,BS2} = bs:start_link({BSC2}),
    register(bs1,BS1),
    register(bs2,BS2),
    {ok,MS} = mobile:start_link({BS1,[BS2]}),
    {ok,MS2} = mobile:start_link({BS2,[BS1]}),
    register(ms,MS),
    register(ms2,MS2),
    timer:apply_after(6000,?MODULE,startHand,[]).
