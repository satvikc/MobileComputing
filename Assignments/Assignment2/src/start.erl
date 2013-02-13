-module(start).
-compile(export_all).
-import(mobile).
-import(bs).
-import(bsc).
-import(msc).
-export([start_normal/0]).


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
