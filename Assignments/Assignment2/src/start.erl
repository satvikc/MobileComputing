-module(start).
-compile(export_all).
-import(mobile).
-import(bs).
-import(bsc).
-import(msc).
-export([start_normal/0]).

start_normal() ->
    {ok,MSC}  = msc:start_link(),
    register(msc,MSC),
    {ok,BSC1} = bsc:start_link({msc}),
    {ok,BSC2} = bsc:start_link({msc}),
    register(bsc1,BSC1),
    register(bsc2,BSC2),
    {ok,BS1} = bsc:start_link({bsc1}),
    {ok,BS2} = bsc:start_link({bsc2}),
    register(bs1,BS1),
    register(bs2,BS2),
    {ok,MS} = mobile:start_link({bs1,[bs2]}),
    register(ms,MS).
