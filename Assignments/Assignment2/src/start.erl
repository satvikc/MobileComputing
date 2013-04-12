-module(start).
-compile(export_all).
-import(mobile).
-import(bs).
-import(bsc).
-import(msc).
-export([start_normal/0,state/1,start_intra/0]).


%% Implement flush
start_normal() ->
    {ok,MSC}  = msc:start_link(),
    register(msc,MSC),				
    {ok,BSC1} = bsc:start_link({MSC}),
    {ok,BSC2} = bsc:start_link({MSC}),
    register(bsc1,BSC1),
    register(bsc2,BSC2),
    {ok,BS1} = bs:start_link({BSC1,[],[]}),
    {ok,BS2} = bs:start_link({BSC2,[],[]}),
    register(bs1,BS1),
    register(bs2,BS2),
    {ok,MS} = mobile:start_link({BS1,[BS2]}),
    register(ms,MS),
    ms ! {startcall},
    timer:apply_after(6000,?MODULE,startHand,[]).

start_intra() ->
    {ok,MSC}  = msc:start_link(),
    register(msc,MSC),				
    {ok,BSC1} = bsc:start_link({MSC}),
    register(bsc1,BSC1),
    {ok,BS1} = bs:start_link({BSC1,[],[]}),
    {ok,BS2} = bs:start_link({BSC1,[],[]}),
    register(bs1,BS1),
    register(bs2,BS2),
    {ok,MS} = mobile:start_link({BS1,[BS2]}),
    register(ms,MS),
    ms ! {startcall},
    timer:apply_after(6000,?MODULE,startHand,[]).

startHand() ->
    ms ! {self(),set,signal,0}.

endcall() ->
    io:format("Ending call on ms2~n"),
    ms2 ! {endcall},
    io:format("Ending call on ms~n"),
    ms  ! {endcall}.

endcallf() ->
    io:format("Ending call on ms2~n"),
    ms2 ! {endcall},
    io:format("Ending call on ms~n"),
    ms  ! {endcall},
    io:format("Ending call on ms~n"),
    ms3  ! {endcall}.


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

%% Switching rule 2
start_switch2() ->
    {ok,MSC}  = msc:start_link(),
    register(msc,MSC),				
    {ok,BSC1} = bsc:start_link({MSC}),
    {ok,BSC2} = bsc:start_link({MSC}),
    register(bsc1,BSC1),
    register(bsc2,BSC2),
    {ok,BS1} = bs:start_link({BSC1,[],[]}),
    {ok,BS11} = bs:start_link({BSC1,[],[]}),
    {ok,BS22} = bs:start_link({BSC2,[BS11],[]}),
    {ok,BS2} = bs:start_link({BSC2,[BS1],[BS22]}),
    register(bs1,BS1),
    register(bs2,BS2),
    register(bs11,BS11),
    register(bs22,BS22),
    {ok,MS} = mobile:start_link({BS1,[BS2]}),
    {ok,MS2} = mobile:start_link({BS2,[BS1]}),
    register(ms,MS),
    ms ! {startcall},
    register(ms2,MS2),
    ms2 ! {startcall},
    timer:apply_after(6000,?MODULE,startHand,[]).

start_switch1() ->
    {ok,MSC}  = msc:start_link(),
    register(msc,MSC),				
    {ok,BSC1} = bsc:start_link({MSC}),
    {ok,BSC2} = bsc:start_link({MSC}),
    register(bsc1,BSC1),
    register(bsc2,BSC2),
    {ok,BS1} = bs:start_link({BSC1,[],[]}),
    {ok,BS2} = bs:start_link({BSC2,[],[]}),
    register(bs1,BS1),
    register(bs2,BS2),
    {ok,MS} = mobile:start_link({BS1,[BS2]}),
    {ok,MS2} = mobile:start_link({BS2,[BS1]}),
    register(ms,MS),
    ms ! {startcall},
    register(ms2,MS2),
    ms2 ! {startcall},
    timer:apply_after(6000,?MODULE,startHand,[]).

%% Switching rule 3
start_switch3() ->
    {ok,MSC}  = msc:start_link(),
    register(msc,MSC),				
    {ok,BSC1} = bsc:start_link({MSC}),
    {ok,BSC2} = bsc:start_link({MSC}),
    {ok,BSC3} = bsc:start_link({MSC}),
    {ok,BSC4} = bsc:start_link({MSC}),
    register(bsc1,BSC1),
    register(bsc2,BSC2),
    register(bsc3,BSC3),
    register(bsc4,BSC4),
    {ok,BS1} = bs:start_link({BSC1,[],[]}),
    {ok,BS4} = bs:start_link({BSC4,[],[]}),
    {ok,BS22} = bs:start_link({BSC2,[],[]}),
    {ok,BS2} = bs:start_link({BSC2,[],[BS4]}),
    {ok,BS3} = bs:start_link({BSC3,[],[]}),
    register(bs1,BS1),
    register(bs2,BS2),
    register(bs3,BS3),
    register(bs4,BS4),
    register(bs22,BS22),
    {ok,MS} = mobile:start_link({BS1,[BS2]}),
    {ok,MS2} = mobile:start_link({BS2,[BS1]}),
    {ok,MS3} = mobile:start_link({BS3,[BS2]}),
    register(ms,MS),
    ms ! {startcall},
    register(ms2,MS2),
    ms2 ! {startcall},
    register(ms3,MS3),
    ms3 ! {startcall},
    timer:apply_after(6000,?MODULE,startHand,[]),
    timer:apply_after(12000,?MODULE,startHand3,[]).


startHand3() ->
    ms3 ! {self(),set,signal,0}.

endc() ->
    io:format("Ending call on ms~n"),
    ms ! {endcall}.

endc2() ->
    io:format("Ending call on ms2~n"),
    ms2 ! {endcall}.

endc3() ->
    io:format("Ending call on ms3~n"),
    ms3 ! {endcall}.

endc4() ->
    io:format("Ending call on ms4~n"),
    ms4 ! {endcall}.
