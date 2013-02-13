-module(mobile).
-compile(export_all).
-behaviour(gen_server).
-include("../include/constants.hrl").
-include("../include/records.hrl").
-export([start_link/1,init/1,handle_cast/2,handle_info/2,handle_call/3,code_change/3,terminate/2]).

%% NOTE: handle measurements at MSC when already a handoff is in progress.
%% Define terminate to cancel timer.

start_link({Base,Bases}) ->
    gen_server:start_link(?MODULE,[Base,Bases],[]).

send_measurements(#ms_state{bs=BS,signals=Signals}) ->
    io:format("[Mobile ~p] send measurements to BS ~p ~n",[self(),BS]), 
    gen_server:cast(BS, {measurements,#address{ms=self()},?SACCH,takeTop(Signals,6)}).

send_link_active_request(A=#address{newbs=NewBS},Channel) ->
    io:format("[Mobile ~p] send link active request to ~p over channel ~p~n",[self(),NewBS,Channel]), 
    gen_server:cast(NewBS,{link_active_request,A,Channel}).

%% For gen_server

%% Gives random signal strength to all the bases with the basestation
%% with which the MS is connected having 100 signal strength.
init([Base,Bases]) ->
    SignalList = lists:map(fun(L) -> {L,random_number(100)} end,Bases),
    Signals = dict:from_list(SignalList),
    S = #ms_state{bs=Base,signals=dict:store(Base,100,Signals)},
    Channel = gen_server:call(Base,{getChannel,self()}),
    case Channel of
	none -> io:format("[Mobile ~p] Can not initiate call so terminating~n",[self()]),
		{stop,"No Empty Channel"};
	Ch ->   {ok,TRef} = timer:send_after(?MEASURETIME,{timer,measuretime}),
		{ok, S#ms_state{tref=TRef,tch=Ch}}
    end.

%% Link active received. Update the BS to new BS and send link active
%% request to the new BS
handle_cast({link_active,A,ACh,Ch},S) ->
    io:format("[Mobile ~p] received link active on channel ~p~n",[self(),ACh]), 
    send_link_active_request(A,Ch),
    {noreply,S#ms_state{bs=A#address.newbs}};

%% Link establishment successful so call can continue on the new TCH.
handle_cast({link_establishment,#address{newbs=NewBS},_},S) ->
    io:format("[MS ~p] Handover complete to BS ~p~n",[self(),NewBS]), 
    {noreply,S#ms_state{bs=NewBS}}.

%% helper call to check state
handle_call({get,state},_,S) ->
    {reply,S,S}.


%% For timer events to send measurements every 480ms.
handle_info({timer,measuretime},S) ->
    send_measurements(S),
    {ok,TRef} = timer:send_after(?MEASURETIME,{timer,measuretime}),
    {noreply,S#ms_state{tref=TRef}};

handle_info({From,get,state},S) ->
    From ! S,
    {noreply,S};

handle_info({_,set,signal,Val},S=#ms_state{bs=BS,signals=Sig}) ->
    NewS = S#ms_state{signals=dict:store(BS,Val,Sig)}, 
    {noreply,NewS}.

terminate(_Reason,#ms_state{tref=TRef}) ->
    _ = timer:cancel(TRef),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Generates a random number in [1,Bound].
random_number(Bound) ->
    random:seed(erlang:now()),
    random:uniform(Bound).

takeTop(D,N) ->
    PairList = dict:to_list(D),
    ValueList = lists:sort(fun ({_,A},{_,B}) -> A =< B end,PairList),
    dict:from_list(lists:sublist(ValueList,N)).
