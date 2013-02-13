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
    io:format("[Mobile ~p] send measurements ~p to ~p~n",[self(),Signals,BS]), 
    gen_server:cast(BS, {measurements,?SACCH,#address{ms=self()},takeTop(Signals,6)}).

send_link_active_request(A=#address{newbs=NewBS},Channel) ->
    io:format("[Mobile ~p] send link active request to ~p over channel ~p~n",[self(),NewBS,Channel]), 
    gen_server:call(NewBS,{link_active_request,A,Channel}).

%% For gen_server

%% Gives random signal strength to all the bases with the basestation
%% with which the MS is connected having 100 signal strength.
init([Base,Bases]) ->
    SignalList = lists:map(fun(L) -> {L,random_number(100)} end,Bases),
    Signals = dict:from_list(SignalList),
    S = #ms_state{bs=Base,signals=dict:store(Base,100,Signals)},
    {ok,TRef} = timer:apply_after(?MEASURETIME,?MODULE,send_measurements,[S]),
    {ok, S#ms_state{tref=TRef}}.

%% Link active received. Update the BS to new BS and send link active
%% request to the new BS
handle_cast({link_active,A,Ch},S) ->
    send_link_active_request(A,Ch),
    {noreply,S#ms_state{bs=A#address.newbs}};

%% Link establishment successful so call can continue on the new TCH.
handle_cast({list_establishment,_,_},S) ->
    io:format("[MS ~p] Handover complete~n",[self()]), 
    {noreply,S}.

%% helper call to check state
handle_call({get,state},_,S) ->
    {reply,S,S}.


%% For timer events to send measurements every 480ms.
handle_info({ok,TRef},S) ->
    {ok,TRef} = timer:apply_after(?MEASURETIME,?MODULE,send_measurements,[S]),
    {noreply,S#ms_state{tref=TRef}};

handle_info({From,get,state},S) ->
    From ! S,
    {noreply,S};

handle_info({_,change,signal,Val},S=#ms_state{bs=BS,signals=Sig}) ->
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
