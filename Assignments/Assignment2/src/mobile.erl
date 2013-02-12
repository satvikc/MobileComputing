-module(mobile).
-compile(export_all).
-behaviour(gen_server).
-include("../include/constants.hrl").
-include("../include/records.hrl").
-export([start_link/1,init/1,handle_cast/2,handle_info/2]).

-record(state, {bs,signals,tch=none,tref=none}).

%% NOTE: handle measurements at MSC when already a handoff is in progress.
%% Define terminate to cancel timer.

start_link({Base,Bases}) ->
    gen_server:start_link(?MODULE,[{Base,Bases}],[]).

send_measurements(#state{bs=BS,signals=Signals}) ->
    gen_server:cast(BS, {measurements,#address{ms=self()},list:sublist(Signals,6)}).

send_link_active_request(A=#address{newbs=NewBS}) ->
    gen_server:call(NewBS,{link_active_request,A).

%% For gen_server

%% Gives random signal strength to all the bases with the basestation
%% with which the MS is connected having 100 signal strength.
init({Base,Bases}) ->
    SignalList = lists:map(fun(L) -> {L,random_number(100)} end,Bases),
    Signals = dict:from_list(SignalList),
    S = #state{bs=Base,signals=dict:store(Base,100,Signals)},
    {ok,TRef} = timer:apply_after(480,?MODULE,send_measurements,[S]),
    {ok, S#state{tref=TRef}}.

%% Link active received. Update the BS to new BS and send link active
%% request to the new BS
handle_cast({link_active,A},S) ->
    send_link_active_request(A),
    {noreply,S#state{bs=NewBS}};

%% Link establishment successful so call can continue on the new TCH.
handle_cast({list_establishment_ok,A,Channel},S) ->
    {noreply,S#state{tch=Channel}};

%% Link establishment has failed to call has to be dropped.
handle_cast({link_establishment_error,A},S) ->
    io:format("[MS ~p]: handoff failed",[self()]),
    {noreply,S#state{tch=none}}.

%% For timer events to send measurements every 480ms.
handle_info({ok,TRef},S) ->
    {ok,TRef} = timer:apply_after(480,?MODULE,send_measurements,[S]),
    {noreply,S#state{tref=TRef}}.

%% Generates a random number in [1,Bound].
random_number(Bound) ->
    random:seed(erlang:now()),
    random:uniform(Bound).
