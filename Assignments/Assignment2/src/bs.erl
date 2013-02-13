-module(bs).
-compile(export_all).
-behaviour(gen_server).
-include("../include/constants.hrl").
-include("../include/records.hrl").
-export([start_link/1,init/1,handle_cast/2,handle_call/3,handle_info/2,code_change/3,terminate/2]).

start_link({BSC}) ->
    gen_server:start_link(?MODULE,[BSC],[]).

send_ho_req_bsc(#bs_state{bsc=BSC},{A,Measurements}) ->
    io:format("[BS ~p] send measurements ~p to ~p ~n",[self(),Measurements,BSC]), 
    gen_server:cast(BSC, {ho_req_bsc,A#address{bs=self()},Measurements}).

send_link_active(_,{A=#address{ms=MS},Ch}) ->
    io:format("[BS ~p] send link active to ~p on channel ~p~n",[self(),MS,Ch]), 
    gen_server:cast(MS,{link_active,A,Ch}).

send_activation(#bs_state{bsc=BSC},A) ->
    io:format("[BS ~p] send activation to BSC ~p ~n",[self(),BSC]), 
    gen_server:cast(BSC,{activation,A#address{newbs=self()}}).

send_link_establishment(_,{A=#address{ms=MS},Ch}) ->
    io:format("[BS ~p] send link establishment to MS ~p on channel ~p~n",[self(),MS,Ch]), 
    gen_server:cast(MS,{link_establishment,A,Ch}).
%% For gen_server

init([BSC]) -> {ok, #bs_state{bsc=BSC}}.

handle_cast({measurements,A,List},S) ->
    send_ho_req_bsc(S,{A,List}),
    {noreply,S};

handle_cast({ho_command_newbs,A},S) ->
    send_activation(S,A),
    {noreply,S};

handle_cast({link_active_request,A,Ch},S) ->
    NewState = send_link_establishment(S,{A,Ch}),
    {noreply,NewState};

handle_cast({ho_ack_bs,A,Ch},S) ->
    send_link_active(S,{A,Ch}),
    {noreply,S}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_,S) ->
    {noreply,S}.

%% helper call to check state
handle_call({get,state},_,S) ->
    {reply,S,S}.
