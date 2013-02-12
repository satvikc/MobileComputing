-module(bsc).
-compile(export_all).
-behaviour(gen_server).
-include("../include/constants.hrl").
-include("../include/records.hrl").
-export([start_link/1,init/1,handle_cast/2]).

%% bsc = base station controller of this base station
%% allocated = dict of key = channel id and value = mobile station.
%% channels = list of available channels
-record(state, {msc,bscs=none}).

start_link({MSC}) ->
    gen_server:start_link(?MODULE,[{MSC}],[]).

send_ho_req_msc(S,{A#address{msc=MSC},Measurements}) ->
    gen_server:cast(MSC, {ho_req_msc,Address#address{bs=self()},Measurements}).

send_ho_command_bs(S,{A#address{newbs=BS}}) ->
    gen_server:cast(BS, {ho_command_bs,Address#address{bs=self()}}).

send_ho_ack_bs(S,{A#address{bs=BS}}) ->
    gen_server:cast(BS, {ho_ack_bs,Address#address{bs=self()}}).

send_ho_ack_msc(S,{A#address{msc=MSC}}) ->
    gen_server:cast(MSC, {ho_ack_bs,Address#address{bs=self()}}).


%% For gen_server

init({MSC}) ->
    {ok, #state{msc=MSC}}.

handle_cast({measurements,A,List},S) ->
    send_ho_req_bs(S,{A,List}),
    {noreply,S};

handle_cast({ho_command_bsc,A},S) ->
    send_activation(S,A),
    {noreply,S};

handle_cast({link_active_request,#address{ms=MS}},S) ->
    NewState = send_link_establishment(MS,S),
    {noreply,NewState};

handle_cast({ho_ack,A,NewA},S) ->
    send_link_active(A,NewA),
    {noreply,S}.


	     
