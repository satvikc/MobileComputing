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

send_ho_req_bsc(#state{bsc=BSC},{Address,Measurements}) ->
    gen_server:cast(BSC, {ho_request_bs,Address#address{bs=self()},Measurements}).

send_ho_command(#state{bsc=BSC},{Address,Measurements}) ->
    gen_server:cast(BSC, {ho_request_bs,Address#address{bs=self()},Measurements}).

send_ho_ack_tobs(#state{bsc=BSC},{Address,Measurements}) ->
    gen_server:cast(BSC, {ho_request_bs,Address#address{bs=self()},Measurements}).

send_ho_req_bsc(#state{bsc=BSC},{Address,Measurements}) ->
    gen_server:cast(BSC, {ho_request_bs,Address#address{bs=self()},Measurements}).


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


	     
