-module(bs).
-compile(export_all).
-behaviour(gen_server).
-include("../include/constants.hrl").
-include("../include/records.hrl").
-export([start_link/1,init/1,handle_cast/2]).

%% bsc = base station controller of this base station
%% allocated = dict of key = channel id and value = mobile station.
%% channels = list of available channels
-record(state, {bsc,allocated=none,channels=[]}).

start_link({BSC,Channels}) ->
    gen_server:start_link(?MODULE,[{BSC,Channels}],[]).

send_ho_req_bs(#state{bsc=BSC},{A,Measurements}) ->
    gen_server:cast(BSC, {ho_request_bs,A#address{bs=self()},Measurements}).

send_link_active(S,A=#address{ms=MS}) ->
    gen_server:cast(MS,{link_active,A}).

send_activation(#state{bsc=BSC},A) ->
    gen_server:cast(BSC,{activation,A#address{newbs=self()}}).

send_link_establishment(S=#state{allocated=Alc,channels=Chs},A=#address{ms=MS}) ->
    case Chs of
	[] -> gen_server:cast(MS,{link_establishment_fail,A}),
	      S#state{channels=[]};
	[H|T] -> gen_server:cast(MS,{link_establishment_ok,H}),
		 S#state{allocated=dict:store(H,MS,Alc),channels=T}
    end.

%% For gen_server

init({BSC,Channels}) ->
    {ok, #state{bsc=BSC,channels=Channels}}.

handle_cast({measurements,A,List},S) ->
    send_ho_req_bs(S,{A,List}),
    {noreply,S};

handle_cast({ho_command_bsc,A},S) ->
    send_activation(S,A),
    {noreply,S};

handle_cast({link_active_request,A},S) ->
    NewState = send_link_establishment(S,A),
    {noreply,NewState};

handle_cast({ho_ack,A},S) ->
    send_link_active(S,A),
    {noreply,S}.
