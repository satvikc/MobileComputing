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

send_ho_req_bs(#state{bsc=BSC},{Address,Measurements}) ->
    gen_server:cast(BSC, {ho_request_bs,Address#address{bs=self()},Measurements}).

send_link_active(#address{ms=MS},NewAddress) ->
    gen_server:cast(MS,{link_active,NewAddress}).

send_activation(#state{bsc=BSC},Address) ->
    gen_server:cast(BSC,{activation,Address,#address{bs=self()}}).

send_link_establishment(MS,S=#state{allocated=A,channels=Chs}) ->
    case Chs of
	[] -> gen_server:cast(MS,{link_establishment_fail}),
	      S#state{channels=[]};
	[H|T] -> gen_server:cast(MS,{link_establishment_ok,H}),
		 S#state{allocated=dict:store(H,MS,A),channels=T}
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

handle_cast({link_active_request,#address{ms=MS}},S) ->
    NewState = send_link_establishment(MS,S),
    {noreply,NewState};

handle_cast({ho_ack,A,NewA},S) ->
    send_link_active(A,NewA),
    {noreply,S}.


	     
