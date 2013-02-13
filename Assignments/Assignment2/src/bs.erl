-module(bs).
-compile(export_all).
-behaviour(gen_server).
-include("../include/constants.hrl").
-include("../include/records.hrl").
-export([start_link/1,init/1,handle_cast/2,handle_call/3,handle_info/2,code_change/3,terminate/2]).

start_link({BSC}) ->
    gen_server:start_link(?MODULE,[BSC],[]).

send_ho_req_bsc(#bs_state{bsc=BSC},{A,Measurements}) ->
    io:format("[BS ~p] send measurements to BSC ~p ~n",[self(),BSC]), 
    gen_server:cast(BSC, {ho_req_bsc,A#address{bs=self()},Measurements}).

send_link_active(_,{A=#address{ms=MS},Ch}) ->
    io:format("[BS ~p] send link active to MS ~p on channel ~p~n",[self(),MS,?AGCH]), 
    gen_server:cast(MS,{link_active,A,?AGCH,Ch}).

send_activation(#bs_state{bsc=BSC},A) ->
    io:format("[BS ~p] send activation to new BSC ~p ~n",[self(),BSC]), 
    gen_server:cast(BSC,{activation,A#address{newbs=self()}}).

send_link_establishment(S,{A=#address{ms=MS},Ch}) ->
    io:format("[BS ~p] send link establishment to MS ~p on channel ~p state ~p~n",[self(),MS,Ch,S]), 
    gen_server:cast(MS,{link_establishment,A,Ch}).

send_ho_conn_newbsc(#bs_state{bsc=BSC},A) ->
    io:format("[BS ~p] send ho conn to new BSC ~p ~n",[self(),BSC]), 
    gen_server:cast(BSC,{ho_conn_newbsc,A#address{newbs=self()}}).

send_flush(#bs_state{bsc=BSC},{A}) ->
    io:format("[BS ~p] send flush to BSC ~p ~n",[self(),BSC]), 
    gen_server:cast(BSC, {flush,A#address{bs=self()}}).


%% For gen_server

init([BSC]) -> {ok, #bs_state{bsc=BSC}}.

handle_cast({measurements,A,Ch,List},S) ->
    io:format("[BS ~p] received measurements on channel ~p~n",[self(),Ch]), 
    send_ho_req_bsc(S,{A,List}),
    {noreply,S};

handle_cast({ho_command_newbs,A},S) ->
    io:format("[BS ~p] received ho command newbs~n",[self()]), 
    send_activation(S,A),
    {noreply,S};

handle_cast({link_active_request,A,Ch},S) ->
    io:format("[BS ~p] received link_active_request~n",[self()]), 
    send_link_establishment(S,{A,Ch}),
    send_ho_conn_newbsc(S,A),
    {noreply,S};

handle_cast({ho_ack_bs,A,Ch},S) ->
    io:format("[BS ~p] received ho ack bs~n",[self()]), 
    send_link_active(S,{A,Ch}),
    {noreply,S};

handle_cast({flush,A},S) ->
    io:format("[BS ~p] received flush~n",[self()]), 
    send_flush(S,{A}),
    {noreply,S}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({From,get,state},S) ->
    From ! S,
    {noreply,S};

handle_info(_,S) ->
    {noreply,S}.

%% helper call to check state
handle_call({getChannel,MS},_,S=#bs_state{bsc=BSC}) ->
    Channel = gen_server:call(BSC,{getChannel,MS}),
    {reply,Channel,S};
handle_call({get,state},_,S) ->
    {reply,S,S}.
