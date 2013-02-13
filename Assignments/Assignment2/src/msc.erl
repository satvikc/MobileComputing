-module(msc).
-compile(export_all).
-behaviour(gen_server).
-include("../include/constants.hrl").
-include("../include/records.hrl").
-export([start_link/0,init/1,handle_cast/2,handle_call/3,handle_info/2,code_change/3,terminate/2]).

start_link() ->
    gen_server:start_link(?MODULE,[],[]).

send_ho_command_newbsc(_,{A=#address{newbsc=BSC}}) ->
    io:format("[MSC ~p] send ho command to BSC ~p~n",[self(),BSC]), 
    gen_server:cast(BSC, {ho_command_newbsc,A#address{msc=self()}}).

send_ho_command_bsc(_,{A=#address{bsc=BSC},Ch}) ->
    io:format("[MSC ~p] send ho command to BSC ~p~n",[self(),BSC]), 
    gen_server:cast(BSC, {ho_command_bsc,A,Ch}).

%% For gen_server

init([]) ->
    {ok, #msc_state{}}.

handle_cast({ho_req_msc,A},S) ->
    send_ho_command_newbsc(S,A),
    {noreply,S};

handle_cast({ho_ack_msc_ok,A,Ch},S) ->
    send_ho_command_bsc(S,{A,Ch}),
    {noreply,S};

handle_cast({ho_ack_msc_fail,A},S) ->
    io:format("[MSC ~p] Call dropped for mobile ~p~n",[self(),A#address.ms]),
    {noreply,S}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_,S) ->
    {noreply, S}.

%% helper call to check state
handle_call({get,state},_,S) ->
    {reply,S,S}.
