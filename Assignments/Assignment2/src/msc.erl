-module(msc).
-compile(export_all).
-behaviour(gen_server).
-include("../include/constants.hrl").
-include("../include/records.hrl").
-export([start_link/0,init/1,handle_cast/2,handle_call/3]).

start_link() ->
    gen_server:start_link(?MODULE,[],[]).

send_ho_command_newbsc(_,{A=#address{newbsc=BSC}}) ->
    gen_server:cast(BSC, {ho_command_newbsc,A#address{msc=self()}}).

send_ho_command_bsc(_,{A=#address{bsc=BSC},Ch}) ->
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
    io:format("[MSC] Call dropped for mobile ~p",[A#address.ms]),
    {noreply,S}.

%% helper call to check state
handle_call({get,state},_,S) ->
    {reply,S,S}.
