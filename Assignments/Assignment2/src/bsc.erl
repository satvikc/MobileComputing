-module(bsc).
-compile(export_all).
-behaviour(gen_server).
-include("../include/constants.hrl").
-include("../include/records.hrl").
-export([start_link/1,init/1,handle_cast/2,handle_call/3,handle_info/2,code_change/3,terminate/2]).

start_link({MSC}) ->
    gen_server:start_link(?MODULE,[MSC],[]).

send_ho_req_msc(_,{A=#address{msc=MSC}}) ->
    io:format("[BSC ~p] send ho request to MSC ~p~n",[self(),MSC]), 
    gen_server:cast(MSC, {ho_req_msc,A#address{bsc=self()}}).

send_ho_command_newbs(_,{A=#address{newbs=BS}}) ->
    io:format("[BSC ~p] send ho command to new BS ~p~n",[self(),BS]), 
    gen_server:cast(BS, {ho_command_newbs,A}).

send_ho_ack_bs(_,{A=#address{bs=BS},Ch}) ->
    io:format("[BSC ~p] send ho ack to BS ~p~n",[self(),BS]), 
    gen_server:cast(BS, {ho_ack_bs,A,Ch}).

send_ho_ack_msc_ok(_,{A=#address{msc=MSC},Ch}) ->
    io:format("[BSC ~p] send ho ack ok to MSC ~p~n",[self(),MSC]), 
    gen_server:cast(MSC, {ho_ack_msc_ok,A,Ch}).

send_ho_ack_msc_fail(_,{A=#address{msc=MSC}}) ->
    io:format("[BSC ~p] send ho ack fail to MSC ~p~n",[self(),MSC]), 
    gen_server:cast(MSC, {ho_ack_msc_fail,A}).

send_ho_conn_msc(_,{A=#address{msc=MSC}}) ->
    io:format("[BSC ~p] send ho conn to MSC ~p~n",[self(),MSC]), 
    gen_server:cast(MSC, {ho_conn_msc,A}).

send_flush(_,{A=#address{bs=BS}}) ->
    io:format("[BSC ~p] send flush to BS ~p~n",[self(),BS]), 
    gen_server:cast(BS, {flush,A}).

send_flush_msc(_,{A=#address{msc=MSC}}) ->
    io:format("[BSC ~p] send flush to MSC ~p~n",[self(),MSC]), 
    gen_server:cast(MSC, {flush,A}).

%% For gen_server
init([MSC]) ->
    {ok, #bsc_state{msc=MSC,channels=lists:seq(0,?MAXCHANNELS-1)}}.

handle_cast({ho_req_bsc,A=#address{ms=MS},Dict},S=#bsc_state{inhandoff=In}) ->
    io:format("[BSC ~p] received ho req bsc~n",[self()]), 
    NewS = case dict:is_key(MS,In) of
	       true  -> S;
	       false -> case takeDecision(A#address{bsc=self()},Dict,S) of
			    none -> S;
			    Ad -> New = S#bsc_state{inhandoff=dict:store(MS,true,In)}, 
				 send_ho_req_msc(New,{Ad}),
				 New
			end
    end,
    {noreply,NewS};

handle_cast({ho_command_newbsc,A},S) ->
    io:format("[BSC ~p] received ho command newbsc~n",[self()]), 
    send_ho_command_newbs(S,{A}),
    {noreply,S};

handle_cast({activation,A=#address{ms=MS}},S) ->
    io:format("[BSC ~p] received activation~n",[self()]), 
    case findChannel(MS,S) of
	{fail,NS} -> send_ho_ack_msc_fail(S,{A}),
		     {noreply,NS};
	{ok,H,NS} -> send_ho_ack_msc_ok(NS,{A,H}),
		     {noreply,NS}
    end;

handle_cast({ho_command_bsc,A,Ch},S) ->
    io:format("[BSC ~p] received ho command bsc~n",[self()]), 
    send_ho_ack_bs(S,{A,Ch}),
    {noreply,S};

handle_cast({ho_conn_newbsc,A},S) ->
    io:format("[BSC ~p] received ho conn newbsc~n",[self()]), 
    send_ho_conn_msc(S,{A}),
    {noreply,S};

handle_cast({ho_conn_bsc,A},S) ->
    io:format("[BSC ~p] received ho conn bsc~n",[self()]), 
    send_flush(S,{A}),
    {noreply,S};

handle_cast({flush,A=#address{ms=MS}},S=#bsc_state{channels=C,allocated=Alc,inhandoff=In}) ->
    io:format("[BSC ~p] received flush ~n",[self()]),
    Occ = dict:fetch(MS,Alc),
    NAlc = dict:erase(MS,Alc),
    NC = [Occ|C],
    NIn = dict:erase(MS,In),
    NewS = S#bsc_state{channels=NC,allocated=NAlc,inhandoff=NIn},
    send_flush_msc(NewS,{A}),
    {noreply,NewS}.

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
handle_call({getChannel,MS},_,S) ->
    case findChannel(MS,S) of
	{fail,NS} -> {reply,none,NS};
	{ok,H,NewS} -> {reply,H,NewS}
    end;

handle_call({get,state},_,S) ->
    {reply,S,S}.


takeDecision(A=#address{bs=BS},Dict,#bsc_state{msc=MSC}) ->
    case maxInDict(Dict) of
	BS  -> none;
	NBS ->
	    io:format("[BSC ~p] max ~p where as BS is ~p So handoff is needed.~n",[self(),NBS,BS]),   
	    St = gen_server:call(NBS,{get,state}),
	    #bs_state{bsc=NewBSC} = St,
	    A#address{newbsc=NewBSC,newbs=NBS,msc=MSC}
    end.

maxInDict(D) ->
    {BS,_} = dict:fold(fun (K,V,{OK,OV}) -> if V > OV -> {K,V};
					       true   -> {OK,OV}
					    end end,{none,-1},D),				     
    BS.
		      

findChannel(MS,S=#bsc_state{channels=Chs,allocated=Alc}) ->
    case Chs of
	[] -> {fail,S};
	[H|T] -> NewS=S#bsc_state{allocated=dict:store(MS,H,Alc),channels=T},
		 {ok,H,NewS}
    end.
