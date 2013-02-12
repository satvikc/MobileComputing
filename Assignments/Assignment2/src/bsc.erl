-module(bsc).
-compile(export_all).
-behaviour(gen_server).
-include("../include/constants.hrl").
-include("../include/records.hrl").
-export([start_link/1,init/1,handle_cast/2,handle_call/3]).

start_link({MSC}) ->
    gen_server:start_link(?MODULE,[{MSC}],[]).

send_ho_req_msc(_,{A=#address{msc=MSC}}) ->
    gen_server:cast(MSC, {ho_req_msc,A#address{bsc=self()}}).

send_ho_command_newbs(_,{A=#address{newbs=BS}}) ->
    gen_server:cast(BS, {ho_command_newbs,A}).

send_ho_ack_bs(_,{A=#address{bs=BS},Ch}) ->
    gen_server:cast(BS, {ho_ack_bs,A,Ch}).

send_ho_ack_msc_ok(_,{A=#address{msc=MSC},Ch}) ->
    gen_server:cast(MSC, {ho_ack_msc_ok,A,Ch}).

send_ho_ack_msc_fail(_,{A=#address{msc=MSC}}) ->
    gen_server:cast(MSC, {ho_ack_msc_fail,A}).

%% For gen_server
init({MSC}) ->
    {ok, #bsc_state{msc=MSC,channels=list:seq(0,97)}}.

handle_cast({ho_req_bsc,A,Dict},S) ->
    case takeDecision(A,Dict,S) of
	none -> ok;
	A -> send_ho_req_msc(S,{A})
    end,
    {noreply,S};

handle_cast({ho_command_newbsc,A},S) ->
    send_ho_command_newbs(S,A),
    {noreply,S};

handle_cast({activation,A=#address{ms=MS}},S=#bsc_state{channels=Chs,allocated=Alc}) ->
    NewS=case Chs of
	[] -> send_ho_ack_msc_fail(S,A),
	      S#bsc_state{channels=[]};
	[H|T] -> send_ho_ack_msc_ok(S,{A,H}),
		 S#bsc_state{allocated=dict:store(H,MS,Alc),channels=T}
    end,
    {noreply,NewS};

handle_cast({ho_command_bsc,A,Ch},S) ->
    send_ho_ack_bs(S,{A,Ch}),
    {noreply,S}.

%% helper call to check state
handle_call({get,state},_,S) ->
    {reply,S,S}.


takeDecision(A=#address{bs=BS},Dict,_) ->
    case maxInDict(Dict) of
	BS  -> none;
	NBS -> #bs_state{bsc=NewBSC} = gen_server:cast(NBS,{get,state}),
	       A#address{newbsc=NewBSC,newbs=NBS}
    end.

maxInDict(D) ->
    dict:fold(fun (_,V,A) -> if V > A -> V;
				true  -> A
			     end end,-1,D).				     
		      
