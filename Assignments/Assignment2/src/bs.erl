-module(bs).
-compile(export_all).
-behaviour(gen_server).
-include("../include/constants.hrl").
-include("../include/records.hrl").
-export([start_link/1,init/1,handle_cast/2,handle_call/3,handle_info/2,code_change/3,terminate/2]).

start_link({BSC,Co,Nbh}) ->
    gen_server:start_link(?MODULE,[BSC,Co,Nbh],[]).

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

send_connect(BSC) ->
    C = gen_server:call(BSC,{connect,self()}),
    lists:split(length(C) div 2,C).

%% For gen_server

init([BSC,Co,Nbh]) ->
    {L,H} = send_connect(BSC),
    {ok,#bs_state{bsc=BSC,low=lists:sort(L),high=lists:sort(H),cochannel=set:from_list(Co),neighbours=set:from_list(Nbh)}}.

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
    {noreply,S};

handle_cast({allocatedChannels,C},S) ->
    io:format("[BS ~p] reassigning channels",[self()]),
    {H,L} = lists:split(length(C) div 2,C),
    {noreply,S#bs_state{low=lists:sort(L),high=lists:sort(H)}};

handle_cast({freeChannel,Ch},S=#bs_state{low=L,high=H,locked=Lck}) ->
    case dict:is_key(Ch,Lck) of
	true -> NLck = dict:erase(Ch,Lck),
		case dict:fetch(Ch,Lck) of
		    l -> {noreply,S#bs_state{low=lists:sort([Ch|L],locked=NLck)}};
		    h -> {noreply,S#bs_state{high=lists:sort([Ch|H]),locked=NLck}}
		end;
	false -> {noreply,S}
    end.

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

handle_call({borrowAvailable},_,S=#bs_state{cochannel=Co,high=H}) ->
    {reply,length(lists:filter(fun(X) -> isFreeAll(X,Co) end,H)),S};

handle_call({borrow},{BS,_},S=#bs_state{cochannel=Co,high=H,given=G}) ->
    case findFirst(fun(Ch) -> isFreeAll(Ch,Co) end,H) of
	{ok,Ch} -> case lockAll(Ch,Co) of
		       true -> NH = lists:delete(Ch,H),
			       NG = dict:append(BS,Ch,G),
			       {reply,{ok,Ch},S#bs_state{high=NH,given=NG}};
		       false -> {reply,{fail},S}
		   end;
	{fail} -> {reply,{fail},S}
    end;

handle_call({isfreeChannel,Ch},_,S) ->
    {reply,isFreeChannel(Ch,S),S};

handle_call({lockChannel,Ch},_,S=#bs_state{low=L,high=H,locked=Lck}) ->
    case isFreeChannel(Ch,S) of
	true -> case lists:member(Ch,L) of
		    true -> NL = lists:delete(Ch,L),
			    NLck = dict:append(Ch,l,Lck),
			    {reply,true,S#bs_state{low=NL,locked=NLck}};
		    false -> case lists:member(Ch,H) of
				 true -> NH = lists:delete(Ch,H),
					 NLck = dict:append(Ch,l,Lck),
					 {reply,true,S#bs_state{high=NH,locked=NLck}};
				 false -> NLck = dict:append(Ch,n,Lck),
					  {reply,true,S#bs_state{locked=NLck}}
			     end
		end;
	false -> {reply,false,S}
    end;

handle_call({get,state},_,S) ->
    {reply,S,S}.

%% Checks if channel is free in the cochannel
isFree(Ch,Co) ->
    gen_server:call(Co,{isfreeChannel,Ch}).

freeChannel(Ch,Co) ->
    gen_server:cast(Co,{freeChannel,Ch}).

isFreeAll(Ch,Co) ->
   lists:all(fun(X) -> isFree(Ch,X) end,Co).

lockAll(Ch,Co) ->
    Status = lists:map(fun(X) -> gen_server:call(X,{lockChannel,Ch}) end,Co),
    case lists:all(fun(X) -> X end,Status) of
	true -> true;
	false -> Z = lists:zip(Status,Co),
		 ToSend = lists:filter(fun({X,_}) -> X end,Z),
		 lists:foreach(fun(C) -> freeChannel(Ch,C) end,ToSend),
		 false
    end.

%% Checks if channel is free for locking
isFreeChannel(Ch,#bs_state{allocated=A,given=G,borrowed=B,locked=L}) ->
    not(isValue(Ch,A) or isValue(Ch,G) or isValue(Ch,B) or dict:is_key(Ch,L)).

%% for dictionaries
isValue(V,D) ->
    dict:size(dict:filter(fun(_,V1) -> V==V1 end,D)) /= 0.

%% Locks the Channel in the Cochannel cell
lock(Ch,Co) ->
    gen_server:call(Co,{lockChannel,Ch}).

%% Channel allocation
allocateChannel(MS,S=#bs_state{low=L,high=H,borrowed=B,neighbours=Nh,allocated=Alc}) ->
    case L of
	[] -> case fromNeighbour(Nh) of
		  {ok,Ch,N} -> NewS=S#bs_state{borrowed=[{MS,Ch,N}|B]},
			       {ok,Ch,NewS};
		  {fail}    -> {fail,S}
	      end;
	[H|T] -> NewS=S#bs_state{allocated=dict:store(MS,H,Alc),low=T},
		 {ok,H,NewS}
    end.

%% Channel Borrowing policy
fromNeighbour(Nh) ->
    C = lists:map(fun(X) -> countAvailable(X) end,Nh),
    {C,N} = lists:max(lists:zip(C,Nh)),
    case C of
	0 -> {fail};
	_ -> case borrow(N) of
		 {ok,C} -> {ok,C,N};
		 {fail} -> {fail}
	     end
    end.

%% Finds number of channels available for borrowing
countAvailable(Nh) ->
    gen_server:call(Nh,{borrowAvailable}).

%% Requests a channel for borrowing
borrow(Nh) ->
    gen_server:call(Nh,{borrow}).


%% Finds the first element satisfying the given predicate in the list
findFirst(_,[]) -> {fail};
findFirst(P,[X|Xs]) -> case P(X) of
			   true -> {ok,X};
			   false -> findFirst(P,Xs)
		       end.
