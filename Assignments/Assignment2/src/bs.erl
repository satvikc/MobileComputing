-module(bs).
-compile(export_all).
-behaviour(gen_server).
-include("../include/constants.hrl").
-include("../include/records.hrl").
-export([start_link/1,init/1,handle_cast/2,handle_call/3,handle_info/2,code_change/3,terminate/2]).

start_link({BSC,Co,Nb}) ->
    gen_server:start_link(?MODULE,[BSC,Co,Nb],[]).

send_ho_req_bsc(#bs_state{bsc=BSC},{A,Measurements}) ->
    io:format("[BS ~p] send measurements to BSC ~p ~n",[self(),BSC]),
    gen_server:cast(BSC, {ho_req_bsc,A#address{bs=self()},Measurements}).

send_link_active(_,{A=#address{ms=MS},Ch}) ->
    io:format("[BS ~p] send link active to MS ~p on channel ~p~n",[self(),MS,?AGCH]),
    gen_server:cast(MS,{link_active,A,?AGCH,Ch}).

send_activation(#bs_state{bsc=BSC},{A,Ch}) ->
    io:format("[BS ~p] send activation to new BSC ~p ~n",[self(),BSC]),
    gen_server:cast(BSC,{activation,A#address{newbs=self()},Ch}).

send_link_establishment(_,{A=#address{ms=MS},Ch}) ->
    io:format("[BS ~p] send link establishment to MS ~p on channel ~p~n",[self(),MS,Ch]),
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
    {ok,#bs_state{bsc=BSC,low=lists:sort(L),high=lists:sort(H),cochannel=Co,neighbours=Nbh}}.

handle_cast({measurements,A,Ch,List},S) ->
    io:format("[BS ~p] received measurements on channel ~p~n",[self(),Ch]),
    send_ho_req_bsc(S,{A,List}),
    {noreply,S};

handle_cast({ho_command_newbs,A=#address{ms=MS}},S) ->
    io:format("[BS ~p] received ho command newbs~n",[self()]),
    case allocateChannel(MS,S) of
	{ok,Ch,NS} -> send_activation(S,{A,Ch});
	{fail,NS}  -> send_activation(S,{A,none})
    end,
    {noreply,NS};

handle_cast({link_active_request,A,Ch},S) ->
    io:format("[BS ~p] received link_active_request~n",[self()]),
    send_link_establishment(S,{A,Ch}),
    send_ho_conn_newbsc(S,A),
    {noreply,S};

handle_cast({ho_ack_bs,A,Ch},S) ->
    io:format("[BS ~p] received ho ack bs~n",[self()]),
    send_link_active(S,{A,Ch}),
    {noreply,S};

handle_cast({flush,A=#address{ms=MS}},S=#bs_state{allocated=Alc}) ->
    io:format("[BS ~p] received flush~n",[self()]),
    NS = case dict:find(MS,Alc) of 
	     error -> S;
	     Ch    -> endcall(MS,Ch,S)
	 end,
    send_flush(S,{A}),
    {noreply,NS};

handle_cast({allocatedChannels,C},S) ->
    io:format("[BS ~p] reassigning channels~n",[self()]),
    {H,L} = lists:split(length(C) div 2,C),
    {noreply,S#bs_state{low=lists:sort(L),high=lists:sort(H)}};

handle_cast({unlockChannel,Ch,BS},S=#bs_state{high=H,locked=Lck}) ->
    io:format("[BS ~p] Unlock Channel ~p for BS ~p~n",[self(),Ch,BS]),
    case dict:is_key(Ch,Lck) of
	true -> case removeLock(Ch,BS,Lck) of
		    {complete,l,NLck} ->
			io:format("[BS ~p] Switching Rule 4 applied using switchin rule 2~n",[self()]),
			NS= switchingRule2(Ch,S),
			{noreply,NS#bs_state{locked=NLck}};
		    {complete,h,NLck} ->
			{noreply,S#bs_state{high=lists:sort([Ch|H]),locked=NLck}};
		    {complete,_,NLck} ->
			{noreply,S#bs_state{locked=NLck}};
		    {partial,NLck}    ->
			{noreply,S#bs_state{locked=NLck}}
		end;
	false -> {noreply,S}
    end;


handle_cast({endcall,Ch,MS},S) ->
    NS = endcall(MS,Ch,S),
    {noreply,NS};

handle_cast({returnChannel,Ch,BS},S) ->
    io:format("[BS ~p] Return Channel ~p to BS ~p~n",[self(),Ch,BS]),
    NS = returnChannel(BS,Ch,S),
    {noreply,NS};

handle_cast({switchChannelBS,Ch,Ch1,BS},S=#bs_state{borrowed=B}) ->
    {_,MS,_} = lists:keyfind(Ch1,1,B),
    switchChannel(Ch,MS),
    NB=lists:keystore(Ch,1,lists:keydelete(Ch1,1,B),{Ch,MS,BS}),
    {noreply,S#bs_state{borrowed=NB}}.


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
handle_call({getChannel,MS},_,S=#bs_state{allocated=Alc}) ->
    case dict:find(MS,Alc) of
	error -> {reply,none,S};
	C     -> {reply,C,S}
    end;

handle_call({borrowAvailable},_,S=#bs_state{cochannel=Co,high=H}) ->
    {reply,length(lists:filter(fun(X) -> isFreeAll(X,Co) end,H)),S};

handle_call({borrow,BS},_,S=#bs_state{cochannel=Co,high=H,given=G}) ->
    case findFirst(fun(Ch) -> isFreeAll(Ch,Co) end,H) of
	{ok,Ch} -> case lockAll(Ch,Co) of
		       true -> NH = lists:delete(Ch,H),
			       NG = dict:append(Ch,BS,G),
			       {reply,{ok,Ch},S#bs_state{high=NH,given=NG}};
		       false -> {reply,{fail},S}
		   end;
	{fail} -> {reply,{fail},S}
    end;

handle_call({isfreeChannel,Ch},_,S) ->
    {reply,isFreeChannel(Ch,S),S};

handle_call({lockChannel,Ch,BS},_,S=#bs_state{low=L,high=H,locked=Lck}) ->
    io:format("[BS ~p] Lock Channel ~p for BS ~p ~n",[self(),Ch,BS]),
    case isFreeChannel(Ch,S) of
	true -> case lists:member(Ch,L) of
		    true -> NL = lists:delete(Ch,L),
			    NLck = insertLock(Ch,BS,l,Lck),
			    {reply,true,S#bs_state{low=NL,locked=NLck}};
		    false -> case lists:member(Ch,H) of
				 true -> NH = lists:delete(Ch,H),
					 NLck = insertLock(Ch,BS,h,Lck),
					 {reply,true,S#bs_state{high=NH,locked=NLck}};
				 false -> NLck = insertLock(Ch,BS,n,Lck),
					  {reply,true,S#bs_state{locked=NLck}}
			     end
		end;
	false -> {reply,false,S}
    end;

handle_call({startcall,MS},_,S) ->
    case allocateChannel(MS,S) of
	{ok,Ch,NS} -> {reply,{ok,Ch},NS};
	{fail,NS}  -> {reply,{fail},NS}
    end;

handle_call({get,state},_,S) ->
    {reply,S,S}.



%% Checks if channel is free in the cochannel
isFree(Ch,Co) ->
    gen_server:call(Co,{isfreeChannel,Ch}).

freeChannel(Ch,Co) ->
    gen_server:cast(Co,{unlockChannel,Ch,self()}).

isFreeAll(Ch,Co) ->
   lists:all(fun(X) -> isFree(Ch,X) end,Co).

lockAll(Ch,Co) ->
    Status = lists:map(fun(X) -> lock(Ch,X) end,Co),
    case lists:all(fun(X) -> X end,Status) of
	true -> true;
	false -> Z = lists:zip(Status,Co),
		 ToSend = lists:filter(fun({X,_}) -> X end,Z),
		 lists:foreach(fun(C) -> freeChannel(Ch,C) end,ToSend),
		 false
    end.

unlockAll(Ch,Co) ->
    lists:foreach(fun(C) -> freeChannel(Ch,C) end,Co).

%% Check if channel is free for locking
isFreeChannel(Ch,#bs_state{allocated=A,given=G,borrowed=B,locked=L}) ->
    not(isValue(Ch,A) or dict:is_key(Ch,G) or lists:keymember(Ch,1,B) or dict:is_key(Ch,L)).

%% for dictionaries
isValue(V,D) ->
    dict:size(dict:filter(fun(_,V1) -> V==V1 end,D)) /= 0.

%% Locks the Channel in the Cochannel cell
lock(Ch,Co) ->
    gen_server:call(Co,{lockChannel,Ch,self()}).

%% Channel allocation
allocateChannel(MS,S=#bs_state{low=L,borrowed=B,neighbours=Nh,allocated=Alc}) ->
    case L of
	[CH|T] -> 
	    io:format("[BS ~p] Allocated local channel ~p~n",[self(),CH]),
	    NewS=S#bs_state{allocated=dict:store(MS,CH,Alc),low=T},
	    {ok,CH,NewS};
	[] -> case fromNeighbour(Nh) of
		  {ok,Ch,N} -> 
		      io:format("[BS ~p] Borrowed channel ~p from Neighbour ~p~n",[self(),Ch,N]),
		      NewS=S#bs_state{borrowed=[{Ch,MS,N}|B]},
		      {ok,Ch,NewS};
		  {fail}    -> 
		      {fail,S}
	      end
    end.

%% Channel Return and switching policies when call ends
switchingRule2(Ch,S=#bs_state{borrowed=B,allocated=Alc}) -> %% First and second applied together
    case B of %% Second switching rule
	[] -> 
	    switchingRule1(Ch,S);
	[{Ch1,MS1,N1}|Rest] -> 
	    io:format("[BS ~p] Switching Rule 2 applied~n",[self()]),
	    switchChannel(Ch,MS1),
	    returnBorrowed(Ch1,N1),
	    S#bs_state{borrowed=Rest,allocated=dict:store(MS1,Ch,Alc)}
    end.

switchingRule1(Ch,S=#bs_state{low=L,allocated=Alc}) -> %% First.
  SAlc = lists:sort(fun({_,A},{_,B}) -> A >= B end,dict:to_list(Alc)), %% First Switching rule
  case SAlc of
      [] -> S#bs_state{low=lists:sort([Ch|L])};
      [{MS1,Ch1}|_] ->
	  io:format("[BS ~p] Switching Rule 1 applied~n",[self()]),
	  if
	      Ch1 > Ch -> switchChannel(Ch,MS1),
			  io:format("[BS ~p] Switching channel ~p to ~p for ~p ~n",[self(),Ch1,Ch,MS1]),
			  S#bs_state{allocated=dict:store(MS1,Ch,Alc),low=lists:sort([Ch1|L])};
	      true -> S#bs_state{low=lists:sort([Ch|L])}
	  end
  end.

endcall(MS,Ch,S=#bs_state{borrowed=B,allocated=Alc}) ->
    case dict:is_key(MS,Alc) of
	true  ->
	    NAlc = dict:erase(MS,Alc),
	    switchingRule2(Ch,S#bs_state{allocated=NAlc});
	false -> 
	    io:format("BORROWED ~p Channel search ~p~n",[B,Ch]),
	    {_,_,N} = lists:keyfind(Ch,1,B),
	    returnBorrowed(Ch,N),
	    S#bs_state{borrowed=lists:keydelete(Ch,1,B)}
    end.

returnBorrowed(Ch,BS) ->
    gen_server:cast(BS,{returnChannel,Ch,self()}).

switchChannel(Ch,MS) ->
    gen_server:cast(MS,{switchChannel,Ch}).

switchChannelBS(Ch,Ch1,BS) ->
    gen_server:cast(BS,{switchChannelBS,Ch,Ch1,self()}).

%% Channel switching policies when borrowed channel is returned
returnChannel(_,Ch,S=#bs_state{high=H,given=G,cochannel=Co}) ->
    NG = dict:erase(Ch,G),
    SG = lists:sort(fun({A,_},{B,_}) -> A >= B end,dict:to_list(NG)), %% Third Switching rule
    %%io:format("[BS ~p] G ~p NG ~p SG ~p~n",[self(),G,NG,SG]),
    case SG of
	[] -> unlockAll(Ch,Co),
	      NH = lists:sort([Ch|H]),
	      S#bs_state{high=NH,given=NG};
	[{Ch1,[BS1]}|_] ->
	    io:format("[BS ~p] Switching Rule 3 applied~n",[self()]),
	    if
		Ch1 > Ch -> 
		    io:format("[BS ~p] Ch1 ~p Ch ~p BS1 ~p~n",[self(),Ch1,Ch,BS1]),
		    switchChannelBS(Ch,Ch1,BS1),
		    NNG = dict:store(Ch,BS1,dict:erase(Ch1,NG)),
		    NH = lists:sort([Ch1|H]),
		    unlockAll(Ch1,Co),
		    S#bs_state{high=NH,given=NNG};
		true -> unlockAll(Ch,Co),
			NH = lists:sort([Ch|H]),
			S#bs_state{high=NH,given=NG}
	    end;
	[{Ch1,BS1}|_] ->
	    io:format("[BS ~p] Switching Rule 3 applied~n",[self()]),
	    if
		Ch1 > Ch -> 
		    io:format("[BS ~p] Ch1 ~p Ch ~p BS1 ~p~n",[self(),Ch1,Ch,BS1]),
		    switchChannelBS(Ch,Ch1,BS1),
		    NNG = dict:store(Ch,BS1,dict:erase(Ch1,NG)),
		    NH = lists:sort([Ch1|H]),
		    unlockAll(Ch1,Co),
		    S#bs_state{high=NH,given=NNG};
		true -> unlockAll(Ch,Co),
			NH = lists:sort([Ch|H]),
			S#bs_state{high=NH,given=NG}
	    end
    end.


%% Channel Borrowing policy
fromNeighbour(Nh) ->
    C = lists:map(fun(X) -> countAvailable(X) end,Nh),
    {_,N} = lists:max(lists:zip(C,Nh)),
    case C of
	0 -> {fail};
	_ -> case borrow(N) of
		 {ok,Ch} -> {ok,Ch,N};
		 {fail} -> {fail}
	     end
    end.

%% Finds number of channels available for borrowing
countAvailable(Nh) ->
    gen_server:call(Nh,{borrowAvailable}).

%% Requests a channel for borrowing
borrow(Nh) ->
    gen_server:call(Nh,{borrow,self()}).


%% Finds the first element satisfying the given predicate in the list
findFirst(_,[]) -> {fail};
findFirst(P,[X|Xs]) -> case P(X) of
			   true -> {ok,X};
			   false -> findFirst(P,Xs)
		       end.

%% lock is actually a dict of type (Channel,[Basestations])
removeLock(Ch,BS,L) ->
    {BSL,V} = dict:fetch(Ch,L),
    NBSL    = lists:delete(BS,BSL),
    case NBSL of
	[] -> {complete,V,dict:erase(Ch,L)};
	_  -> {partial,dict:store(Ch,{NBSL,V},L)}
    end.

insertLock(Ch,BS,V,L) ->
    case dict:is_key(Ch,L) of
	true -> dict:update(Ch,fun({X,Val}) -> {[{BS}|X],Val} end,L);
	false -> dict:store(Ch,{[],V},L)
    end.
