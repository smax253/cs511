-module(server).

-export([start_server/0]).

-include_lib("./defs.hrl").

-spec start_server() -> _.
-spec loop(_State) -> _.
-spec do_join(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_leave(_ChatName, _ClientPID, _Ref, _State) -> _.
-spec do_new_nick(_State, _Ref, _ClientPID, _NewNick) -> _.
-spec do_client_quit(_State, _Ref, _ClientPID) -> _NewState.

start_server() ->
    catch(unregister(server)),
    register(server, self()),
    case whereis(testsuite) of
	undefined -> ok;
	TestSuitePID -> TestSuitePID!{server_up, self()}
    end,
    loop(
      #serv_st{
	 nicks = maps:new(), %% nickname map. client_pid => "nickname"
	 registrations = maps:new(), %% registration map. "chat_name" => [client_pids]
	 chatrooms = maps:new() %% chatroom map. "chat_name" => chat_pid
	}
     ).

loop(State) ->
    receive 
	%% initial connection
	{ClientPID, connect, ClientNick} ->
	    NewState =
		#serv_st{
		   nicks = maps:put(ClientPID, ClientNick, State#serv_st.nicks),
		   registrations = State#serv_st.registrations,
		   chatrooms = State#serv_st.chatrooms
		  },
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, join, ChatName} ->
	    NewState = do_join(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to join a chat
	{ClientPID, Ref, leave, ChatName} ->
	    NewState = do_leave(ChatName, ClientPID, Ref, State),
	    loop(NewState);
	%% client requests to register a new nickname
	{ClientPID, Ref, nick, NewNick} ->
	    NewState = do_new_nick(State, Ref, ClientPID, NewNick),
	    loop(NewState);
	%% client requests to quit
	{ClientPID, Ref, quit} ->
	    NewState = do_client_quit(State, Ref, ClientPID),
	    loop(NewState);
	{TEST_PID, get_state} ->
	    TEST_PID!{get_state, State},
	    loop(State)
    end.

%% executes join protocol from server perspective
do_join(ChatName, ClientPID, Ref, State) ->
	{RoomPID, NewState} = case maps:is_key(ChatName, State#serv_st.chatrooms) of 
				false -> 
					PID = spawn(chatroom, start_chatroom, [ChatName]),
					NState = State#serv_st{
						chatrooms=maps:put(ChatName, PID, State#serv_st.chatrooms),
						registrations=maps:put(ChatName, [ClientPID], State#serv_st.registrations)},
					{PID, NState};
				true -> 
					PID = maps:get(ChatName, State#serv_st.chatrooms),
					PrevReg = maps:get(ChatName, State#serv_st.registrations),
					NState = State#serv_st{
						registrations=maps:put(ChatName, [ClientPID | PrevReg], State#serv_st.registrations)
					},
					{PID, NState}
				end,
	Nickname = maps:get(ClientPID, State#serv_st.nicks),
	RoomPID ! {self(), Ref, register, ClientPID, Nickname},
	NewState.

%% executes leave protocol from server perspective
do_leave(ChatName, ClientPID, Ref, State) ->
	case maps:get(ChatName, State#serv_st.chatrooms, not_found) of
		not_found -> 
			State;
		ChatroomPID -> 
			OldRegistrations = maps:get(ChatName, State#serv_st.registrations),
			NewRegistrations = maps:put(ChatName, lists:delete(ClientPID, OldRegistrations),State#serv_st.registrations),
			ChatroomPID ! {self(), Ref, unregister, ClientPID},
			ClientPID ! {self(), Ref, ack_leave},
			State#serv_st{registrations=NewRegistrations}
	end.

%% executes new nickname protocol from server perspective
do_new_nick(State, Ref, ClientPID, NewNick) ->
    io:format("server:do_new_nick(...): IMPLEMENT ME~n"),
    State.

%% executes client quit protocol from server perspective
do_client_quit(State, Ref, ClientPID) ->
    io:format("server:do_client_quit(...): IMPLEMENT ME~n"),
    State.
