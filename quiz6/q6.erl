-module(q6).

-compile(export_all).

start() ->
    S = spawn(fun server/0), spawn(?MODULE, client, [S]).

server() ->
    receive
      {From, start} ->
	  Servlet = spawn(?MODULE, servlet,
			  [rand:uniform(11) - 1]),
	  From ! {Servlet},
	  server()
    end.

servlet(Correct) ->
    %io:format("Correct: ~w~n", [Correct]),
    receive
      {From, Number} ->
	  %io:format("Guess: ~w~n", [Number]),
	  case Number of
	    Correct -> From ! gotIt;
	    _ -> From ! tryAgain, servlet(Correct)
	  end
    end.

client(S) ->
    S ! {self(), start},
    receive {Servlet} -> clientGuessing(Servlet) end.

clientGuessing(Servlet) ->
    Servlet ! {self(), rand:uniform(11) - 1},
    receive
      tryAgain -> clientGuessing(Servlet);
      _ -> ok
    end.
