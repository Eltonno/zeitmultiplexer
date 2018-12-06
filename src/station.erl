%%%-------------------------------------------------------------------
%%% @author Elton
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. Nov 2018 20:51
%%%-------------------------------------------------------------------
-module(station).
-author("Elton").

%% API
-export([start/1,transmitter_loop/2,receiver_loop/2]).


start(Offset) ->
  RecPID = spawn(?MODULE, reciever_loop, []),
  spawn(?MODULE, init_transmitter, [RecPID, Offset]).

init_transmitter(RecPID, Offset) ->
  X = (1000 - (vsutil:now2UTC(erlang:timestamp())+Offset)rem 1000)+990,
  timer:send_after(X, RecPID, {self(), getslotlist}),
  receive
    {_,L} ->
      Slot = selectslot(L),
      X =  (Slot-1) * 40, %%(1000 - (vsutil:now2UTC(erlang:timestamp())+Offset)rem 1000)+ Eventuell muss das wegen timing noch benutzt werden
      timer:apply_after(X,?MODULE, transmitter_loop, [RecPID, Offset])
  end.

transmitter_loop(RecPID, Offset) ->
  RecPID ! {self(), getslotlist},
  receive
    {_,L} ->
      Slot = selectslot(L),
      %%TODO: Datenpaket zusammenbauen
      case io:read("") of
        eof ->
          transmitter_loop(RecPID, Offset);
        {ok, Term} ->
          ok;
        {error, ErrorInfo} ->
          ok;
        {error, ErrorDescription} ->
          ok
      end,
      %%TODO: Slot überprüfen
      %%TODO: Datenpaket verschicken mit multicast

      %%Berechnung vom nächsten X
      X = (1000 - (vsutil:now2UTC(erlang:timestamp())+Offset)rem 1000)+ (Slot-1) * 40,
      timer:apply_after(X,?MODULE, transmitter_loop, [RecPID, Offset])
  end.

receiver_loop(Offset, Oclist) ->
  X = 1000 - (vsutil:now2UTC(erlang:timestamp())+Offset)rem 1000,
  timer:send_after(X, resetlist),
  receive
    {TranPID, getslotlist} ->
      TranPID ! {self(), Oclist},
      receiver_loop(Offset, Oclist);
    resetlist ->
      receiver_loop(Offset,[]);
    Any ->
      receiver_loop(Offset,Oclist)
  end.

member(_,[]) ->
  false;
member(Elem, [H|T]) ->
  if
    H == Elem -> true;
    true -> member(Elem, T)
  end.

selectslot(L) ->
  Rand = rand:uniform(25),
  Member = member(Rand, L),
  if
    Member ->
      selectslot(L);
    true ->
      Rand
  end.
