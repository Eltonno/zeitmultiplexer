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
-export([start/4,transmitter_loop/5,receiver_loop/5, init_transmitter/5, init_receiver/5]).

-define(TTL, 1).


start(Offset, MultiCastAddress, Address, Port) ->
  {ok, Socket} = gen_udp:open(Port,[
    binary,
    {active, false},
    {reuseaddr, true},
    {multicast_if, Address},
    inet,
    {multicast_ttl, ?TTL},
    {multicast_loop, true},
    {add_membership, {MultiCastAddress, Address}},
    {ip, MultiCastAddress}]),
  RecPID = spawn(?MODULE, reciever_loop, [Offset, [], Socket, Address, Port]),
  spawn(?MODULE, init_transmitter, [RecPID, Offset, Socket, Address, Port]).

init_transmitter(RecPID, Offset, Socket,Address,Port) ->
  %% Es wird für den Rest dieses Frames, sowie für den gesamten nächsten (-10 ms um resetlist zu vermeiden) zugehört, und dann
  %% die Liste mit den reservierten Slots angefragt.
  X = (1000 - (vsutil:now2UTC(erlang:timestamp())+Offset)rem 1000)+990,
  timer:send_after(X, RecPID, {self(), getslotlist}),
  receive
    {_,L} ->
      Slot = selectslot(L),
      X =  (Slot-1) * 40, %%(1000 - (vsutil:now2UTC(erlang:timestamp())+Offset)rem 1000)+ Eventuell muss das wegen timing noch benutzt werden
      timer:apply_after(X,?MODULE, transmitter_loop, [RecPID, Offset,Socket,Address, Port])
  end.

transmitter_loop(RecPID, Offset,Socket, Address, Port) ->
  RecPID ! {self(), getslotlist},
  receive
    {_,L} ->
      Slot = selectslot(L),
      %%TODO: Datenpaket zusammenbauen
      case io:read("") of
        eof ->
          transmitter_loop(RecPID, Offset, Socket, Address, Port);
        {ok, Package} ->
          %%TODO: Slot überprüfen
          X = ((1000 - (vsutil:now2UTC(erlang:timestamp())+Offset)rem 1000)/ 40)+1,
          %%TODO: Datenpaket verschicken mit multicast
          gen_udp:send(Socket, Address, Port, Package);
        {error, ErrorInfo} ->
          ok;
        {error, ErrorDescription} ->
          ok
      end,
      %%Berechnung vom nächsten X
      X = (1000 - (vsutil:now2UTC(erlang:timestamp())+Offset)rem 1000)+ (Slot-1) * 40,
      timer:apply_after(X,?MODULE, transmitter_loop, [RecPID, Offset])
  end.

init_receiver(Offset, Oclist, Socket, Address, Port) ->
  ok = gen_udp:controlling_process(Socket, self()),
  receiver_loop(Offset, Oclist, Socket, Address, Port).

receiver_loop(Offset, Oclist, Socket, Address, Port) ->
  X = 1000 - (vsutil:now2UTC(erlang:timestamp())+Offset)rem 1000,
  %% Zur vollen Sekunde (Ende des Frames) wird die Reservierungsliste geleert
  timer:send_after(X, resetlist),
  receive
    {TranPID, getslotlist} ->
      TranPID ! {self(), Oclist},
      receiver_loop(Offset, Oclist, Socket, Address, Port);
    resetlist ->
      receiver_loop(Offset,[], Socket, Address, Port);
    Any ->
      receiver_loop(Offset, Oclist, Socket, Address, Port)
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
