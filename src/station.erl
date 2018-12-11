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
-export([start/1, init_transmitter/6, init_receiver/6, transmitter_loop/7, gen_receiver/4]).

-define(TTL, 1).


start([Interface, MultiCastAddress, Port, ClockClass, Offset]) ->
  InterfaceAddress = vsutil:getInterfaceIp(Interface),
  util:logging("logfile.log", "ist gestartet"),
  {_, MultiCastAddr} = inet:parse_address(atom_to_list(MultiCastAddress)),
  PortInteger = list_to_integer(atom_to_list(Port)),
%%  {ok, Socket} = gen_udp:open(Port,[
%%    {mode, binary},
%%    {active, true},
%%    {reuseaddr, true},
%%    {multicast_if, atom_to_list(MultiCastAddress)},
%%    {multicast_ttl, ?TTL},
%%    {multicast_loop, true},
%%    {broadcast,true},
%%    {add_membership, {MultiCastAddress, list_to_atom(InterfaceAddress)}},
%%    {ip, MultiCastAddress}]),
  Socket = vsutil:openRecA(MultiCastAddr, InterfaceAddress, PortInteger),
  RecPID = spawn(?MODULE, init_receiver, [Offset, [], Socket, InterfaceAddress, Port, ClockClass]),
  TranPID = spawn(?MODULE, init_transmitter, [RecPID, Offset, Socket, InterfaceAddress, Port, ClockClass]),
  spawn(?MODULE, gen_receiver, [RecPID, TranPID, Socket, ClockClass]).

init_transmitter(RecPID, Offset, Socket,Address,Port,ClockClass) ->
  %% Es wird für den Rest dieses Frames, sowie für den gesamten nächsten (-10 ms um resetlist zu vermeiden) zugehört, und dann
  %% die Liste mit den reservierten Slots angefragt.
  X = ((vsutil:now2UTC(erlang:timestamp())+list_to_integer(atom_to_list(Offset)))rem 1000)+990,
  timer:send_after(X, RecPID, {self(), getslotlist}),
  receive
    {_,L,NewOffset} ->
      Slot = selectslot(L),
      SleepTimer =  (Slot-1) * 40, %%(1000 - (vsutil:now2UTC(erlang:timestamp())+Offset)rem 1000)+ Eventuell muss das wegen timing noch benutzt werden
      timer:apply_after(SleepTimer,?MODULE, transmitter_loop, [RecPID, NewOffset, Socket, Address, Port, ClockClass])
  end.

transmitter_loop(RecPID, Offset, Socket, Address, Port, ClockClass, Slot) ->
  RecPID ! {self(), getslotlist},
  receive
    {_,L,NewOffset} ->
      NewSlot = selectslot(L),
      case io:read("") of
        eof ->
          transmitter_loop(RecPID, NewOffset, Socket, Address, Port, ClockClass, Slot);
        {ok, Data} ->
          X = (((vsutil:now2UTC(erlang:timestamp())+Offset)rem 1000)/ 40)+1,
          %%TODO: Datenpaket verschicken mit multicast
          if
            X == Slot ->
              Package = vsutil:concatBinary(vsutil:createBinaryS(ClockClass),vsutil:createBinaryD(Data),vsutil:createBinaryNS(NewSlot),vsutil:createBinaryT(vsutil:now2UTC(erlang:timestamp())+NewOffset)),
              RemSlotTime = (1000 - (vsutil:now2UTC(erlang:timestamp())+Offset)rem 40),
              if
                RemSlotTime =< 20 ->
                  gen_udp:send(Socket, Address, Port, Package);
                true ->
                  timer:sleep(RemSlotTime - 20),
                  gen_udp:send(Socket, Address, Port, Package)
              end,
              %%Berechnung vom nächsten X
              X = (1000 - (vsutil:now2UTC(erlang:timestamp())+Offset)rem 1000)+ (Slot-1) * 40,
              timer:apply_after(X,?MODULE, transmitter_loop, [RecPID, Offset, Socket, Address, Port, ClockClass, NewSlot]);
            X > Slot ->
              init_transmitter(RecPID,Offset,Socket,Address,Port,ClockClass);
            true ->
              Sleep = (1000 - (vsutil:now2UTC(erlang:timestamp())+Offset)rem 40)+ (Slot-X+1) * 40,
              timer:apply_after(Sleep,?MODULE, transmitter_loop, [RecPID, Offset,Socket,Address,Port,ClockClass, Slot])
          end;
        {error, _ErrorInfo} ->
          ok
      end
  end.

init_receiver(Offset, Oclist, Socket, Address, Port, ClockClass) ->
  receiver_loop(Offset, Oclist, Socket, Address, Port, ClockClass).

receiver_loop(Offset, Oclist, Socket, Address, Port, ClockClass) ->
  X = (1000 - vsutil:now2UTC(erlang:timestamp())+list_to_integer(atom_to_list(Offset)))rem 1000,
  %% Zur vollen Sekunde (Ende des Frames) wird die Reservierungsliste geleert
  timer:send_after(X, resetlist),
  receive
    {TranPID, getslotlist} ->
      TranPID ! {self(), Oclist, Offset},
      receiver_loop(Offset, Oclist, Socket, Address, Port, ClockClass);
    resetlist ->
      receiver_loop(Offset,[], Socket, Address, Port, ClockClass);
    {packet, Slot, OffsetDiff} ->
      NewOclist = append(Oclist,Slot),
      NewOffset = Offset + OffsetDiff,
      receiver_loop(NewOffset, NewOclist, Socket, Address, Port, ClockClass);
    _Any ->
      receiver_loop(Offset, Oclist, Socket, Address, Port, ClockClass)
  end.

gen_receiver(RecPID, TranPID, Socket, ClockClass) ->
  ok = gen_udp:controlling_process(Socket, self()),
  {ok, RecvData} = gen_udp:recv(Socket, 34),
  {_Address, _Port, Packet} = RecvData,
  {StationTyp,_Nutzdaten,Slot,Timestamp} = vsutil:message_to_string(Packet),
  if
    StationTyp == "A" ->
      TimeDiff = Timestamp - vsutil:now2UTC(erlang:timestamp()),
      RecPID ! {packet, Slot, TimeDiff/2}
  end,
  RecPID ! {packet, Slot, 0},
  gen_receiver(RecPID, TranPID, Socket, ClockClass).

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

append([], []) ->
  [];
append([],[H|T]) ->
  [H|T];
append([],Elem) ->
  [Elem];
append([H|T], []) ->
  [H|T];
append(Elem, []) ->
  [Elem];
append([H1|T1],[H2|T2]) ->
  append([H1|T1] ++ [H2], T2);
append([H|T],Elem) ->
  [H|T] ++ [Elem];
append(L,[H|T]) ->
  append([L] ++ [H], T);
append(E1,E2) ->
  [E1] ++ [E2].
%%
%%keyfind(_,[]) ->
%%  false;
%%keyfind(Key, Tuplelist) ->
%%  [Head|Rest] = Tuplelist,
%%  {K,_} = Head,
%%  if K == Key -> Head;
%%    true -> keyfind(Key,Rest)
%%  end.
%%
%%keystore(_,[],Tupel) ->
%%  [Tupel];
%%keystore(Key,[Head|Rest],Tupel) ->
%%  {K,_}= Head,
%%  if K == Key -> append(Tupel, Rest);
%%    true -> keystore(Key,Rest,[Head],Tupel)
%%  end.
%%
%%keystore(_,[],Front,Tupel) ->
%%  append(Front, Tupel);
%%keystore(Key,[Head|Rest],Front,Tupel) ->
%%  {K,_} = Head,
%%  if K == Key -> append(append(Front,Tupel),Rest);
%%    true -> keystore(Key,Rest,append(Front,Head),Tupel)
%%  end.