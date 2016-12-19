-module(autostrada).
-compile(export_all).
%-export([main/0]).
-include("cecho.hrl").

-define(ROAD_COLOR, 1).
-define(ROAD_COLOR_LINE, 2).
-define(GATE_COLOR, 4).
-define(GATE_OPEN_COLOR, 5).
-define(BLACK_FG, 6).
-define(CARCOLOR_START, 7).
-define(CARCOLOR_END, 14).
-define(BLACK, 15).
-define(FONT_COLOR, 16).

-define(ROAD_WIDTH, 8).
-define(CAR_HEIGHT, 4).
-define(GATE_HEIGHT, 3).
-define(ROAD_COUNT, 8).
-define(GATE_TIMEOUT, 2000).
-define(ONEDAY, 86400).
-define(ONEHOUR, 3600).
-define(ONEMINUTE, 60).
-define(DRAWSPEED, 50).

-record(car, {speed, pid, color, y}).

main() ->
  init(),
  Roads = createRoads(),
  SettingsPid = spawn_link(?MODULE, settingsSynchronizer, [1, 0]),
  spawn_link(?MODULE, carSpawnerProcess, [SettingsPid, Roads]),
  spawn_link(?MODULE, input_reader, [SettingsPid]),
  drawLoop(Roads, 0, SettingsPid).

drawLoop(Roads, Time, SettingsPid) ->
  cecho:wbkgd(?ceSTDSCR, ?ceCOLOR_PAIR(?BLACK)),
  cecho:wnoutrefresh(?ceSTDSCR),
  Fn  = fun(Pid) -> syncMessage(Pid, draw) end,
  lists:foreach(Fn, Roads),
  drawStats(Time, Roads),
  cecho:doupdate(),
  timer:sleep(round(recieveTimeModifier(SettingsPid) * ?DRAWSPEED)),
  NewTime = Time + (?DRAWSPEED div 5),
  SettingsPid ! {newTime, NewTime},
  drawLoop(Roads, NewTime, SettingsPid).

drawStats(Time, Roads) ->
  cecho:attron(?ceSTDSCR, ?ceCOLOR_PAIR(?FONT_COLOR)),
  cecho:mvaddstr(0, 0, getDisplayTime(Time)),
  Fun = fun (Pid) -> Pid ! {self(), getCarCount}, receive {carCount, Count} -> Count end end,
  CarsCount = lists:sum(lists:map(Fun, Roads)),
  cecho:mvaddstr(1, 0, io_lib:format("Liczba pojazdow: ~p", [CarsCount])),
  cecho:attroff(?ceSTDSCR, ?ceCOLOR_PAIR(?FONT_COLOR)),
  cecho:wnoutrefresh(?ceSTDSCR).

calculateCarCount(Pid) ->
   Pid ! {self(), getCarCount},
   receive X -> X end.

syncMessage(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        {Pid, ok} -> done
    end.

settingsSynchronizer(TimeModifier, Time) ->
  NewTimeModifier = receive
    slowDown -> min(2, TimeModifier + 0.05);
    speedUp -> max(0, TimeModifier - 0.05)
    after 0 -> TimeModifier
  end,
  NewTime = receive {newTime, T} -> T after 0 -> Time end,
  receive
    {From, getTimeModifier} -> From ! {timeModifier, NewTimeModifier};
    {From, getHourAndModifier} -> From ! {hourAndModifier, NewTime, NewTimeModifier}
    after 0 -> ok
  end,
  settingsSynchronizer(NewTimeModifier, NewTime).

input_reader(SettingsPid) ->
  P = cecho:getch(),
  case P of
	   $q -> application:stop(cecho);
     $a -> SettingsPid ! slowDown, input_reader(SettingsPid);
     $s -> SettingsPid ! speedUp, input_reader(SettingsPid);
	    _ -> input_reader(SettingsPid)
end.

init() ->
  os:putenv("TERM", "xterm-256color"),
  crypto:start(),
  application:start(cecho),
  cecho:cbreak(),
  cecho:noecho(),
  cecho:curs_set(?ceCURS_INVISIBLE),
  cecho:start_color(),
  cecho:init_pair(?ROAD_COLOR, 239, 239),
  cecho:init_pair(?ROAD_COLOR_LINE, 255, 239),
  cecho:init_pair(?BLACK_FG, 0, 239),
  cecho:init_pair(?BLACK, 0, 0),
  cecho:init_pair(?GATE_COLOR, 35, 35),
  cecho:init_pair(?CARCOLOR_START, 61, 61),
  cecho:init_pair(?CARCOLOR_START + 1, 133, 133),
  cecho:init_pair(?CARCOLOR_START + 2, 106, 106),
  cecho:init_pair(?CARCOLOR_START + 3, 234, 234),
  cecho:init_pair(?CARCOLOR_START + 4, 90, 90),
  cecho:init_pair(?CARCOLOR_START + 5, 88, 88),
  cecho:init_pair(?CARCOLOR_START + 6, 97, 97),
  cecho:init_pair(?CARCOLOR_START + 7, 217, 217),
  cecho:init_pair(?GATE_OPEN_COLOR, 34, 34),
  cecho:init_pair(?FONT_COLOR, 0, 255).

% processes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
roadProcess(Window, Cars, WinHeight) ->
  IsReversed = isReversedRoad(Window),
  receive
    {From, getCarCount} -> From ! {carCount, length(Cars)}, roadProcess(Window, Cars, WinHeight);
    {From, draw} ->
      drawRoad(Window, Cars),
      From ! {self(), ok},
      roadProcess(Window, Cars, WinHeight);
    {new_car, SettingsPid, Car} ->
      IsValidMove = validMove(Car, Cars),
      if IsValidMove ->
        CarPid = spawn_link(?MODULE, carProcess, [SettingsPid, self(), Car, WinHeight, IsReversed]),
        NewCar = Car#car{pid=CarPid},
        roadProcess(Window, [NewCar|Cars], WinHeight);
      true -> roadProcess(Window, Cars, WinHeight) end;
    {From, {move, Car}} ->
      FilteredCars = lists:filter(fun(X) -> X#car.pid /= From end, Cars),
      IsValidMove = validMove(Car, FilteredCars),
      MinDistance = calcDistance(Car, [#car{y=WinHeight div 2 - (if IsReversed -> ?GATE_HEIGHT; true -> 0 end)}|FilteredCars]),
      IsEndOfJourney = endOfJourney(Car#car.y - 1, WinHeight),
      UpdatedCars = if IsValidMove ->
                        if IsEndOfJourney -> From ! end_journey, FilteredCars;
                        true -> From ! {ok_move, MinDistance}, [Car#car{pid=From}|FilteredCars] end;
                     true -> From ! bad_move, Cars end,
      roadProcess(Window, UpdatedCars, WinHeight);
    terminate -> ok
  end.

carProcess(SettingsPid, RoadPid, Car, WinHeight, IsReversed) ->
  TimeModifier = recieveTimeModifier(SettingsPid),
  timer:sleep(round(TimeModifier * Car#car.speed * 100)),
  UpdatedCar = updateCar(Car),
  IsCarPassingThruGate = willCarPassThruGate(UpdatedCar, WinHeight, IsReversed),
  if IsCarPassingThruGate -> timer:sleep(round(TimeModifier * ?GATE_TIMEOUT)); true -> ok end,
  RoadPid ! {self(), {move, UpdatedCar}},
  receive
    {ok_move, MinDistance} -> carProcess(SettingsPid, RoadPid, updateCarSpeed(UpdatedCar, MinDistance), WinHeight, IsReversed);
    bad_move -> carProcess(SettingsPid, RoadPid, Car, WinHeight, IsReversed);
    end_journey -> terminate
  end.

carSpawnerProcess(SettingsPid, Roads) ->
  Index =  rand:uniform(length(Roads)),
  Road = lists:nth(Index,Roads),
  Road ! {new_car, SettingsPid, #car{speed=5,pid=0,color=randColor(),y=-?CAR_HEIGHT}},
  SettingsPid ! {self(), getHourAndModifier},
  {Hour, Modifier} = receive {hourAndModifier, T, M} -> {(T rem ?ONEDAY) div ?ONEHOUR, M} end,
  Timeout = if Hour =< 5 -> 5000;
               Hour =< 10 -> 800;
               Hour =< 15 -> 1000;
               Hour =< 18 -> 300;
               Hour =< 21 -> 1500;
               true -> 4000 end,
  timer:sleep(round(Modifier * Timeout)),
  carSpawnerProcess(SettingsPid, Roads).

createRoads() -> lists:map(fun createRoad/1, lists:seq(1, ?ROAD_COUNT)).
createRoad(Index) ->
  {MaxCol, _} = cecho:getmaxyx(),
  cecho:newwin(MaxCol, ?ROAD_WIDTH, 0, 1 + ?ROAD_WIDTH * (Index - 1)),
  spawn_link(?MODULE, roadProcess, [Index, [], MaxCol]).

updateCar(Car) -> Car#car{y=Car#car.y+1}.

% helpers
getDisplayTime(Time) ->
  DayTime = Time rem ?ONEDAY,
  Hours = DayTime div ?ONEHOUR,
  Minutes = (DayTime div ?ONEMINUTE) rem ?ONEMINUTE,
  io_lib:format("Godzina ~2..0B:~2..0B", [Hours, Minutes]).
recieveTimeModifier(Pid) -> Pid ! {self(), getTimeModifier}, receive {timeModifier, Modifier} -> Modifier end.
randColor() -> ?CARCOLOR_START - 1 + rand:uniform(?CARCOLOR_END - ?CARCOLOR_START + 1).
isReversedRoad(Window) -> Window > (?ROAD_COUNT div 2).
validMove(_, []) -> true;
validMove(NewCar, [#car{y=Y}|Cars]) -> if (NewCar#car.y + ?CAR_HEIGHT < Y) or (NewCar#car.y > Y) -> validMove(NewCar, Cars); true -> false end.
endOfJourney(Y, WinHeight) -> Y >= WinHeight + 1.
extractY(#car{y=Y}, IsReversed, WinHeight) -> if IsReversed -> WinHeight - ?CAR_HEIGHT - 1 - Y; true -> Y end.
calcDistance(_,[],_) -> infinity;
calcDistance(Car, [#car{y=Y}|Cars], Min) -> min(Min, min(Y - Car#car.y, calcDistance(Car, Cars))).
calcDistance(Car, Cars) -> calcDistance(Car, lists:filter(fun(#car{y=Y}) -> Y > Car#car.y end, Cars), infinity).
updateCarSpeed(Car, MinDistance) ->
  if
    MinDistance == infinity -> Car#car{speed=1};
    true -> Car#car{speed = min(max(10 - MinDistance, 1), 10)}
  end.

willCarPassThruGate(Car, WinHeight, IsReversed) ->
  TrueY = extractY(Car, IsReversed, WinHeight),
  ((TrueY + ?CAR_HEIGHT == WinHeight div 2) and (not IsReversed)) or ((TrueY == (WinHeight div 2) + ?GATE_HEIGHT - 1) and IsReversed).
anyCarIsPassingThruGate(Cars, IsReversed, WinHeight) ->
  Positions = lists:map(fun(Car) -> extractY(Car, IsReversed, WinHeight) end, Cars),
  lists:any(fun(Y) -> (Y + ?CAR_HEIGHT >= WinHeight div 2) and (Y < WinHeight div 2 + ?GATE_HEIGHT) end, Positions).
% drawing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
drawRoad(Window, Cars) ->
  {Height, _} = cecho:getmaxyx(),
  IsReversed = isReversedRoad(Window),
  GateY = Height div 2,
  AnyCarIsPassingThruGate = anyCarIsPassingThruGate(Cars, IsReversed, Height),

  cecho:wbkgd(Window, ?ceCOLOR_PAIR(?ROAD_COLOR)),
  drawGate(Window, GateY, AnyCarIsPassingThruGate),
  if
    Window /= ?ROAD_COUNT ->
      cecho:wmove(Window, 0, ?ROAD_WIDTH - 1),
      {Char, Color} = if Window == (?ROAD_COUNT div 2) -> {0, ?BLACK_FG}; true -> {$|, ?ROAD_COLOR_LINE} end,
      cecho:attron(Window, ?ceCOLOR_PAIR(Color)),
      cecho:wvline(Window, Char, Height),
      cecho:attroff(Window, ?ceCOLOR_PAIR(Color));
   true -> ok
  end,
  Fn  = fun(Car) -> drawCar(Window, Height, Car) end,
  lists:map(Fn, Cars),
  cecho:wnoutrefresh(Window).

drawGate(Window, GateY, AnyCarIsPassingThruGate) ->
  if AnyCarIsPassingThruGate ->
    SubWin = cecho:derwin(Window, ?GATE_HEIGHT, 1, GateY, 0),
    cecho:wbkgd(SubWin, ?ceCOLOR_PAIR(?GATE_OPEN_COLOR)),
    cecho:delwin(SubWin),
    SubWin = cecho:derwin(Window, ?GATE_HEIGHT, 1, GateY, ?ROAD_WIDTH - 2),
    cecho:wbkgd(SubWin, ?ceCOLOR_PAIR(?GATE_OPEN_COLOR)),
    cecho:delwin(SubWin);
  true ->
    SubWin = cecho:derwin(Window, ?GATE_HEIGHT, ?ROAD_WIDTH, GateY, 0),
    cecho:wbkgd(SubWin, ?ceCOLOR_PAIR(?GATE_COLOR)),
    cecho:delwin(SubWin)
  end.

drawCar(Window, Height, #car{y=Y, color=Color}) ->
  IsReversed = isReversedRoad(Window),

  {NewY, CarHeight} = if
                         Y < 0 -> {0, ?CAR_HEIGHT + Y};
                         true -> {Y, ?CAR_HEIGHT}
                       end,
  DrawY = if
          IsReversed -> Height - NewY - CarHeight;
          true -> NewY
         end,

  if CarHeight > 0 ->
    SubWin = cecho:derwin(Window, CarHeight, ?ROAD_WIDTH - 3, DrawY,  1),
    cecho:wbkgd(SubWin, ?ceCOLOR_PAIR(Color)),
    cecho:delwin(SubWin);
  true -> ok
  end.
