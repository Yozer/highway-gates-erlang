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
-define(WHITE, 15).

-define(ROAD_WIDTH, 8).
-define(CAR_HEIGHT, 4).
-define(GATE_HEIGHT, 3).
-define(ROAD_COUNT, 8).
-define(GATE_TIMEOUT, 2000).
-define(CARSPAWNER_TIMEOUT, 500).

-record(car, {speed, pid, color, y}).
%-record(road, {window = 0, reverse = false, cars = []}).

main() ->
try
  init(),
  Roads = createRoads(),
  initCarSpawner(Roads),
  drawLoop(Roads)
after
  application:stop(cecho)
end.

drawLoop(Roads) ->
  cecho:wbkgd(?ceSTDSCR, ?ceCOLOR_PAIR(?WHITE)),
  cecho:wnoutrefresh(?ceSTDSCR),
  Fn  = fun(Pid) -> syncMessage(Pid, draw) end,
  lists:foreach(Fn, Roads),
  cecho:doupdate(),
  timer:sleep(40),
  drawLoop(Roads).

syncMessage(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        {Pid, ok} -> done
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
  cecho:init_pair(?WHITE, 255, 255),
  cecho:init_pair(?GATE_COLOR, 35, 35),
  cecho:init_pair(?CARCOLOR_START, 61, 61),
  cecho:init_pair(?CARCOLOR_START + 1, 133, 133),
  cecho:init_pair(?CARCOLOR_START + 2, 106, 106),
  cecho:init_pair(?CARCOLOR_START + 3, 234, 234),
  cecho:init_pair(?CARCOLOR_START + 4, 90, 90),
  cecho:init_pair(?CARCOLOR_START + 5, 88, 88),
  cecho:init_pair(?CARCOLOR_START + 6, 97, 97),
  cecho:init_pair(?CARCOLOR_START + 7, 217, 217),
  cecho:init_pair(?GATE_OPEN_COLOR, 34, 34).

initCarSpawner(Roads) ->spawn_link(?MODULE, carSpawnerProcess, [Roads]).

% processes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
roadProcess(Window, Cars, WinHeight) ->
  IsReversed = isReversedRoad(Window),
  receive
    {From, draw} ->
      drawRoad(Window, Cars),
      From ! {self(), ok},
      roadProcess(Window, Cars, WinHeight);
    {new_car, Car} ->
      IsValidMove = validMove(Car, Cars),
      if IsValidMove ->
        CarPid = spawn_link(?MODULE, carProcess, [self(), Car, WinHeight, IsReversed]),
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

carProcess(RoadPid, Car, WinHeight, IsReversed) ->
  timer:sleep(Car#car.speed * 100),
  %timer:sleep(1000),
  UpdatedCar = updateCar(Car),
  IsCarPassingThruGate = willCarPassThruGate(UpdatedCar, WinHeight, IsReversed),
  if IsCarPassingThruGate -> timer:sleep(?GATE_TIMEOUT); true -> ok end,
  RoadPid ! {self(), {move, UpdatedCar}},
  receive
    {ok_move, MinDistance} -> carProcess(RoadPid, updateCarSpeed(UpdatedCar, MinDistance), WinHeight, IsReversed);
    bad_move -> carProcess(RoadPid, Car, WinHeight, IsReversed);
    end_journey -> terminate
  end.

carSpawnerProcess(Roads) ->
  Index = crypto:rand_uniform(1, length(Roads) + 1),
  Road = lists:nth(Index,Roads),
  Road ! {new_car, #car{speed=5,pid=0,color=randColor(),y=-?CAR_HEIGHT}},
  timer:sleep(?CARSPAWNER_TIMEOUT),
  carSpawnerProcess(Roads).

createRoads() -> lists:map(fun createRoad/1, lists:seq(1, ?ROAD_COUNT)).
createRoad(Index) ->
  {MaxCol, _} = cecho:getmaxyx(),
  cecho:newwin(MaxCol, ?ROAD_WIDTH, 0, 1 + ?ROAD_WIDTH * (Index - 1)),
  spawn_link(?MODULE, roadProcess, [Index, [], MaxCol]).

updateCar(Car) -> Car#car{y=Car#car.y+1}.

% helpers
randColor() -> crypto:rand_uniform(?CARCOLOR_START, ?CARCOLOR_END + 1).
isReversedRoad(Window) -> Window > (?ROAD_COUNT div 2).
validMove(_, []) -> true;
validMove(NewCar, [#car{y=Y}|Cars]) -> if (NewCar#car.y + ?CAR_HEIGHT < Y) or (NewCar#car.y > Y) -> validMove(NewCar, Cars); true -> false end.
endOfJourney(Y, WinHeight) -> Y >= WinHeight + 1.
extractY(#car{y=Y}, IsReversed, WinHeight) -> if IsReversed -> WinHeight - ?CAR_HEIGHT - 1 - Y; true -> Y end.
calcDistance(_,[],_) -> infinity;
calcDistance(Car, [#car{y=Y}|Cars], Min) -> min(Min, min(Y - Car#car.y, calcDistance(Car, Cars))).
calcDistance(Car, Cars) -> calcDistance(Car, lists:filter(fun(#car{y=Y}) -> Y > Car#car.y end, Cars), infinity).
updateCarSpeed(Car, MinDistance) -> if MinDistance == infinity -> Car#car{speed=1}; true -> Car#car{speed = min(max(10 - MinDistance, 1), 10)} end.

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
  %cecho:attron(Window, ?ceCOLOR_PAIR(8)),
  %cecho:mvwvline(Window, DrawY, 1, 0, CarHeight).
  %cecho:mvwvline(Window, DrawY, ?ROAD_WIDTH - 3, 0, CarHeight),
  %cecho:mvwhline(Window, DrawY + CarHeight, 1, 0, ?ROAD_WIDTH - 3),
  %if NewY >= 0 ->
    %cecho:mvwhline(Window, DrawY, 1, 0, ?ROAD_WIDTH - 3),
    %cecho:corner(Window, DrawY, 1, 1),
    %cecho:corner(Window, DrawY, ?ROAD_WIDTH - 3, 2);
    %true -> ok
  %end,
  %cecho:corner(Window, DrawY + CarHeight, ?ROAD_WIDTH - 3, 3),
  %cecho:corner(Window, DrawY + CarHeight, 1, 4).
