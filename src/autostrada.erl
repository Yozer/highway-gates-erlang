-module(autostrada).
-compile(export_all).
-include("cecho.hrl").

-define(ROAD_COLOR, 1).
-define(ROAD_COLOR_LINE, 2).
-define(ROAD_WIDTH, 8).

-define(ROAD_COUNT, 8).

main() ->
try
  init(),
  Roads = createRoads(),
  drawRoads(Roads),
  %{MaxRow, MaxCol} = cecho:getmaxyx(),
  %Window = cecho:newwin(10, 10, 0, 1),
  %cecho:waddstr(Window, io_lib:format("MaxRow:~p MaxCol:~p", [MaxRow,MaxCol])),
  %cecho:box(Window, 0, 0),

  %cecho:wbkgd(Window, ?ceCOLOR_PAIR(?ROAD_COLOR)),
%  cecho:wattron(Window, ?ceCOLOR_PAIR(CN)),
  %cecho:waddstr(Window, "test"),

  %drawRoad(Window),
  %cecho:wrefresh(Window),

  %Window2 = createRoad(1),
  %drawRoad(Window2),
  %cecho:wrefresh(Window2),
  timer:sleep(5000)
  %cecho:wmove(Window, 3, 3),
  %cecho:waddch(Window, $a),
  %cecho:wrefresh(Window),
  %timer:sleep(2000)
after
  application:stop(cecho)
end.


init() ->
  os:putenv("TERM", "xterm-256color"),
  application:start(cecho),
  cecho:cbreak(),
  cecho:noecho(),
  cecho:curs_set(?ceCURS_INVISIBLE),
  cecho:start_color(),
  cecho:init_pair(?ROAD_COLOR, 239, 239),
  cecho:init_pair(?ROAD_COLOR_LINE, 255, 239).

createRoads() -> lists:map(fun ?MODULE:createRoad/1, lists:seq(1, ?ROAD_COUNT)).

createRoad(Index) ->
  {_, MaxCol} = cecho:getmaxyx(),
  cecho:newwin(MaxCol, ?ROAD_WIDTH, 0, 1 + ?ROAD_WIDTH * (Index - 1)),
  Index.

drawRoads(Roads) -> lists:map(fun ?MODULE:drawRoad/1, Roads).

drawRoad(Window) ->
  {_, MaxCol} = cecho:getmaxyx(),
  cecho:wbkgd(Window, ?ceCOLOR_PAIR(?ROAD_COLOR)),
  if
    Window /= ?ROAD_COUNT ->
      cecho:wmove(Window, 0, ?ROAD_WIDTH - 1),
      cecho:attron(Window, ?ceCOLOR_PAIR(?ROAD_COLOR_LINE)),
      cecho:wvline(Window, $|, MaxCol),
      cecho:attroff(Window, ?ceCOLOR_PAIR(?ROAD_COLOR_LINE));
   true ->
     cecho:wmove(Window, 0, 1)
  end,
  cecho:wrefresh(Window).
