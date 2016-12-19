#!/usr/bin/env escript
%%! -noinput -pa ebin +A 50
-include_lib("include/cecho.hrl").
main(_) -> autostrada:main().
