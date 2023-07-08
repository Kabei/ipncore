#!/bin/bash

if [ "$MODE" = "iex" ]
then
    iex --sname ${NODE} --cookie ${COOKIE} -S mix
elif [ -n "$LOG" ]
then
    elixir --sname ${NODE} --cookie ${COOKIE} -S mix run --no-halt > $LOG
else
    elixir --sname ${NODE} --cookie ${COOKIE} -S mix run --no-halt
fi
