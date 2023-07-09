#!/bin/bash

killall beam.smp

if [ "$MODE" = "iex" ]
then
    iex --name ${NODE} --cookie ${COOKIE} -S mix
elif [ -n "$LOG" ]
then
    elixir --name ${NODE} --cookie ${COOKIE} -S mix run --no-halt > $LOG
else
    elixir --name ${NODE} --cookie ${COOKIE} -S mix run --no-halt
fi
