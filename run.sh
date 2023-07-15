#!/bin/bash

killall beam.smp

if [ "$MODE" = "iex" ]; then
    iex -S mix
elif [ -n "$LOG" ]; then
    elixir -S mix run --no-halt > $LOG
else
    elixir -S mix run --no-halt
fi
