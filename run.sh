#!/bin/bash

killall beam.smp

cpus=$(nproc)
total_pids=2000000

# if not defined
if [ -z "$MIX_ENV" ]; then
  export MIX_ENV=prod
fi

if [ "$MODE" = "iex" ]; then
    iex --erl "+A $cpus +P $total_pids" -S mix
elif [ -n "$LOG" ]; then
    elixir --erl "+A $cpus +P $total_pids" -S mix run --no-halt > $LOG
else
    elixir --erl "+A $cpus +P $total_pids" -S mix run --no-halt
fi
