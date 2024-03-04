#!/bin/bash

killall beam.smp
sleep 2
# killall epmd
# epmd -daemon

cpus=$(nproc)
total_pids=2000000

# if not defined
# if [ -z "$MIX_ENV" ]; then
#   export MIX_ENV=prod
# fi

if [ -z "$LOG" ]; then
   export LOG=nohup.out
fi

if [ "$MODE" = "iex" ]; then
    iex --erl "+A $cpus +P $total_pids" -S mix
else
    nohup elixir --erl "+A $cpus +P $total_pids" -S mix run --no-halt --no-compile > $LOG 2>&1 &
fi
