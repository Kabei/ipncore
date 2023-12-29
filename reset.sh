#!/bin/bash

killall beam.smp
rm -R data
rm nohup.out
nohup mix run --no-halt --no-compile > nohup.out 2>&1 &