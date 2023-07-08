#!/bin/bash

killall beam.smp

git fetch --all
git reset --hard HEAD
git pull

mix deps.get
