#!/bin/bash

git fetch --all
git reset --hard HEAD
git pull

mix deps.get
