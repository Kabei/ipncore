#!/bin/bash

git reset --hard HEAD
git pull

mix deps.get
mix deps.clean --unlock --unused
