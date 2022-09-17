#!/bin/bash

mkdir /root/posts
export POSTS_PATH="/root/posts"

unzip -q ipncore.zip

cd ipncore/bin

mkdir priv

mv ../lib/ipncore-0.1.0/priv/cert ./

./ipncore start

unset MIX_ENV

