
#!/bin/bash

export MIX_ENV=prod
rm -rf _build
mix deps.get
mix release ipncore
cp _build/prod/rel/ipncore .