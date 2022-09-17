
#!/bin/bash

export MIX_ENV=prod POST_PATH="~/posts"

mix release.init
mix release

cd _build/prod/rel

zip -rq ipncore.zip ipncore

mv ipncore.zip ../../../

unset MIX_ENV