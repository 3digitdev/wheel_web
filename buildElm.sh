#!/bin/bash

# Standard building

# Compile Elm
# elm make src/Main.elm --optimize --output=public/wheel.js

# Debug Commands
elm-live src/Main.elm --dir=public --start-page=index.html -- --output=public/wheel.js
