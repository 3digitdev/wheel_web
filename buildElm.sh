#!/bin/bash

# Standard building

# Compile Elm
# elm make src/Main.elm --optimize --output=public/wheel.js

# Debug Commands
elm make src/Main.elm --output=public/wheel.js
elm reactor
