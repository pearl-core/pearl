#!/usr/bin/env sh

hash -r
git --version
bash --version
zsh --version
fish --version
./tests/bunit/bin/checkstyle.sh ./bin/ ./lib/
./tests/bunit/bin/unit-tests.sh ./tests/unit-tests
bash ./tests/test-utils/integ-tests/integ-tests.sh
zsh ./tests/test-utils/integ-tests/integ-tests.sh
fish ./tests/test-utils/integ-tests/integ-tests.fish
