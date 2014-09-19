#!/bin/bash

ccl --batch --quiet \
    -e '(ql:quickload :formulador-editor)' \
    -e '(formulador-editor::start-editor)' \
    -e '(quit)'
