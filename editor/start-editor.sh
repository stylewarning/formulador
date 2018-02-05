#!/bin/bash

sbcl --noinform --non-interactive \
     --eval '(ql:quickload :formulador-editor)' \
     --eval '(formulador-editor::start-editor)' \
     --eval '(uiop:quit)'
