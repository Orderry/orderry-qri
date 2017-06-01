#!/usr/bin/env bash

# Script for starting as systemd service

erl -sname orderry-qri -pa ebin deps/*/ebin -s qri_app -config raven +K true -noinput
