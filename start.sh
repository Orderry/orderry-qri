#!/bin/sh

SNAME='orderry-qri'

rebar compile
erl -sname $SNAME -pa ebin deps/*/ebin -s qri_app | ccze -A
