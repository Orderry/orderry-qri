#!/bin/sh

SNAME='orderry-qri'

env HOME=.
export HOME=.

rebar compile
erl -sname $SNAME -pa ebin deps/*/ebin -s qri_app | ccze -A
