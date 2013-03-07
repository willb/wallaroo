#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/apps/*/ebin $PWD/deps/*/ebin -boot start_sasl -s reloader -s wallaroo_web
