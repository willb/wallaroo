#!/bin/sh
exec erl -boot start_sasl -s reloader -s wallaroo_web -detached
