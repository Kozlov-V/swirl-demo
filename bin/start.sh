#!/bin/bash
exec erl -sname 'swirl-demo' -pa ebin deps/*/ebin -boot start_sasl +K true -s swirl_demo