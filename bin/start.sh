#!/bin/bash
exec erl -sname 'swirl-demo' -pa ebin deps/*/ebin +K true -s swirl_demo