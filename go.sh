#!/bin/bash
erl -pa ebin/ -pa deps/*/ebin -setcookie spapi -name proper_samples
