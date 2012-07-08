#!/bin/bash
erl -pa ebin/ -pa deps/*/ebin -setcookie spapi -name testing_samples
