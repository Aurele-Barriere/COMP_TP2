#!/bin/bash

./compile.sh tests/sierpinski
./tests/sierpinski > data
python3 plot.py
