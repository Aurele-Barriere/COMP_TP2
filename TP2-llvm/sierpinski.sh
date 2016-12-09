#!/bin/bash

./compile tests/sierpinski
./tests/sierpinski > data
python3 plot.py
