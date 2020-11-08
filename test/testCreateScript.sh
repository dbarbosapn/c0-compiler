#!/bin/bash

gcc -Wall testCreation.c -o testCreation

$(./testCreation testExample.txt ParserTest.hs)

rm testCreation
