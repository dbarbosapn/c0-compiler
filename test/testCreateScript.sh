#!/bin/bash

gcc -Wall ./test/testCreation.c -o ./test/testCreation

$(./test/testCreation ./test/testExample.txt ./test/ParserTest.hs)

rm ./test/testCreation
