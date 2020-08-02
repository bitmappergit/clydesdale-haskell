#!/bin/csh -fx

ld -N -A $argv[1] -T $argv[2] -o $argv[3] $argv[5-]
if ($status != 0) exit 1

nm -gp $argv[3] > $argv[4]
if ($status != 0) exit 2

exit 0
