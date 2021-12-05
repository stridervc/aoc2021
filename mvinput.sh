#!/bin/bash

DAY=`date +%d`

mv ~/Downloads/input inputs/input${DAY}.txt && chmod a-w inputs/input${DAY}.txt
