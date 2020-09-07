#!/bin/sh
set -e -x

stack build --fast

rm -fr .tim
stack exec -- org2tim /Users/jay/Dev/scm/github.jeapostrophe/home/etc/time.org
du -hac .tim

stack exec -- tim help

(stack exec -- tim zelp || [ $? -eq 1 ])

stack exec -- tim sum

stack exec -- tim sum :Games
stack exec -- tim sum :Work:UML-301:2017 Spring

(stack exec -- tim start Test || [ $? -eq 1 ])
stack exec -- tim new Test
stack exec -- tim start Test

stack exec -- tim
sleep 1
stack exec -- tim

stack exec -- tim stop