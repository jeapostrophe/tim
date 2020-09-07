#!/bin/sh
set -e -x

stack build --fast
PATH="$(dirname $(stack exec -- which org2tim)):$PATH"

(tim || [ $? -eq 1 ])

rm -fr .tim
org2tim /Users/jay/Dev/scm/github.jeapostrophe/home/etc/time.org
du -hac .tim

tim help

(tim zelp || [ $? -eq 1 ])

tim sum

tim sum Games
tim sum Work:UML-301:2017 Spring

tim sum Test
(tim start Test || [ $? -eq 1 ])
(tim || [ $? -eq 1 ])

tim new Test
tim sum Test
tim start Test

tim
sleep 1
tim

tim stop
tim sum Test
tim
