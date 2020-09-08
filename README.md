tim - Tim the Time Man

This is a command-line based way to keep track of time.
It was mostly an experiment in writing a program in Haskell entirely in VSCode.

It has a 64-bit header which is POSIX seconds for the start of the "epoch" of the log.
Each entry is 64-bits, with 32-bits as a delta from the epoch, then 16-bits for the duration, and 16-bits for the "topic".

These limits mean it can store about 70 years of logs where each entry is about 18 hours long. Should be enough for anyone, right?

I switched to TimeWarrior though.