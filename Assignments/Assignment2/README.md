# Description
The code is implemented in erlang.

* Compiling
$ erl -make

* Cleaning
$ make clean

* Constants are provided in ./include/constants.hrl. Right now default
  time between sending measurements is 5sec instead of 480 ms and
  number of tch per cluster is 1 for the sake of demo purpose.

# Demo 1 for successful handoff

* I assume you have compiled the code.

$ cd ebin
$ erl

* This will start the erlang interpreter. From now on don't forget
  putting '.' at the end of each command.

$ l(start).
$ start:start_normal().

* This will start MSC , 2 BSC with each having a BS and a MS connected
  to one of the BS with an ongoing call.

* Then after 6 seconds it will automatically decrease the signal
  strength of the mobiles BS thus initiating handoff.

# Demo 2 for unsuccessful handoff

* Assuming code is compiled and you are in ebin directory

$ erl
$ l(start).
$ start:start_failure().

* This situation is similar to previous except the other BS has already a MS connected so there are no free tch and the new BSC thus call is dropped.
