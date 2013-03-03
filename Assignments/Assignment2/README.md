# Description
The code is implemented in erlang.

* Compiling
$ erl -make

* Cleaning
$ make clean

* Constants are provided in ./include/constants.hrl. Right now default
  time between sending measurements is 5sec instead of 480 ms and
  number of tch per cluster is 1 for the sake of demo purpose.

# Demo 1 for intra cell handoff

* I assume you have compiled the code.

$ cd ebin
$ erl

* This will start the erlang interpreter. From now on don't forget
  putting '.' at the end of each command.

$ l(start).
$ start:start_intra().

* This will start MSC , 1 BSC with having 2 BS and a MS connected
  to one of the BS with an ongoing call.

* Then after 6 seconds it will automatically decrease the signal
  strength of the mobile thus initiating handoff.

# Demo 2 for channel borrowing

* Assuming code is compiled and you are in ebin directory

$ erl
$ l(start).
$ start:start_switch1().
$ ms2 ! {endcall}.
$ ms ! {endcall}.

* This consist of two BS with each having one mobile. When handoff
  occurs from bs1 to bs2 there are two mobiles at bs2 so when one call
  ends channel switching takes place from higher channel to lower
  channel.

# Demo 3 for channel borrowing 

* Assuming code is compiled and you are in ebin directory

$ erl
$ l(start).
$ start:start_switch2().
$ ms2 ! {endcall}.
$ ms ! {endcall}.

* This is similar to above but now only one channel is available at
  bs2 to which a mobile is already connected. So when ms tries to
  connect to bs2 a borrowing from bs22 takes place. bs11 is the
  cochannel of bs22 so the given channel is locked in bs11.
