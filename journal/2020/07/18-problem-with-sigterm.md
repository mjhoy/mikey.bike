---
title: A problem with SIGTERM
date: 2020-07-18T09:00:00-0400
---

_The Linux Programming Interface_ has this to say about the `SIGTERM` signal:

> This is the standard signal used for terminating a process and is the
> default signal sent by the _kill_ and _killall_ commands. Users sometimes
> explicitly send the `SIGKILL` signal to a process using _kill -KILL_ or
> _kill -9_. However, this is generally a mistake. A well-designed application
> will have a handler for `SIGTERM` that causes the application to exit
> gracefully, cleaning up temporary files and releasing other resources
> beforehand.

Sounds good, right? Especially the bit about releasing other resources. You
might think that a shell script that executes another long-lived process will
terminate that process when it receives a `SIGTERM`. Let's try it. Here's a
simple bash script:

```bash
#!/bin/bash

python -m http.server 8800
```

Call it `start-server.bash` and run it in a terminal:

```
$ bash ./start-server.bash
Serving HTTP on 0.0.0.0 port 8800 (http://0.0.0.0:8800/) ...
```

In another terminal, we can run `ps` to see the two processes: the parent
bash process, and the child python process. We also see two `zsh` processes,
which are the shells running in my two terminals. I'll use the `-j` flag to
get the parent PID of processes (`PPID`).

```
$ ps -j
USER    PID  PPID  PGID   SESS JOBC STAT   TT       TIME COMMAND
mjhoy 62548 62547 62548      0    1 S    s001    0:00.05 -zsh
mjhoy 61001 61000 61001      0    1 S    s002    0:00.30 -zsh
mjhoy 62620 61001 62620      0    1 S+   s002    0:00.01 bash start-server.bash
mjhoy 62621 62620 62620      0    1 S+   s002    0:00.11 /usr/bin/python -m http.server 8800
```

Note that the `PPID` of the python process (62620) is the `PID` of the bash
process. Control-C in the terminal running the server works as expected:

```
^C
Keyboard interrupt received, exiting.
```

And both processes have stopped:

```
$ ps -j
USER    PID  PPID  PGID   SESS JOBC STAT   TT       TIME COMMAND
mjhoy 62548 62547 62548      0    1 S    s001    0:00.05 -zsh
mjhoy 61001 61000 61001      0    1 S    s002    0:00.30 -zsh
```

Control-C sends the `SIGINT` signal to the foreground process in the
terminal. The bash process properly forwards this along to its child
processes, as you would expect. `SIGTERM`, however, _does not_ get forwarded
to the subprocesses. Again, let's start a server, and get `ps` from another
terminal:

```
$ ps -j
USER    PID  PPID  PGID   SESS JOBC STAT   TT       TIME COMMAND
mjhoy 62548 62547 62548      0    1 S    s001    0:00.06 -zsh
mjhoy 61001 61000 61001      0    1 S    s002    0:00.35 -zsh
mjhoy 62785 61001 62785      0    1 S+   s002    0:00.01 bash start-server.bash
mjhoy 62786 62785 62785      0    1 S+   s002    0:00.11 /usr/bin/python -m http.server 8800
```

We can send a signal manually using `kill`. If we run `kill -INT -62785`, for
instance, that does the same thing as hitting Control-C in our server
terminal, and both processes (bash and python) exit. But with `TERM` this
doesn't happen:

```
$ kill -TERM 62785
$ ps -j
USER    PID  PPID  PGID   SESS JOBC STAT   TT       TIME COMMAND
mjhoy 62548 62547 62548      0    1 S    s001    0:00.07 -zsh
mjhoy 61001 61000 61001      0    1 S+   s002    0:00.35 -zsh
mjhoy 62786     1 62785      0    0 S    s002    0:00.15 /usr/bin/python -m http.server 8800
```

In the other terminal, it _seems_ like the server was stopped, but in fact
the python server is still happily running on port 8800.

So: beware of `SIGTERM`. It's worth noting that `TERM` is the _default_ kill
signal; i.e., just running `kill 62785` sends the `TERM` signal to that
process.

There are two ways to solve this in the bash script. One is to use `exec`,
which will replace the bash process with the python process, instead of
forking as a subprocess:

```bash
#!/bin/bash

exec python -m http.server 8800
```

The other way is to install a _signal handler_ that will properly kill the
subprocess on receiving a `SIGTERM`. It's a little weird, because you need to
get the pid of the subprocess for the handler, but you can do it like so:

```bash
#!/bin/bash

handle_term() {
  kill -TERM "$child_pid"
}

trap handle_term SIGTERM
python -m http.server 8800 &
child_pid=$!
wait "$child_pid"
```

I ran into an issue with this at work, because the `foreman` ruby gem, which
is a popular way in rubyland to manage multiple processes, sends the
`SIGTERM` signal to child processes if any other child process fails to
start. So, if any child process is a bash script that starts a server and
doesn't explicitly handle `SIGTERM`, _or_ (worse) if you happen to define a
subprocess like so:

```
cd some_directory && ./run_server
```

you will find yourself with server processes that continue to run after
`foreman` exits. The only thing then is to hunt them down with `lsof` and
kill them manually.
