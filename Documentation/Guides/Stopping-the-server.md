# Stopping the server #

## Manual ##

To cleanly stop the iTasks server you can can either

- Start the `shutdown` task from the `iTasks.WF.Tasks.System` module, the server will stop with the specified exit code.
- Send a SIGINT or SIGTERM signal to the application (i.e. by pressing CTRL+C), the server will stop with exit code 1

This will gracefully close all connections and stop the server.

Other ways of stopping the server (e.g. sending a SIGKILL signal) may result
in corrupted data for shares and tasks.

## Automatic ##

In some cases, the iTasks server will automatically terminate.

- If there are only startup tasks defined which are all stable the server will stop with exit code 0
- If there is an uncaught exception in a startup task the server will stop with exit code 1
