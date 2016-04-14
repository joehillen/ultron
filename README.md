# ultron

A Slack Bot for UNIX-style ChatOps

Most ChatOps bots require that all plugins are written in the bots' host language.

**ultron** follows The UNIX Philosophy wherein every command is just a program.

This allows commands to be written in any programming language.

**ultron** uses [slack-bot-async](https://github.com/joehillen/slack-bot-async)
and [slack-api-types](https://github.com/joehillen/slack-api-types).

# Usage


    $ ultron --help
    ultron - A Slack Bot for UNIX-style ChatOps

    Usage: ultron [-d|--debug] CONFIG

    Available options:
      -h,--help                Show this help text
      -d,--debug               Set log level to DEBUG


# Build

```
stack build
```

## Configure

See [example.cfg](example.cfg)

## Built-In Commands

* `help` -- list available commands

# Features

* Single and Double Quoted Arguments

## Writing Commands

Commands are just programs (The UNIX Philosophy).

If the exit code of the command program is zero,
STDOUT is written directly to slack unformatted.
Formatting is up to the command program.
See [Slack Message Formatting](https://api.slack.com/docs/formatting).

If the exit code of the command program is non-zero,
the command is treated as a failure
and the STDOUT and STDERR printed if they had any output.

The following environment variables are passed to command programs:

* `ULTRON_UID` -- the user id that called the command
* `ULTRON_CID` -- the channel id where the command was called

# Security

Be cognizant of the security implications of the command programs that you write.

Make sure command programs:

* Do not allow access to arbitrary files.
* Prevent path traversals.
* Properly escape input arguments.
* Do not allow users to call eval functions or shells.

Do not trust user input if you want protect ultron's host system from
malicious input.

For example, it might seem convenient to add a link to `grep` in the path,
but `grep` can accept a file path as an argument. A proper solution would be to
wrap `grep`, escape input, and whitelist specific arguments.

## Access Control

Access control to bot commands is only done via channels.

If a user doesn't not have access to the Slack channel that the bot is listening
in, then they will not have access to the commands.

More granular controls can be implemented in the command's program,
using the `ULTRON_UID` environment variable.

# Wishlist (PRs Welcome)

* UNIX pipes
* Process Control (list running commands and kill them)
* Attachments Support
