# ultron

A Slack Bot for UNIX-style ChatOps

Most ChatOps bots require that all plugins are written in their host language.

*ultron* follows the UNIX Philosophy wherein every command is just a program.

This allows commands to be written in any programming language.

# Usage

    $ ultron --help
    ultron - a Unix-style ChatOps bot for Slack

    Usage: ultron CONFIG

    Available options:
      -h,--help                Show this help text


## Configure

See [example.cfg](example.cfg)

## Built-In Commands

* `help` -- list available commands

## Writing Commands

Commands are just programs (The UNIX Philosophy)

If the exit code of the command program is zero,
STDOUT is written directly to slack unformatted.
Formatting is up to the command program. See [Slack Message Formatting](https://api.slack.com/docs/formatting).

If the exit code of the command program is non-zero,
the command is treated as a failure
and the STDOUT and STDERR printed if they had any output.

The following environment variables are passed to command programs:

* `ULTRON_UID` -- the user id that called the command
* `ULTRON_CID` -- the channel id where the command was called


# Build

```
stack build
```

# Known Issues

* Responses are logged to stdout.
* Some message types are not handled properly: [mpickering/slack-api#37](https://github.com/mpickering/slack-api/issues/37)

# Wishlist (PRs Welcome)

* Quoted Strings (with escaping)
* UNIX pipes
* Process Control (list running commands and kill them)
* Attachments Support
