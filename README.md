# Agner - Slack Bot

Slack bot to create and control playlist based on YouTube movies.

## Table of contents
* [Installing](#installing)
* [Usage](#usage)
* [Available message commands](#available-message-commands)
* [Built With](#built-with)
* [License](#license)
* [TODO](#todo)

### Installing

Create [Bot User](https://api.slack.com/bot-users) and generate `slack_token` for Slack API communication. 

```
cp rel/extra.config.dist rel/config.dist
```

And set `slack_token`

```
make run
```

Visit [localhost:7777](localhost:7777)

### Usage

* To add movie, send YouTube url to Your bot.

* To manage playlist type normal message.

### Available message commands

```
next
```

```
volume 0-100
```

## Built With

* [Erlang](http://erlang.org/doc/index.html)
* [Cowboy](https://ninenines.eu/)

## License

MIT - [read license](LICENSE)

## Todo

* delete song
* seek to
* print queue
* print current