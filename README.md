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
cp rel/extra.config.dist rel/extra.config
```

And set `slack_token`
### Usage

In order to run server execute command:

```
make run
```

or 

```
cp docker-compose.yml.dist docker-compose.yml

docker-compose up -d
```

To add movie, send YouTube url to Your bot using Slack.

In order to play songs visit [localhost:7777](localhost:7777)

<img width="1321" alt="screen shot 2018-07-12 at 08 28 28" src="https://user-images.githubusercontent.com/14317604/42616225-bb552e44-85ad-11e8-96fc-387763faa748.png">


### Available message commands

To manage playlist type normal message.

```
next
```

```
volume 0-100
```

```
delete
```

```
seek <int>
```

```
pause
```

```
previous
```


## Built With

* [Erlang](http://erlang.org/doc/index.html)
* [Cowboy](https://ninenines.eu/)

## License

MIT - [read license](LICENSE)

## Todo

* print queue
* print current
