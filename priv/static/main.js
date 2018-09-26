HtmlLogger = function () {
    function log(color, text) {
        var html = '<span style="color: ' + color + ';">' + text + '</span>';
        var node = document.createElement("p");
        node.innerHTML = '<span>' + new Date().toISOString() + ': </span>' + html;

        var parent = document.getElementById("output");

        if (parent.children.length > 100) {
            parent.removeChild(parent.lastChild);
        }

        parent.insertBefore(node, parent.firstChild);
    }

    function info(text) {
        log('green', text);
    }

    function debug(text) {
        log('blue', text);
    }

    function error(text) {
        log('red', text);
    }

    return {
        'info': info,
        'debug': debug,
        'error': error
    }
};

YTPlayer = function (configuration, logger) {
    var player;

    function init() {
        logger.info('INIT PLAYER');

        player = new YT.Player('player', {
            height: '600',
            width: '800',
            events: {
                'onReady': function () {
                    setVolume(50);
                    setQuality("small");
                    configuration.onPlayerReady();
                },
                'onStateChange': function (event) {
                    logger.info('PLAYER STATE: ' + translatePlayerState(event.data));

                    if (event.data === YT.PlayerState.ENDED) {
                        configuration.onVideoEnd();
                    }

                    if (event.data === YT.PlayerState.PLAYING) {
                        configuration.onVideoStart();
                    }
                }
            }
        });
    }

    function setVolume(volume) {
        player.setVolume(volume);
        logger.info('CURRENT VOLUME: ' + volume);
    }

    function getVolume() {
        logger.info('GETTING VOLUME');

        return player.getVolume();
    }

    function setQuality(quality) {
        player.setPlaybackQuality(quality);
        logger.info('CURRENT QUALITY: ' + quality);
    }

    function getVideoTitle() {
        return player.getVideoData().title
    }

    function play(video) {
        player.loadVideoById(video.movieId);
        logger.info('VIDEO LOADED: ' + video.movieId);
    }

    function pause() {
        player.pauseVideo();
    }

    function seekTo(to) {
        player.seekTo(to);
        logger.info('SEEK TO: ' + to);
    }

    function translatePlayerState(state) {
        switch (state) {
            case -1:
                return "UNSTARTED";
            case -0:
                return "ENDED";
            case 1:
                return "PLAYING";
            case 2:
                return "PAUSED";
            case 3:
                return "BUFFERING";
            case 5:
                return "VIDEO CUED";
            default:
                return "UNKNOWN";
        }
    }

    init();

    return {
        'setVolume': setVolume,
        'getVolume': getVolume,
        'getVideoTitle': getVideoTitle,
        'play': play,
        'pause': pause,
        'seekTo': seekTo
    }
};

Agner = function (configuration, logger) {
    var webSocket;
    var keepAliveInterval = setInterval(keepAlive, 5000);

    function connect() {
        logger.info('CONNECTING: ' + configuration.host);

        webSocket = new WebSocket(configuration.host);
        webSocket.onopen = function () {
            logger.info('CONNECTED');

            configuration.onOpen()
        };
        webSocket.onclose = function (evt) {
            logger.error('CLOSE: ' + evt.code + ' ' + evt.reason);

            if (evt.code === 1000) {
                logger.info('CONNECTION SUCCESSFULLY COMPLETED');
                clearInterval(keepAliveInterval);
                logger.info('INTERVAL CLEARED');
            }

            configuration.onClose(evt)
        };
        webSocket.onmessage = function (evt) {
            logger.debug('MESSAGE: ' + evt.data);

            configuration.onMessage(evt)
        };
        webSocket.onerror = function () {
            logger.error('ERROR');
        };
    }

    function keepAlive() {
        if (webSocket.readyState === webSocket.OPEN) {
            send({action: "ping"});
        }

        if (webSocket.readyState === webSocket.CLOSED) {
            connect();
        }
    }

    function sendVideoIdRequest() {
        send({action: "get"});
    }

    function sendDeleteCurrentVideoRequest(movieId) {
        send({action: "delete", movieId: movieId});
    }

    function reconnectSlack() {
        send({action: "reconnect_slack"});
    }

    function answer(messageId, answer) {
        send({action: "answer", messageId: messageId, answer: answer});
    }

    function send(message) {
        webSocket.send(JSON.stringify(message));
    }

    connect();

    return {
        'sendVideoIdRequest': sendVideoIdRequest,
        'sendDeleteCurrentVideoRequest': sendDeleteCurrentVideoRequest,
        'reconnectSlack': reconnectSlack,
        'answer': answer
    }
};

Player = function (wssHost, logger) {
    var player;
    var agner;
    var clientPaused = false;
    var currentVideo;
    var playedVideos = [];

    function init() {
        var connectionConfiguration = {
            'host': wssHost,
            'onOpen': onOpen,
            'onMessage': onMessage,
            'onClose': onClose
        };

        var YTPlayerConfiguration = {
            'onPlayerReady': function () {
                agner = new Agner(connectionConfiguration, logger)
            },
            'onVideoEnd': function () {
                setTimeout(sendVideoIdRequest, 2000);
            },
            'onVideoStart': function () {
                setVideoTitle()
            }
        };

        player = new YTPlayer(YTPlayerConfiguration, logger);
    }

    function onOpen() {
        if (clientPaused === false) {
            sendVideoIdRequest();
        }
    }

    function onMessage(evt) {
        var msg = JSON.parse(evt.data);

        switch (msg.action) {
            case "play":
                var video = {
                    "movieId": msg.movieId,
                    "source": msg.source
                };

                playedVideos.push(video);
                playVideo(video);
                break;
            case "next":
                sendVideoIdRequest();
                break;
            case "added_to_empty_queue":
                if (currentVideo !== undefined && currentVideo.source !== "queue") {
                    sendVideoIdRequest();
                }
                break;
            case "volume":
                player.setVolume(msg.level);
                break;
            case "seek":
                player.seekTo(msg.to);
                break;
            case "pause":
                clientPaused = true;
                player.pause();
                break;
            case "delete":
                sendDeleteCurrentVideoRequest();
                break;
            case "deleted":
                sendVideoIdRequest();
                break;
            case "previous":
                previous();
                break;
            case "now":
                sendCurrentMovieTitle(msg.messageId);
                break;
            case "say":
                say(msg.text);
                break;
            case "pong":
                break;
            default:
                logger.debug('ACTION NOT HANDLED: ' + msg.action);
        }
    }

    function onClose(evt) {
        if (evt.code === 1000) {
            player.pause();
        }
    }

    function playVideo(video) {
        currentVideo = video;
        player.play(video);
    }

    function sendVideoIdRequest() {
        clientPaused = false;
        agner.sendVideoIdRequest()
    }

    function previous() {
        if (playedVideos.length === 1) {
            playVideo(playedVideos[0]);
        } else {
            playVideo(playedVideos.pop());
        }
    }

    function sendDeleteCurrentVideoRequest() {
        agner.sendDeleteCurrentVideoRequest(currentVideo.movieId);
    }

    function sendCurrentMovieTitle(messageId) {
        logger.info('now on ' + messageId);
        agner.answer(messageId, player.getVideoTitle())
    }

    function setVideoTitle() {
        var videoTitle = document.getElementById("title");
        videoTitle.innerHTML = player.getVideoTitle();

        var pageTitle = document.getElementsByTagName("title")[0];
        pageTitle.innerHTML = 'Agner - ' + player.getVideoTitle();
    }

    function reconnectSlack() {
        agner.reconnectSlack()
    }

    function say(text) {
        if (window.speechSynthesis.speaking) {
            setTimeout(say, 100, text);
        } else {
            setTimeout(
                function (text) {
                    var msg = new SpeechSynthesisUtterance(text);
                    var previousVolume = player.getVolume();

                    msg.lang = 'pl-PL';
                    msg.onend = function () {
                        player.setVolume(previousVolume);
                    };

                    player.setVolume(previousVolume * 0.1);
                    window.speechSynthesis.speak(msg);
                },
                100,
                text
            )
        }
    }

    init();

    return {
        'reconnectSlack': reconnectSlack
    }
};
