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
        'getVideoTitle': getVideoTitle,
        'play': play,
        'pause': pause,
        'seekTo': seekTo
    }
};

PlayerClient = function (wssHost, logger) {
    var webSocket;
    var timeout = 5000;
    var player;
    var keepAliveInterval;
    var clientPaused = false;
    var currentVideo;
    var playedVideos = [];

    function init() {
        var configuration = {
            'onPlayerReady': function () {
                keepAliveInterval = setInterval(keepAlive, timeout);
                connect();
            },
            'onVideoEnd': function () {
                setTimeout(sendVideoIdRequest, 2000);
            },
            'onVideoStart': function () {
                setVideoTitle()
            }
        };

        player = new YTPlayer(configuration, logger);
    }

    function connect() {
        logger.info('CONNECTING: ' + wssHost);

        webSocket = new WebSocket(wssHost);
        webSocket.onopen = function () {
            onOpen()
        };
        webSocket.onclose = function (evt) {
            onClose(evt)
        };
        webSocket.onmessage = function (evt) {
            onMessage(evt)
        };
        webSocket.onerror = function () {
            onError()
        };
    }

    function onOpen() {
        logger.info('CONNECTED');

        if (clientPaused === false) {
            sendVideoIdRequest();
        }
    }

    function onMessage(evt) {
        logger.debug('RESPONSE: ' + evt.data);

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
            case "pong":
                break;
            default:
                logger.debug('ACTION NOT HANNDLED: ' + msg.action);
        }
    }

    function onError() {
        logger.error('ERROR');
    }

    function onClose(evt) {
        logger.error('CLOSE: ' + evt.code + ' ' + evt.reason);

        if (evt.code === 1000) {
            logger.info('CONNECTION SUCCESSFULLY COMPLETED');
            clearInterval(keepAliveInterval);
            logger.info('INTERVAL CLEARED');
            player.pause();
        }
    }

    function playVideo(video) {
        currentVideo = video;
        player.play(video);
    }

    function sendVideoIdRequest() {
        clientPaused = false;
        webSocket.send(JSON.stringify({action: "get"}));
    }

    function previous() {
        if (playedVideos.length === 1) {
            playVideo(playedVideos[0]);
        } else {
            playVideo(playedVideos.pop());
        }
    }

    function sendDeleteCurrentVideoRequest() {
        webSocket.send(JSON.stringify({
            action: "delete",
            movieId: currentVideo.movieId
        }));
    }

    function keepAlive() {
        if (webSocket.readyState === webSocket.OPEN) {
            webSocket.send(JSON.stringify({action: "ping"}));
        }

        if (webSocket.readyState === webSocket.CLOSED) {
            connect();
        }
    }

    function setVideoTitle() {
        var titleElement = document.getElementById("title");
        titleElement.innerHTML = player.getVideoTitle();
    }

    function reconnectSlack() {
        webSocket.send(JSON.stringify({action: "reconnect_slack"}));
    }

    init();

    return {
        'reconnectSlack': reconnectSlack
    }
};
