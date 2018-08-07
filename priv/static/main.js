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

PlayerClient = function (wssHost, logger) {
    var webSocket;
    var timeout = 5000;
    var player;
    var keepAliveInterval;
    var clientPaused = false;
    var currentVideo;
    var playedVideos = [];

    function init() {
        logger.info('INIT PLAYER');

        player = new YT.Player('player', {
            height: '600',
            width: '800',
            events: {
                'onReady': onPlayerReady,
                'onStateChange': onPlayerStateChange
            }
        });
    }

    function onPlayerReady() {
        setVolume(50);
        setQuality("small");

        keepAliveInterval = setInterval(keepAlive, timeout);

        connect();
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
                setVolume(msg.level);
                break;
            case "seek":
                seek(msg.to);
                break;
            case "pause":
                clientPaused = true;
                pauseVideo();
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
            pauseVideo();
        }
    }

    function onPlayerStateChange(event) {
        logger.info('PLAYER STATE: ' + translatePlayerState(event.data));

        if (event.data === YT.PlayerState.ENDED) {
            setTimeout(sendVideoIdRequest, 2000);
        }

        if (event.data === YT.PlayerState.PLAYING) {
            logger.info('VIDEO URL: ' + player.getVideoUrl());
            setVideoTitle(player.getVideoData().title);
        }
    }

    function playVideo(video) {
        currentVideo = video;
        player.loadVideoById(video.movieId);
        logger.info('VIDEO LOADED: ' + video.movieId);
    }

    function pauseVideo() {
        player.pauseVideo();
    }

    function setVolume(volume) {
        player.setVolume(volume);
        logger.info('CURRENT VOLUME: ' + volume);
    }

    function seek(to) {
        player.seekTo(to);
        logger.info('SEEK TO: ' + to);
    }

    function setQuality(quality) {
        player.setPlaybackQuality(quality);
        logger.info('CURRENT QUALITY: ' + quality);
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

    function keepAlive() {
        if (webSocket.readyState === webSocket.OPEN) {
            webSocket.send(JSON.stringify({action: "ping"}));
        }

        if (webSocket.readyState === webSocket.CLOSED) {
            connect();
        }
    }

    function setVideoTitle(title) {
        var titleElement = document.getElementById("title");
        titleElement.innerHTML = title;
    }

    function reconnectSlack() {
        webSocket.send(JSON.stringify({action: "reconnect_slack"}));
    }

    return {
        'init': init,
        'reconnectSlack': reconnectSlack
    }
};
