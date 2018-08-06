PlayerClient = function (wssHost) {
    var webSocket;
    var timeout = 5000;
    var player;
    var keepAliveInterval;
    var clientPaused = false;
    var currentVideo;
    var playedVideos = [];

    function init() {
        print('<span style="color: green;">INIT PLAYER </span>');

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
        print('<span style="color: green;">CONNECTING: ' + wssHost + '</span>');

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
        print('<span style="color: green;">CONNECTED </span>');

        if (clientPaused === false) {
            sendVideoIdRequest();
        }
    }

    function onMessage(evt) {
        print('<span style="color: blue;">RESPONSE: ' + evt.data + '</span>');

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
                print('<span style="color: blue;">ACTION NOT HANNDLED: ' + msg.action + '</span>');
        }
    }

    function onError() {
        print('<span style="color: red;">ERROR </span>');
    }

    function onClose(evt) {
        print('<span style="color: red;">CLOSE: ' + evt.code + ' ' + evt.reason + '</span>');

        if (evt.code === 1000) {
            print('<span style="color: green;">CONNECTION SUCCESSFULLY COMPLETED </span>');
            clearInterval(keepAliveInterval);
            print('<span style="color: green;">INTERVAL CLEARED </span>');
            pauseVideo();
        }
    }

    function onPlayerStateChange(event) {
        print('<span style="color: green;">PLAYER STATE: ' + translatePlayerState(event.data) + '</span>');

        if (event.data === YT.PlayerState.ENDED) {
            setTimeout(sendVideoIdRequest, 2000);
        }

        if (event.data === YT.PlayerState.PLAYING) {
            print('<span style="color: green;">VIDEO URL: ' + player.getVideoUrl() + '</span>');
            setVideoTitle(player.getVideoData().title);
        }
    }

    function playVideo(video) {
        currentVideo = video;
        player.loadVideoById(video.movieId);
        print('<span style="color: green;">VIDEO LOADED: ' + video.movieId + '</span>');
    }

    function pauseVideo() {
        player.pauseVideo();
    }

    function setVolume(volume) {
        player.setVolume(volume);
        print('<span style="color: green;">CURRENT VOLUME: ' + volume + '</span>');
    }

    function seek(to) {
        player.seekTo(to);
        print('<span style="color: green;">SEEK TO: ' + to + '</span>');
    }

    function setQuality(quality) {
        player.setPlaybackQuality(quality);
        print('<span style="color: green;">CURRENT QUALITY: ' + quality + '</span>');
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
                break;
            case -0:
                return "ENDED";
                break;
            case 1:
                return "PLAYING";
                break;
            case 2:
                return "PAUSED";
                break;
            case 3:
                return "BUFFERING";
                break;
            case 5:
                return "VIDEO CUED";
                break;
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

    function print(html) {
        var node = document.createElement("p");
        node.innerHTML = '<span>' + new Date().toISOString() + ': </span>' + html;

        var parent = document.getElementById("output");

        if (parent.children.length > 100) {
            parent.removeChild(parent.lastChild);
        }

        parent.insertBefore(node, parent.firstChild);
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
