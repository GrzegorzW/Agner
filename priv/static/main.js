PlayerClient = function (wssHost) {
    var webSocket;
    var timerId;

    function keepAlive() {
        var timeout = 20000;
        if (webSocket.readyState === webSocket.OPEN) {
            webSocket.send(JSON.stringify({action: "ping"}));
        }
        timerId = setTimeout(keepAlive, timeout);
    }

    function cancelKeepAlive() {
        if (timerId) {
            clearTimeout(timerId);
        }
    }

    function init() {
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
        webSocket.onerror = function (evt) {
            onError(evt)
        };
    }

    function onOpen() {
        setupPlayer();
        keepAlive();
    }

    function onClose(evt) {
        cancelKeepAlive();
    }

    function onMessage(evt) {
        var msg = JSON.parse(evt.data);

        console.log(msg.action);
        console.log(this.currentVideo);

        switch (msg.action) {
            case "play":
                var video = {
                    "movieId": msg.movieId,
                    "source": msg.source
                };

                playVideo(video);
                break;
            case "next":
                sendVideoIdRequest();
                break;
            case "added_to_empty_queue":
                if (this.currentVideo !== undefined && this.currentVideo.source !== "queue") {
                    sendVideoIdRequest();
                }
                break;
            case "volume":
                setVolume(msg.level);
                break;
            case "pong":
                break;
            default:
                console.log("default", msg.action);
        }
    }

    function onError(evt) {
        cancelKeepAlive();
    }

    //===========================================
    //==================== YT ===================
    //===========================================

    var player;

    function setupPlayer() {
        player = new YT.Player('player', {
            height: '360',
            width: '640',
            events: {
                'onReady': sendVideoIdRequest,
                'onStateChange': onPlayerStateChange
            }
        });
    }

    function onPlayerStateChange(event) {
//        YT.PlayerState.UNSTARTED
//        YT.PlayerState.ENDED
//        YT.PlayerState.PLAYING
//        YT.PlayerState.PAUSED
//        YT.PlayerState.BUFFERING
//        YT.PlayerState.CUED
        if (event.data === YT.PlayerState.ENDED) {
            setTimeout(sendVideoIdRequest, 2000);
        }
    }

    function playVideo(video) {
        this.currentVideo = video;
        player.loadVideoById(video.movieId)
    }

    function setVolume(volume) {
        player.setVolume(volume)
    }

    function sendVideoIdRequest() {
        webSocket.send(JSON.stringify({action: "get"}));
    }

    return {
        'init': init
    }
};