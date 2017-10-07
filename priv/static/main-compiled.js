"use strict";

var PlayerClient = function PlayerClient(wssHost) {
    var _this = this;

    var webSocket = void 0;
    var timerId = void 0;

    var keepAlive = function keepAlive() {
        var timeout = 20000;
        if (webSocket.readyState === webSocket.OPEN) {
            webSocket.send(JSON.stringify({ action: "ping" }));
        }
        timerId = setTimeout(keepAlive, timeout);
    };

    var cancelKeepAlive = function cancelKeepAlive() {
        if (timerId) {
            clearTimeout(timerId);
        }
    };

    var init = function init() {
        webSocket = new WebSocket(wssHost);
        webSocket.onopen = function () {
            onOpen();
        };
        webSocket.onclose = function (evt) {
            onClose(evt);
        };;
        webSocket.onmessage = function (evt) {
            onMessage(evt);
        };
        webSocket.onerror = function (evt) {
            onError(evt);
        };
    };

    var onOpen = function onOpen() {
        setupPlayer();
        keepAlive();
    };

    var onClose = function onClose(evt) {
        cancelKeepAlive();
    };

    var onMessage = function onMessage(evt) {
        var msg = JSON.parse(evt.data);

        console.log(msg.action);
        console.log(_this.currentVideo ? _this.currentVideo : 'no current video');

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
                if (!_this.currentVideo && _this.currentVideo.source !== "queue") {
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
    };

    var onError = function onError(evt) {
        cancelKeepAlive();
    };

    //===========================================
    //==================== YT ===================
    //===========================================

    var player = void 0;

    var setupPlayer = function setupPlayer() {
        player = new YT.Player('player', {
            height: '360',
            width: '640',
            events: {
                'onReady': sendVideoIdRequest,
                'onStateChange': onPlayerStateChange
            }
        });
    };

    var onPlayerStateChange = function onPlayerStateChange(event) {
        //        YT.PlayerState.UNSTARTED
        //        YT.PlayerState.ENDED
        //        YT.PlayerState.PLAYING
        //        YT.PlayerState.PAUSED
        //        YT.PlayerState.BUFFERING
        //        YT.PlayerState.CUED
        if (event.data === YT.PlayerState.ENDED) {
            setTimeout(sendVideoIdRequest, 2000);
        }
    };

    var playVideo = function playVideo(video) {
        _this.currentVideo = video;
        player.loadVideoById(video.movieId);
    };

    var setVolume = function setVolume(volume) {
        player.setVolume(volume);
    };

    var sendVideoIdRequest = function sendVideoIdRequest() {
        webSocket.send(JSON.stringify({ action: "get" }));
    };

    return {
        'init': init
    };
};
