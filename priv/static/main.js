const PlayerClient = function (wssHost) {
    let webSocket;
    let timerId;

    const keepAlive = () => {
      const timeout = 20000;
      if (webSocket.readyState === webSocket.OPEN) {
        webSocket.send(JSON.stringify({action: "ping"}));
      }
      timerId = setTimeout(keepAlive, timeout);
    }

    const cancelKeepAlive = () => {
        if (timerId) {
            clearTimeout(timerId);
        }
    }

    const init = () => {
        webSocket = new WebSocket(wssHost);
        webSocket.onopen = () => {
            onOpen();
        };
        webSocket.onclose = (evt) => {
            onClose(evt);
        };;
        webSocket.onmessage = (evt) => {
            onMessage(evt);
        };
        webSocket.onerror = (evt) => {
            onError(evt);
        };
    }

    const onOpen = () => {
        setupPlayer();
        keepAlive();
    }

    const onClose = (evt) => {
        cancelKeepAlive();
    }

    const onMessage = (evt) => {
        const msg = JSON.parse(evt.data);

        console.log(msg.action);
        console.log(this.currentVideo ? this.currentVideo : 'no current video');

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
                if (!this.currentVideo && this.currentVideo.source !== "queue") {
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

    const onError = (evt) => {
      cancelKeepAlive();
    }

    //===========================================
    //==================== YT ===================
    //===========================================

    let player;

    const setupPlayer = () => {
      player = new YT.Player('player', {
          height: '360',
          width: '640',
          events: {
            'onReady': sendVideoIdRequest,
            'onStateChange': onPlayerStateChange
          }
      });
    }

    const onPlayerStateChange = (event) => {
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

    const playVideo = (video) => {
      this.currentVideo = video;
      player.loadVideoById(video.movieId);
    }

    const setVolume = (volume) => {
      player.setVolume(volume);
    }

    const sendVideoIdRequest = () => {
      webSocket.send(JSON.stringify({action: "get"}));
    }

    return {
      'init': init
    }
};
