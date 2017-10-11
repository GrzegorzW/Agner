function es6BindAll(context, methodNames) {
    methodNames.map(function(methodName) {
    context[methodName] = context[methodName].bind(context);
  });
};

class PlayerClient {
  constructor(wssHost) {
    this.webSocket = new WebSocket(wssHost);
    es6BindAll(this, [
      'init', 'onOpen', 'onClose', 'onMessage', 'onError',
      'setupPlayer', 'keepAlive', 'cancelKeepAlive', 'sendVideoIdRequest',
      'onPlayerStateChange', 'setVolume', 'playVideo'
    ]);
  }

  keepAlive() {
    const timeout = 20000;
    if (this.webSocket.readyState === this.webSocket.OPEN) {
      this.webSocket.send(JSON.stringify({action: "ping"}));
    }
    this.timerId = setTimeout(this.keepAlive, timeout);
  }

  cancelKeepAlive() {
    if (this.timerId) {
      clearTimeout(this.timerId);
    }
  }

  init() {
    this.webSocket.onopen = () => {
      this.onOpen();
    };
    this.webSocket.onclose = (evt) => {
      this.onClose(evt);
    };
    this.webSocket.onmessage = (evt) => {
      this.onMessage(evt);
    };
    this.webSocket.onerror = (evt) => {
      this.onError(evt);
    };
  }

  onOpen() {
    this.setupPlayer();
    this.keepAlive();
  }

  onClose(evt) {
    this.cancelKeepAlive();
  }

  onMessage(evt) {
    const msg = JSON.parse(evt.data);

    console.log(msg.action);
    console.log(this.currentVideo ? this.currentVideo : 'no current video');

    switch (msg.action) {
      case "play":
        var video = {
          "movieId": msg.movieId,
          "source": msg.source
        };
        this.playVideo(video);
      break;
      case "next":
        this.sendVideoIdRequest();
      break;
      case "added_to_empty_queue":
        if (!this.currentVideo && this.currentVideo.source !== "queue") {
          this.sendVideoIdRequest();
        }
      break;
      case "volume":
        this.setVolume(msg.level);
      break;
      case "pong":
      break;
      default:
        console.log("default", msg.action);
    }
  }

  onError(evt) {
    this.cancelKeepAlive();
  }

  //===========================================
  //==================== YT ===================
  //===========================================

  setupPlayer() {
    this.player = new YT.Player('player', {
      height: '360',
      width: '640',
      events: {
        'onReady': this.sendVideoIdRequest,
        'onStateChange': this.onPlayerStateChange
      }
    });
  }

  onPlayerStateChange(event) {
    //        YT.PlayerState.UNSTARTED
    //        YT.PlayerState.ENDED
    //        YT.PlayerState.PLAYING
    //        YT.PlayerState.PAUSED
    //        YT.PlayerState.BUFFERING
    //        YT.PlayerState.CUED
    if (event.data === YT.PlayerState.ENDED) {
      setTimeout(this.sendVideoIdRequest, 2000);
    }
  }

  playVideo(video) {
    this.currentVideo = video;
    this.player.loadVideoById(video.movieId);
  }

  setVolume(volume) {
    this.player.setVolume(volume);
  }

  sendVideoIdRequest() {
    this.webSocket.send(JSON.stringify({action: "get"}));
  }
};
