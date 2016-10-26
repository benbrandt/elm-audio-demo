require('./main.css');
require('./buccaneers.mp3');

var hark = require('hark');

var Elm = require('./Main.elm');

var root = document.getElementById('root');

var app = Elm.Main.embed(root);

// Setup Hark listeners
app.ports.setup.subscribe(function setup() {
  var audio = document.getElementById('audiofile');

  // Hark setup to detect when speaking
  var speechEvents = hark(audio);

  speechEvents.on('speaking', function speaking() {
    app.ports.speaking.send({
      speaking: true,
      timestamp: audio.currentTime
    });
  });

  speechEvents.on('stopped_speaking', function stoppedSpeaking() {
    app.ports.speaking.send({
      speaking: false,
      timestamp: audio.currentTime
    });
  });
});

// Subscribe to play messages
app.ports.play.subscribe(function play() {
  var audio = document.getElementById('audiofile');
  audio.play();
});

// Subscribe to pause messages
app.ports.pause.subscribe(function pause() {
  var audio = document.getElementById('audiofile');
  audio.pause();
});

