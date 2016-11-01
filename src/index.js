require('./main.css');
require('./buccaneers.mp3');

var hark = require('hark');
var Horizon = require('@horizon/client');
var Elm = require('./Main.elm');

var root = document.getElementById('root');

var app = Elm.Main.embed(root);

// Horizon
var horizon = Horizon({ host: 'localhost:8181' });
horizon.onReady(function() {
  console.log('elm_audio_db works!');
});
horizon.connect();

// Setup AudioSegment Collection
var audioSegments = horizon('segments');

// Log DB changes
audioSegments.watch().subscribe(
  function success(items) {
    items.forEach(function (item) {
      console.log(item);
    })
  },
  function error(err) {
    console.log(err);
  }
);

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

// Save Audio Segments to Database
app.ports.save.subscribe(function save(segments) {
  audioSegments.store({ segments: segments });
});

