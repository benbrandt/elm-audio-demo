require('./main.css');
require('./buccaneers.mp3');

var Elm = require('./Main.elm');

var root = document.getElementById('root');

var app = Elm.Main.embed(root);

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

