const app = Elm.Main.init({
  node: document.getElementById('root')
});


const sound = document.createElement('audio');
sound.setAttribute('loop', true);
sound.setAttribute('src', '/alarm.wav');

app.ports.playAlert.subscribe(function (shouldPlay) {
	if (shouldPlay) {
		sound.play();
	} else {
		sound.pause();
	}
})

app.ports.updatePageTitle.subscribe(function (newTitle) {
	if (document.title != newTitle) {
		document.title = newTitle;
	}
})
