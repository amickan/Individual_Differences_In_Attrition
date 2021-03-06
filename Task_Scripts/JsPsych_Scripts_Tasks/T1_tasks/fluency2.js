var ppn = jsPsych.data.urlVariables()['ppn']
if (ppn===undefined)
	ppn = 1
filename  = ppn + "_fluency.dat" // audio filename, server prepends datetime
debug     = 0
waitText  = "<b>WARTE</b>"
waitTime  = 6
speakText = "<b>SPRICH JETZT</b>"
speakTime = 60

fullscreen = {
	type: "fullscreen",
}

record = {
	type: "call-function",
	func: function(){ recordTime(1, ppn+"_fluency_test") } ,
}
welcome = {
	type: "html-button-response",
	stimulus: "<p><b>Herzlich willkommen!</b></p><p>In diesem Teil der Studie werden <b>Audioaufnahmen</b> gemacht.<br>Um dies zu ermöglichen, erlaube dieser Webseite jetzt und in Zukunft Zugriff auf dein Mikrofon.<br>Wähle dafür einfach 'Allow' und 'Remember this decision' im Pop-Up Fenster aus. Diese Einstellung ist Browser abhängig, verwende in Zukunft also am besten immer denselben Browser.</p>",
	choices: ["WEITER"],
}

halt_proper = {
	type: "html-keyboard-response",
	stimulus: "Sorry, microphone does not work. You cannot continue.",
	choices: jsPsych.NO_KEYS,
	
}

halt = {
	timeline: [halt_proper],
	conditional_function: function(){ return !recorder },
}

instruction0 = {
	type: "html-button-response",
	stimulus: "<p><b>Englisch & Deutsch Sprachkompetenz</b></p><p>Für diesen Teil solltest du ca. 10-15 Minuten einplanen.</p><p>Bitte erledige alle Aufgaben auf einmal, ohne größere Pausen zwischendurch. Diese Aufgabe kannst du <b><u>nicht</u> wiederholen, neustarten, oder nach einer Pause oder unterbrochener Internetverbindung fortsetzen</b> (wie es beim Vokabeltest möglich war): sorge also für eine stabile Internetverbindung!</p>",
	//stimulus: "<p><b>Englisch & Deutsch Sprachkompetenz</b></p><p>Für diesen Teil solltest du ca. 10-15 Minuten einplanen.</p><p>Bitte erledige alle Aufgaben auf einmal, ohne größere Pausen zwischendurch.</p>",
	choices: ["WEITER"],
}

instruction11 = {
	type: "html-button-response",
	stimulus: "<p>In der ersten Teilaufgabe hast du genau <b>1 Minute</b> Zeit, so viele <b>ENGLISCHE Wörter</b> wie möglich, die mit einem  <b>bestimmten Buchstaben</b> anfangen, zu benennen.</p><p>Eigennamen, Stadtnamen, Markennamen oder Ähnliches sind nicht erlaubt. Versuche außerdem zu vermeiden, das gleiche Wort mit neuen Endungen zu verwenden (z.B: Bett, Bettgestell, Bettdecke... ), oder konjugierte Versionen eines Verbs aufzuzählen (rennen, rannte, gerannt etc.).</p><p>Deine Antworten werden aufgenommen. Sprich also <b>laut und deutlich</b>.</p><p>Wenn du bereit bist, drücke dann auf 'Weiter'. Dir wird dann der Buchstabe angezeigt, mit dem die Wörter beginnen sollen.</p><p>Ein Countdown von 5 Sekunden beginnt dann automatisch. Wenn dieser abgelaufen ist, beginnt die Aufnahme und darfst du sprechen. Ein zweiter Countdown zählt dann die Zeit, die dir noch bleibt (1 Minute insgesamt).</p>",
	choices: ["WEITER"],
}

function countdown(id, filename, t){
	// first wait countdown then speak countdown
	if (t===undefined) 
		t = 1 // integer 
	setTimeout(function() { 
		const element = document.getElementById(id)
		if (t < waitTime) // waiting
			element.innerHTML = "<br>" + waitText + "<br>" + (waitTime - t)
		else if (t < waitTime + speakTime){  // speaking
			element.innerHTML = "<br>" + speakText + "<br>" + (waitTime + speakTime - t)
			if (t == waitTime)
				recordTime(speakTime, filename)
		} else {
			try {
				element.innerHTML = "" // element is probably invisible or gone by now
			} catch(err) {
			} finally {
				return
			}
		}
		countdown(id, filename, t+1)
	}, 1000)
}

record0 = {
	type: "call-function",
	func: function(){ 
		countdown("feedback", ppn + "_fluency_0")
	} ,
}

task0 = { 
	type: "html-keyboard-response",
	stimulus: "<div><img style='width:200px;' src='pics/englandflag.png'></img>" + "<br /><br />" + "<p><b>Englische Wörter</b> die mit <b>'F'</b> anfangen.<div id=feedback></div>",
	choices: jsPsych.NO_KEYS,
	trial_duration: (waitTime+speakTime)*1000,
}

instruction1 = {
	type: "html-button-response",
	stimulus: "<p>Als nächstes möchten wir dich bitten, so viele <b>ENGLISCHE</b> Wörter wie möglich aus einer <b>bestimmten Kategorie</b> zu benennen.</p><p>Für die Kategorie Früchte, zum Beispiel, könntest du Banane, Apfel, Mango und so weiter aufzählen. Wie vorher, sind Eigennamen, Stadtnamen, Markennamen oder ähnliches nicht erlaubt.</p><p>Insgesamt wirst du diese Aufgabe <b>zweimal</b> machen, mit <b>zwei verschiedenen Kategorien</b>, die dir jeweils oben auf dem Bildschirm angezeigt werden, sobald du auf „Weiter“ gedrückt hast.</p><p>Wie gerade eben, wird dir jeweils durch einen Countdown angezeigt, wann du sprechen darfst, und wie viel Zeit dir noch bleibt. Deine Antworten werden wieder aufgenommen.</p>",
	choices: ["WEITER"],
}

record1 = {
	type: "call-function",
	func: function(){ 
		countdown("feedback", ppn + "_fluency_1") // server prepends datetime
	} ,
}

task1 = { 
	type: "html-keyboard-response",
	stimulus: "<div><img style='width:200px;' src='pics/englandflag.png'></img>" + "<br /><br />" + "<p><b>Englische Wörter</b> aus der Kategorie <b>'Tiere die an Land leben'</b>.<div id=feedback></div>",
	choices: jsPsych.NO_KEYS,
	trial_duration: (waitTime+speakTime)*1000,
}

instruction2 = {
	type: "html-button-response",
	stimulus: "",
	choices: ["WEITER"],
}

record2 = {
	type: "call-function",
	func: function(){ 
		countdown("feedback", ppn + "_fluency_2")
	} ,
}

task2 = { 
	type: "html-keyboard-response",
	stimulus: "<div><img style='width:200px;' src='pics/englandflag.png'></img>" + "<br /><br />" + "<p><b>Englische Wörter</b> aus der Kategorie <b>'Berufe'</b>.<div id=feedback></div>",
	choices: jsPsych.NO_KEYS,
	trial_duration: (waitTime+speakTime)*1000,
}

instruction3 = {
	type: "html-button-response",
	stimulus: "<p>Du bist auf der Hälfte!</p><p>Wir wiederholen die Aufgaben nun noch auf <b>DEUTSCH</b>, (mit einem anderen Buchstaben und anderen Kategorien).</p><p>Sonst gelten die gleichen Regeln wie eben: keine Eigennamen oder dergleichen, und du bekommst genau 1 Minute Zeit, um so viele Wörter wie möglich aufzulisten.</p><p>Drücke auf 'Weiter', um den Buchstaben für die erste Aufgabe zu sehen.</p>",
	choices: ["WEITER"],
}

record3 = {
	type: "call-function",
	func: function(){ 
		countdown("feedback", ppn + "_fluency_3")
	} ,
}

task3 = { 
	type: "html-keyboard-response",
	stimulus: "<div><img style='width:200px;' src='pics/germanyflag.png'></img>" + "<br /><br />" + "<p><b>Deutsche Wörter</b> mit dem Buchstaben <b>'P'</b>.<div id=feedback></div>",
	choices: jsPsych.NO_KEYS,
	trial_duration: (waitTime+speakTime)*1000,
}

instruction4 = {
	type: "html-button-response",
	stimulus: "<p>Nun noch zwei <b>Kategorien</b>, für die du so viele <b>DEUTSCHE</b> Wörter wie möglich nennen sollst.</p>",
	choices: ["WEITER"],
}

record4 = {
	type: "call-function",
	func: function(){ 
		countdown("feedback", ppn + "_fluency_4")
	} ,
}

task4 = { 
	type: "html-keyboard-response",
	stimulus: "<div><img style='width:200px;' src='pics/germanyflag.png'></img>" + "<br /><br />" + "<p><b>Deutsche Wörter</b> aus der Kategorie <b>'Küchenutensilien'</b>.<div id=feedback></div>",
	choices: jsPsych.NO_KEYS,
	trial_duration: (waitTime+speakTime)*1000,
}

instruction5 = {
	type: "html-button-response",
	stimulus: "",
	choices: ["WEITER"],
}

record5 = {
	type: "call-function",
	func: function(){ 
		countdown("feedback", ppn + "_fluency_5")
	} ,
}

task5 = { 
	type: "html-keyboard-response",
	stimulus: "<div><img style='width:200px;' src='pics/germanyflag.png'></img>" + "<br /><br />" + "<p><b>Deutsche Wörter</b> aus der Kategorie <b>'Gemüse'</b>.<div id=feedback></div>",
	choices: jsPsych.NO_KEYS,
	trial_duration: (waitTime+speakTime)*1000,
}


thankyou = {
	type: "html-keyboard-response",
	stimulus: "<p>Du hast es geschafft!<br><br>Vielen Dank für deine Teilnahme!</p>",
}


timeline = [record, welcome, halt, instruction0, instruction11, record0, task0, instruction1, record1, task1, instruction2, record2, task2, instruction3, record3, task3, instruction4, record4, task4, instruction5, record5, task5, thankyou]
if(debug>=1)
	timeline = [instruction0, record0, task0]
	
function saveData(data){
	data['ppn'] = ppn
	data['time'] = (new Date()).toString()
	data['userAgent'] = navigator.userAgent
	data['screenWidth'] = screen.width
	data['screenHeight'] = screen.height
	data['windowWidth'] = $(window).width()
	data['windowHeight'] = $(window).height()

	var jsonData = JSON.stringify(data)
	console.log("    data: " + jsonData)
	$.ajax({
		type:'post',
		url: 'save_data.php',
		data: { 
			filename: filename, 
			data: jsonData,
		},
		success: function(output) {
			console.log("server response: " + JSON.stringify(output))
		},
		error: function(jqXHR, textStatus, errorThrown){
			console.log("ERROR")
			document.body.innerHTML = "<p>Sorry, the experiment was halted because your internet connection failed. You can reload the experiment to start again.</p><p>error: " + textStatus + "</p>";
		},
	})
}
jsPsych.init({
	timeline: timeline,
	//on_finish: saveData,
	on_data_update: function(data) { saveData(data); }, // after each trial data as argument
})
