

// Plugin to show an image and record audio while user pronounces the word
// requires some functions from "audio-recording.js"

jsPsych.plugins['picture-naming'] = (function(){

    jsPsych.pluginAPI.registerPreload('picture-naming', 'stimulus', 'image');

    var txt = {
        btnStopRecording: 'Stop Aufnahme',
        btnContinue: 'Weiter',
        titleInstruction: 'Benenne das Bild auf Spanisch.',
        statusInitializing: 'Initialisieren',
        statusRecording: 'Aufnahme läuft',
        statusStoppingRecording: 'Bitte warten',
        statusUploading: 'Speichern',
        statusDone: 'Fertig',
    }

    var maxRecordingTime = 60000; // Maximum recording length in ms
    var stopRecordingDelay = 1000; // After user presses done, keep recording for this many ms

    var plugin = {};

    plugin.info = {
        name: 'picture-naming',
        parameters: {
            imgFilename: {
                type: jsPsych.plugins.parameterType.IMAGE, // BOOL, STRING, INT, FLOAT, FUNCTION, KEYCODE, SELECT, HTML_STRING, IMAGE, AUDIO, VIDEO, OBJECT, COMPLEX
                default: undefined,
                pretty_name: 'Image filename',
                description: 'Filename of the image to be displayed (no path)'
            },
            imgDir: {
                type: jsPsych.plugins.parameterType.STRING,
                default: "",
                pretty_name: 'Image dir',
                description: 'Directory where images are stored, relative to main html file'
            },
            imgIndex: {
                type: jsPsych.plugins.parameterType.INT,
                default: 0,
                pretty_name: 'Image index',
                description: 'Number that uniquely identifies the image, used to show progress (4/100) and for saving data'
            },
            lastImgIndex: {
                type: jsPsych.plugins.parameterType.INT,
                default: 0,
                pretty_name: 'Last image index',
                description: 'Number of the last image (used to show progress: 4/100)'
            },
            ppn: {
                type: jsPsych.plugins.parameterType.INT,
                default: 1,
                pretty_name: 'Proefpersoon nummer',
                description: 'Participant number, used for saving data'
            },
        }
    }

    plugin.trial = function(display_element, trial){
        console.log("Starting trial: ", trial);

        var data = {
            ppn: trial.ppn,
            imgFilename: trial.imgFilename,
            imgIndex: trial.imgIndex,
            timeShow: null,
            timeStartRec: null,
            timeStopRec: null,
            timeStartUpload: null,
            timeStopUpload: null,
            timeContinue: null,
        };

        // function to end trial when it is time
        var endTrial = function() {
            // kill any remaining setTimeout handlers
            jsPsych.pluginAPI.clearAllTimeouts();
            // clear the display
            display_element.innerHTML = '';
            // move on to the next trial
            jsPsych.finishTrial(data);
        };
        
        var uploadAudioFile = function(blob) {
            var onProgress = function(percentage) {
                progress.attr('value', Math.round(percentage*100));
            }
            var onDone = function(serverResponse) {
                // Done uploading, allow user to continue
                status.html(txt.statusDone);
                progress.attr('value', 100);
                btnContinue.prop('disabled', false);
                data.timeStopUpload = Date.now();
            }
            var onError = function(err) {
                // Audio file could not be uploaded
                // Show alert and keep continue button disabled
                status.html("Error uploading audio: "+err);
                alert("Error uploading audio: '"+err+"'. Do you have a stable internet connection?\nPlease restart the experiment or contact the experimenter.")
            }

            status.html(txt.statusUploading);
            progress.attr('value', 0);

            data.timeStartUpload = Date.now();
            var filename = trial.ppn + "_naming_T3_" + trial.imgIndex;
            uploadAudio(blob, filename, onProgress, onDone, onError);
        }

        var handleDoneClick = function() {
            if (data.timeStopRec === null) {
                // Stop recording and then start uploading
                data.timeStopRec = Date.now();

                status.html(txt.statusStoppingRecording)
                btnDone.prop('disabled', true);
                btnContinue.prop('disabled', true);

                jsPsych.pluginAPI.setTimeout(function() {
                    recorder.stopRecording(function() {
                        var blob = recorder.getBlob();
                        uploadAudioFile(blob);
                    });
                }, stopRecordingDelay);
            }
        }

        var handleContinueClick = function() {
            if (data.timeContinue === null) {
                data.timeContinue = Date.now();

                btnDone.prop('disabled', true);
                btnContinue.prop('disabled', true);
                
                endTrial();
            }
        }

        var container = $(
            '<div id="naming">'+
                '<div id="instruction"></div>'+
                '<div id="stimulus-container">'+
                    '<img id="stimulus" src="" />'+
                '</div>'+
                '<div id="status"></div>'+
                '<progress id="progress" value="0" max="100"></progress>'+
                '<div id="controls">'+
                    '<button id="btn-done" class="jspsych-btn">'+txt.btnStopRecording+'</button>'+
                    '<button id="btn-continue" class="jspsych-btn">'+txt.btnContinue+'</button>'+
                '</div>'+
            '</div>'
        );

        var instruction = container.find('#instruction');
        var btnDone = container.find('#btn-done');
        var btnContinue = container.find('#btn-continue');
        var img = container.find('#stimulus');
        var status = container.find('#status');
        var progress = container.find('#progress');

        // Clear the display and add the container
        display_element.innerHTML = '';
        container.appendTo(display_element);

        // Set instruction and image
        instruction.html((trial.imgIndex+1) + "/" + (trial.lastImgIndex+1) + "<br>" + txt.titleInstruction);
        img.attr('src', trial.imgDir+"/"+trial.imgFilename);
        data.timeShow = Date.now();

        // Initialize buttons and status
        status.html(txt.statusInitializing);
        btnDone.prop('disabled', true);
        btnContinue.prop('disabled', true);
        btnDone.on('click', handleDoneClick);
        btnContinue.on('click', handleContinueClick);
        
        // Get recording and when found, start recording
        var recorder = null;
        getRecorder(function(rec) {
            data.timeStartRec = Date.now();
            recorder = rec;
            recorder.startRecording();
            status.html(txt.statusRecording);
            btnDone.prop('disabled', false);
            progress.attr('value', null); // Set progress to indeterminate

            // After a max time, stop recording
            jsPsych.pluginAPI.setTimeout(function() {
                if (!btnDone.prop('disabled')) {
                    btnDone.click();
                }
            }, maxRecordingTime);
        });
        
    }

    return plugin;

})();





// Define helper functions

// Note: returns array of numbers, not strings
function getCompletedImages() {
    var str = localStorage.getItem("completedImages");
    if (str == null) {
        return [];
    } else {
        var arr = str.split(",");
        arr = arr.map(function(val) { return parseInt(val); }); 
        return arr;
    }
}

function addCompletedImage(imgIndex) {
    var arr = getCompletedImages();
    arr.push(imgIndex);
    var str = arr.join(",");
    localStorage.setItem("completedImages", str);
}

// Note: imgIndex must be a number, not a string
function isImageCompleted(imgIndex) {
    var completedNrs = getCompletedImages();
    if (completedNrs.indexOf(imgIndex) === -1) {
        return false;
    } else {
        return true;
    }
}

function resetCompletedImages() {
    localStorage.removeItem("completedImages");
}

function selectImages(images, isPractice, skipCompleted) {
    // Practice trials use the first 4 images, real trials use the rest
    if (isPractice) {
        images = images.slice(0, 4);
    } else {
        images = images.slice(4);
    }

    // Remove images that were already seen if needed (stored in localStorage)
    return images.filter(function(img) {
        if (skipCompleted && isImageCompleted(img.index)) {
            return false;
        } else {
            return true;
        }
    });
}

function makeTrialsFromImages(images, lastImgIndex, imgDir, ppn) {
    var timeline = [];
    for (var i=0; i<images.length; i++) {
        var img = images[i];
        timeline.push({
            type: 'picture-naming',
            imgFilename: img.filename,
            imgIndex: img.index,
            lastImgIndex: lastImgIndex,
            imgDir: imgDir,
            ppn: ppn,
        });
    }
    return timeline;
}

function saveData(data, ppn) {
    var filename  = ppn + "_naming_T3.dat" // server adds datetime
    var jsonData = JSON.stringify(data);
	$.ajax({
		type: 'POST',
		url: 'save_data.php',
        data: { 
            filename: filename, 
            data: jsonData
        },
		success: function(output) {
            // For picture-naming tasks, store img index in local storage so it can be skipped on page reload
            if (data.trial_type == 'picture-naming') {
                addCompletedImage(data.imgIndex); 
            }
		}
	});
}




// Initialize global variables

// Get participant number from URL query string
var ppn = jsPsych.data.urlVariables()['ppn']
if (ppn === undefined) {
    ppn = 1
}

// For debugging to reset localStorage, add ?reset=1 to url:
if (jsPsych.data.urlVariables()['reset']) {
    resetCompletedImages();
	document.location.replace("naming4.html?ppn="+ppn)
}

// Check if pictureList is loaded from JS file
if (typeof pictureList === 'undefined') {
    var err = new Error("Picture list not loaded, try a page refresh or contact the experimenter");
    alert(err);
    throw err; // die if we don't have the pictures
}

// Directory where images are located (relative to naming.html)
var imgDir = "pics";




// Make instruction trials
var recordTestPrompt = {
	type: "call-function",
	func: function(){
        // Make and send a 1 second recording
        // mainly to show a prompt in browser asking for permission to record
        recordTime(1, ppn+"_naming_test");

        // Send some data about the browser
        var initialData = {
            time: (new Date()).toString(),
            userAgent: navigator.userAgent,
            screenWidth: screen.width,
            screenHeight: screen.height,
            windowWidth: $(window).width(),
            windowHeight: $(window).height(),
        };
        saveData(initialData, ppn);
    },
}

var generalInstructions = {
    type: 'instructions',
    pages: [
        '<p><b>Herzlich willkommen!</b></p>'+
        '<p>In diesem Teil der Studie werden <b>Audioaufnahmen</b> gemacht.<br>Um dies zu ermöglichen, erlaube dieser Webseite jetzt und in Zukunft Zugriff auf dein Mikrofon.<br>Wähle dafür einfach "Allow" und "Remember this decision" im Pop-Up Fenster aus. Diese Einstellung ist Browser-abhängig, verwende in Zukunft also am besten immer denselben Browser.</p>'+
        '<p>Sollte sich die Seite aufhängen, oder dein Browser abstürzen, öffne einfach <b>denselben Browser</b> wieder, oder <b>aktualisiere</b> die Seite. Bei funktionierender Internetverbindung sollte die Aufgabe dann an der Stelle weitergehen, wo die letzten Daten gespeichert wurden (die Instruktionen werden noch einmal gezeigt, egal wo du warst, wundere dich also nicht). Solltest du trotzdem Probleme haben, melde dich bei mir (Anne Mickan).</p>',

        '<p><b>Vokabeltest Spanisch</b></p>'+
        '<p>In der folgenden Aufgabe möchten wir, wie beim letzten Mal, deine Spanisch-Vokabelkenntnisse testen.</p>'+
        '<p>Der Test dauert <b>ca. 30-60 Minuten</b>. Bitte stelle sicher, dass du entsprechend Zeit hast <u>bevor</u> du anfängst. Du solltest die Aufgabe nämlich <b>auf einmal, also in einer Sitzung, erledigen</b>.</p>'+
        '<p>Deine Aufgabe wird darin bestehen, eine Reihe Bilder von Objekten und Tieren auf Spanisch zu benennen. Bitte sei ehrlich und verwende <b>kein Wörterbuch</b> und bitte keine Freunde um Hilfe. Es ist kein Problem, wenn du nicht immer die Antwort weißt.</p>',

        '<p>Insgesamt wirst du 144 Fotos auf Spanisch benennen.</p>'+
        '<p>Pro Bild hast du maximal <b>1 Minute</b> Zeit um zu antworten. Gib deine Antwort jedoch bitte <b>so schnell wie möglich</b>.</p>'+
        '<p>Nenne <b>nur das Wort</b> selber, <u>nicht</u> den dazugehörigen Artikel (also z.B. nur "gato" und <u>nicht</u> "un/el gato").</u></p>'+
        '<p>Wenn du ein Wort nicht auf Spanisch kennst, sag\' "weiß ich nicht".<br>Gib ansonsten immer deine beste Anwort, auch wenn du dir nicht ganz sicher bist.<br>Solange die Aufnahme noch läuft, darfst du dich auch korrigieren.</p>'+
        '<p>Deine Antworten werden per Mikrofon aufgenommen, <b>bitte sprich laut und deutlich</b>.</p>',

        '<p>Die <b>Aufnahme startet automatisch</b>, sobald das Bild erscheint.</p>'+
        '<p>Wenn du <b>fertig</b> bist mit sprechen, drücke den <b>"Stop Aufnahme"</b> Knopf.</p>'+
        '<p>Die Aufnahme wird daraufhin gespeichert.<br>Wenn du eine Aufnahme beendet hast, kannst du diese nicht mehr korrigieren oder verändern.<br>Das Speichern deiner Antwort kann einen Moment dauern, sollte bei stabiler Internetverbindung jedoch schnell gehen.</p>'+
        '<p>Wenn deine Antwort gespeichert wurde, wird der <b>"Weiter"</b> Knopf aktiv, der dich zum nächsten Bild bringt.<br>Solltest du eine Pause brauchen, dann warte mit dem Drücken des "Weiter" Knopfes.<br>Bitte halte eventuelle <b>Pausen so kurz wie möglich</b>!</p>'
    ],
    show_clickable_nav: true,
    button_label_next: "Weiter",
    button_label_previous: "Zurück",
}

var practiceTrialsInstruction = {
	type: "html-button-response",
    stimulus: 
        '<p>Die ersten 4 Bilder dienen als Übung.<br>Erst danach beginnt der eigentliche Test.</p>'+
        '<p>Klicke auf "Weiter" um zu beginnen.</p>',
	choices: ["Weiter"],
}

var realTrialsInstruction = {
	type: "html-button-response",
    stimulus: 
        '<p>Nun beginnt der eigentliche Test.</p>'+
        '<p>Zur Wiederholung:<ul><li>drücke "Stop Aufnahme" <i>nachdem</i> du deine Antwort gesprochen hast,</li><li>und "Weiter" wenn du für das nächste Bild bereit bist.</li></p>',
	choices: ["Weiter"],
}

var thanks = {
	type: "html-button-response",
    stimulus: '<p>Du hast es geschafft!<br><br>Vielen Dank für deine Teilnahme!</p>',
    choices: ["Exit"],
}


// Make image-naming trials
var images = pictureList.map(function(filename, index) {
    return {
        filename: filename,
        index: index,
    }
})

var lastImageIndex = images[images.length-1].index;

var practiceImages = selectImages(images, true, true);
var practiceTrials = makeTrialsFromImages(practiceImages, lastImageIndex, imgDir, ppn);

var realImages = selectImages(images, false, true);
var realTrials = makeTrialsFromImages(realImages, lastImageIndex, imgDir, ppn);


// Make timeline
var timeline = [];
if (practiceTrials.length > 0 || realTrials.length > 0) {
    timeline = timeline.concat(recordTestPrompt, generalInstructions);
}
if (practiceTrials.length > 0) {
    timeline = timeline.concat(practiceTrialsInstruction, practiceTrials);
}
if (realTrials.length > 0) {
    timeline = timeline.concat(realTrialsInstruction, realTrials);
}
timeline = timeline.concat(thanks);


// Make list of images to preload (imgDir/filename)
var allUsedImages = [].concat(practiceImages, realImages);
var preloadImagePaths = allUsedImages.map(function(img) { return imgDir+"/"+img.filename; })


/// Initialize experiment
jsPsych.init({
    timeline: timeline,
    on_data_update: function(data) { saveData(data, ppn); }, // Called after each trial with trialdata in argument
    preload_images: preloadImagePaths,
});



