/* ************************************ */
/* Define helper functions */
/* ************************************ */
function getDisplayElement() {
  $('<div class = display_stage_background></div>').appendTo('body')
  return $('<div class = display_stage></div>').appendTo('body')
}

function fillArray(value, len) {
  if (len === 0) return [];
  var a = [value];
  while (a.length * 2 <= len) a = a.concat(a);
  if (a.length < len) a = a.concat(a.slice(0, len - a.length));
  return a;
}


/* ************************************ */
/* Define experimental variables */
/* ************************************ */
// Get participant number from URL query string
var ppn = jsPsych.data.urlVariables()['ppn']
if (ppn === undefined) {
    ppn = 1
}
var run_attention_checks = true
var attention_check_thresh = 0.65
var sumInstructTime = 0 //ms
var instructTimeThresh = 0 ///in seconds

var encod_stimuli = [{
  stimulus: '<div><img src = "pics/Encod1.png"</img></div>',
  key_answer:[13]
}, {
  stimulus: '<div><img src = "pics/Encod2.png"</img></div>',
  key_answer:[13]
},{
  stimulus: '<div><img src = "pics/Encod3.png"</img></div>',
  key_answer:[13]
}, {
  stimulus: '<div><img src = "pics/Encod4.png"</img></div>',
  key_answer:[13]
},{
  stimulus: '<div><img src = "pics/Encod5.png"</img></div>',
  key_answer:[13]
}, {
  stimulus: '<div><img src = "pics/Encod6.png"</img></div>',
  key_answer:[13]
},{
  stimulus: '<div><img src = "pics/Encod7.png"</img></div>',
  key_answer:[13]
}, {
  stimulus: '<div><img src = "pics/Encod8.png"</img></div>',
  key_answer:[13]
},{
  stimulus: '<div><img src = "pics/Encod9.png"</img></div>',
  key_answer:[13]
}, {
  stimulus: '<div><img src = "pics/Encod10.png"</img></div>',
  key_answer:[13]
},{
  stimulus: '<div><img src = "pics/Encod11.png"</img></div>',
  key_answer:[13]
}, {
  stimulus: '<div><img src = "pics/Encod12.png"</img></div>',
  key_answer:[13]
},{
  stimulus: '<div><img src = "pics/Encod13.png"</img></div>',
  key_answer:[13]
}, {
  stimulus: '<div><img src = "pics/Encod14.png"</img></div>',
  key_answer:[13]
},{
  stimulus: '<div><img src = "pics/Encod15.png"</img></div>',
  key_answer:[13]
}, {
  stimulus: '<div><img src = "pics/Encod16.png"</img></div>',
  key_answer:[13]
},{
  stimulus: '<div><img src = "pics/Encod17.png"</img></div>',
  key_answer:[13]
}, {
  stimulus: '<div><img src = "pics/Encod18.png"</img></div>',
  key_answer:[13]
},{
  stimulus: '<div><img src = "pics/Encod19.png"</img></div>',
  key_answer:[13]
}, {
  stimulus: '<div><img src = "pics/Encod20.png"</img></div>',
  key_answer:[13]
},{
  stimulus: '<div><img src = "pics/Encod21.png"</img></div>',
  key_answer:[13]
}, {
  stimulus: '<div><img src = "pics/Encod22.png"</img></div>',
  key_answer:[13]
},{
  stimulus: '<div><img src = "pics/Encod23.png"</img></div>',
  key_answer:[13]
}, {
  stimulus: '<div><img src = "pics/Encod24.png"</img></div>',
  key_answer:[13]
},{
  stimulus: '<div><img src = "pics/Encod25.png"</img></div>',
  key_answer:[13]
}, {
  stimulus: '<div><img src = "pics/Encod26.png"</img></div>',
  key_answer:[13]
},{
  stimulus: '<div><img src = "pics/Encod27.png"</img></div>',
  key_answer:[13]
}, {
  stimulus: '<div><img src = "pics/Encod28.png"</img></div>',
  key_answer:[13]
},{
  stimulus: '<div><img src = "pics/Encod29.png"</img></div>',
  key_answer:[13]
}, {
  stimulus: '<div><img src = "pics/Encod30.png"</img></div>',
  key_answer:[13]
}]

/*var wait_stim = [{
  stimulus: '<div><img src = "images/wait.jpg"</img></div>',
  key_answer:[20]
},]*/

var path = 'pics/'
var prefix = '<div><img src = "'
var postfix = '"</img></div>'
var top_img = ['Test1.png', 'Test2.png', 'Test3.png', 'Test4.png', 'Test5.png', 'Test6.png',
  'Test7.png', 'Test8.png', 'Test9.png', 'Test10.png', 'Test11.png', 'Test12.png', 'Test13.png',
  'Test14.png', 'Test15.png', 'Test16.png', 'Test17.png', 'Test18.png', 'Test19.png','Test20.png',
  'Test21.png','Test22.png','Test23.png','Test24.png','Test25.png','Test26.png','Test27.png',
  'Test28.png','Test29.png','Test30.png'
]
var all_pages = []

for (var i = 0; i < top_img.length; i++) {
  var page = []
  page.push(prefix + path + top_img[i] + postfix)
  all_pages.push(page)
}

var opts = ["A", "B", "C", "D"]
var all_options = fillArray([opts], 30)

var scale_q1 = {
  "A": 0,
  "B": 0,
  "C": 1,
  "D": 0
}
var scale_q2 = {
  "A": 0,
  "B": 0,
  "C": 0,
  "D": 1
}
var scale_q3 = {
  "A": 0,
  "B": 0,
  "C": 0,
  "D": 1
}
var scale_q4 = {
  "A": 0,
  "B": 0,
  "C": 1,
  "D": 0
}
var scale_q5 = {
  "A": 1,
  "B": 0,
  "C": 0,
  "D": 0
}
var scale_q6 = {
  "A": 0,
  "B": 1,
  "C": 0,
  "D": 0
}
var scale_q7 = {
  "A": 0,
  "B": 1,
  "C": 0,
  "D": 0
}
var scale_q8 = {
  "A": 0,
  "B": 0,
  "C": 0,
  "D": 1
}
var scale_q9 = {
  "A": 0,
  "B": 1,
  "C": 0,
  "D": 0
}
var scale_q10 = {
  "A": 1,
  "B": 0,
  "C": 0,
  "D": 0
}
var scale_q11 = {
  "A": 0,
  "B": 0,
  "C": 0,
  "D": 1
}
var scale_q12 = {
  "A": 0,
  "B": 0,
  "C": 1,
  "D": 0
}
var scale_q13 = {
  "A": 0,
  "B": 0,
  "C": 1,
  "D": 0
}
var scale_q14 = {
  "A": 0,
  "B": 0,
  "C": 0,
  "D": 1
}
var scale_q15 = {
  "A": 0,
  "B": 1,
  "C": 0,
  "D": 0
}
var scale_q16 = {
  "A": 1,
  "B": 0,
  "C": 0,
  "D": 0
}
var scale_q17 = {
  "A": 0,
  "B": 0,
  "C": 1,
  "D": 0
}
var scale_q18 = {
  "A": 0,
  "B": 0,
  "C": 1,
  "D": 0
}
var scale_q19 = {
  "A": 0,
  "B": 1,
  "C": 0,
  "D": 0
}
var scale_q20 = {
  "A": 0,
  "B": 0,
  "C": 1,
  "D": 0
}
var scale_q21 = {
  "A": 0,
  "B": 0,
  "C": 0,
  "D": 1
}
var scale_q22 = {
  "A": 0,
  "B": 0,
  "C": 0,
  "D": 1
}
var scale_q23 = {
  "A": 0,
  "B": 1,
  "C": 0,
  "D": 0
}
var scale_q24 = {
  "A": 1,
  "B": 0,
  "C": 0,
  "D": 0
}
var scale_q25 = {
  "A": 0,
  "B": 0,
  "C": 1,
  "D": 0
}
var scale_q26 = {
  "A": 0,
  "B": 1,
  "C": 0,
  "D": 0
}
var scale_q27 = {
  "A": 0,
  "B": 1,
  "C": 0,
  "D": 0
}
var scale_q28 = {
  "A": 0,
  "B": 0,
  "C": 1,
  "D": 0
}
var scale_q29 = {
  "A": 1,
  "B": 0,
  "C": 0,
  "D": 0
}
var scale_q30 = {
  "A": 0,
  "B": 0,
  "C": 0,
  "D": 1
}

var score_scale = [
  [scale_q1],
  [scale_q2],
  [scale_q3],
  [scale_q4],
  [scale_q5],
  [scale_q6],
  [scale_q7],
  [scale_q8],
  [scale_q9],
  [scale_q10],
  [scale_q11],
  [scale_q12],
  [scale_q13],
  [scale_q14],
  [scale_q15],
  [scale_q16],
  [scale_q17],
  [scale_q18],
  [scale_q19],
  [scale_q20],
  [scale_q21],
  [scale_q22],
  [scale_q23],
  [scale_q24],
  [scale_q25],
  [scale_q26],
  [scale_q27],
  [scale_q28],
  [scale_q29],
  [scale_q30]
]

/* ************************************ */
/* Set up jsPsych blocks */
/* ************************************ */

/* define static blocks */
var instructions_block1 = {
  type: 'poldrack-instructions',
  pages: [
    '<div class = centerbox><p class = block-text><center><b>Gedächtnistest</b></center></br></br>In der folgenden Aufgabe wirst du zunächst 30 Bilder von Türen und Toren sehen. </br></br>Jedes Bild wird für 1 Sekunde angezeigt.  </br></br>Schaue dir die Bilder aufmerksam an, und <b>versuche, die Bilder zu verinnerlichen</b>.  </br> </br>Du wirst anschließend darauf getestest, wie gut du die gezeigten Türen von anderen Türen unterscheiden kannst.</div>',
  ],
  allow_keys: false,
  show_clickable_nav: true,
  timing_post_trial: 1000,
  data: {
    exp_id: "ravens"
  }
};

var instruction_node1 = {
  timeline: [instructions_block1],
  /* This function defines stopping criteria */
  loop_function: function(data) {
    for (i = 0; i < data.length; i++) {
      if ((data[i].trial_type == 'poldrack-instructions') && (data[i].rt != -1)) {
        rt = data[i].rt
        sumInstructTime = sumInstructTime + rt
      }
    }
    if (sumInstructTime <= instructTimeThresh * 1000) {
      feedback_instruct_text =
        'Read through instructions too quickly.  Please take your time and make sure you understand the instructions.  Press <strong>enter</strong> to continue.'
      return true
    } else if (sumInstructTime > instructTimeThresh * 1000) {
      feedback_instruct_text =
        'Klicke <strong>Enter</strong> um fortzufahren.'
      return false
    }
  }
}
var encoding_block = {
  type: 'poldrack-single-stim',
  timeline: encod_stimuli,
  is_html: true,
  choices: [13],
  timing_stim: 1000,
  timing_response: 1000,
  timing_post_trial: 200,
  };
  
/*var wait_block = {
	type: "call-function",
	func: function() {
		countdown("feedback")
		},
} */

/*&var wait_block = {
  type: 'poldrack-single-stim',
  timeline: wait_stim,
  is_html: true,
  choices: [20],
  timing_stim: 2000,
  timing_response: 2000,
  timing_post_trial: 100,
  };*/

var instructions_block2 = {
  type: 'poldrack-instructions',
  pages: [
    '<div class = centerbox><p class = block-text><center><b>Test</b></center></br></br>Du wirst nun jeweils 4 Bilder von Türen auf dem Bildschirm sehen. </br></br>Jeweils eine der Türen hast du in vorigen Teil gesehen. </br></br>Deine Aufgabe ist es, diese Tür zu finden und auszuwählen. Klicke auf A, B, C, oder D um deine Wahl zu treffen. </div>',
  ],
  allow_keys: false,
  show_clickable_nav: true,
  timing_post_trial: 1000,
  data: {
    exp_id: "ravens"
  }
};

var instruction_node2 = {
  timeline: [instructions_block2],
  /* This function defines stopping criteria */
  loop_function: function(data) {
    for (i = 0; i < data.length; i++) {
      if ((data[i].trial_type == 'poldrack-instructions') && (data[i].rt != -1)) {
        rt = data[i].rt
        sumInstructTime = sumInstructTime + rt
      }
    }
    if (sumInstructTime <= instructTimeThresh * 1000) {
      feedback_instruct_text =
        'Read through instructions too quickly.  Please take your time and make sure you understand the instructions.  Press <strong>enter</strong> to continue.'
      return true
    } else if (sumInstructTime > instructTimeThresh * 1000) {
      feedback_instruct_text =
        'Klicke <strong>ENTER</strong> um fortzufahren.'
      return false
    }
  }
}

var survey_block = {
  type: "poldrack-survey-multi-choice",
  exp_id: "ravens",
  horizontal: true,
  preamble: '',
  pages: all_pages,
  options: all_options,
  scale: score_scale,
  show_clickable_nav: true,
  allow_backward: false,
  required: fillArray([true], 30),
};

var end_block = {
  type: 'text',
  text: '<div class = centerbox><p class = center-block-text>Fertig, vielen Dank fürs Mitmachen!</p><p class = center-block-text>Klicke <strong>Enter</strong> um den Test abzuschließen.</p></div>',
  cont_key: [13],
  data: {
    exp_id: "ravens"
  }
};


//Set up experiment
var ravens_experiment = []
ravens_experiment.push(instruction_node1);
ravens_experiment.push(encoding_block);
ravens_experiment.push(instruction_node2);
ravens_experiment.push(survey_block);
ravens_experiment.push(end_block);
