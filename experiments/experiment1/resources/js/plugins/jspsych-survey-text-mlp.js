/**
 * jspsych-survey-text-mlp
 * a jspsych plugin for free response survey questions
 *
 * Josh de Leeuw
 *
 * documentation: docs.jspsych.org
 *
 */


jsPsych.plugins['survey-text-mlp'] = (function() {

  var plugin = {};

  plugin.info = {
    name: 'survey-text-mlp',
    description: '',
    parameters: {
      questions: {
        type: jsPsych.plugins.parameterType.COMPLEX,
        array: true,
        pretty_name: 'Questions',
        default: undefined,
        nested: {
          prompt: {
            type: jsPsych.plugins.parameterType.STRING,
            pretty_name: 'Prompt',
            default: undefined,
            description: 'Prompt for the subject to respod'
          },
          placeholder: {
            type: jsPsych.plugins.parameterType.STRING,
            pretty_name: 'Value',
            default: "",
            description: 'Placeholder text in the textfield.'
          },
          postscript: {
            type: jsPsych.plugins.parameterType.STRING,
            pretty_name: 'Postscript',
            default: null,
            description: 'Postscript'
          },
          fontsize: {
            type: jsPsych.plugins.parameterType.STRING,
            pretty_name: 'font size',
            default: '100%',
            description: 'font size'
          },

          rows: {
            type: jsPsych.plugins.parameterType.INT,
            pretty_name: 'Rows',
            default: 1,
            description: 'The number of rows for the response text box.'
          },
          columns: {
            type: jsPsych.plugins.parameterType.INT,
            pretty_name: 'Columns',
            default: 40,
            description: 'The number of columns for the response text box.'
          },
          required: {
            type: jsPsych.plugins.parameterType.BOOL,
            pretty_name: 'Required',
            default: false,
            description: 'Require a response'
          },
          name: {
            type: jsPsych.plugins.parameterType.STRING,
            pretty_name: 'Question Name',
            default: '',
            description: 'Controls the name of data values associated with this question'
          }
        }
      },
      option_layout: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Option layout',
        default: 'vertical',
        description: 'vertical = response appears below prompt; horizontal = response appears alongside prompt.'
      },
      preamble: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Preamble',
        default: null,
        description: 'HTML formatted string to display at the top of the page above all the questions.'
      },
      button_label: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Button label',
        default:  'Continue',
        description: 'The text that appears on the button to finish the trial.'
    },
      numeric_only: {
        type: jsPsych.plugins.parameterType.BOOL,
        pretty_name: 'Numeric-only input',
        default:  false,
        description: 'Set to true to limit input to numbers only'
    },
      max_number: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Max input number',
        default:  9999,
        description: 'Max input number (min is assumed to be 0)'
      }
    }
  }

  plugin.trial = function(display_element, trial) {

    for (var i = 0; i < trial.questions.length; i++) {
      if (typeof trial.questions[i].rows == 'undefined') {
        trial.questions[i].rows = 1;
      }
    }
    for (var i = 0; i < trial.questions.length; i++) {
      if (typeof trial.questions[i].columns == 'undefined') {
        trial.questions[i].columns = 40;
      }
    }
    for (var i = 0; i < trial.questions.length; i++) {
      if (typeof trial.questions[i].value == 'undefined') {
        trial.questions[i].value = "";
      }
    }

    var html = '';
    // show preamble text
    if(trial.preamble !== null){
      html += '<div id="jspsych-survey-text-preamble" class="jspsych-survey-text-preamble">'+trial.preamble+'</div>';
    }
    // start form
    html += '<form id="jspsych-survey-text-form">'

    // generate question order
    var question_order = [];
    for(var i=0; i<trial.questions.length; i++){
      question_order.push(i);
    }
    if(trial.randomize_question_order){
      question_order = jsPsych.randomization.shuffle(question_order);
    }

    var typeStr = 'type="text"'
    if (trial.numeric_only == true) {typeStr = 'type="number" min="0" max="'+trial.max_number+'"'};

    // add questions
    for (var i = 0; i < trial.questions.length; i++) {
      var question = trial.questions[question_order[i]];
      var question_index = question_order[i];
      html += '<div id="jspsych-survey-text-'+question_index+'" class="jspsych-survey-text-question" style="margin: 2em 0em;">';
      html += '<p class="jspsych-survey-text" style="white-space:pre;font-size:'+question.fontsize+';">' + question.prompt;
      if (trial.option_layout == 'horizontal') {
          var fontsize2 = '100%';
          html += '   '
      } else {
          var fontsize2 = question.fontsize;
          html += '</p>'
      };
      var autofocus = i == 0 ? "autofocus" : "";
      var req = question.required ? "required" : "";
      if(question.rows == 1){
          html += '<input '+typeStr+' id="input-'+question_index+'"  name="#jspsych-survey-text-response-' + question_index + '" data-name="'+question.name+'" size="'+question.columns+'" '+autofocus+' '+req+' placeholder="'+question.placeholder+'" style="font-size:'+fontsize2+';"></input>';
      } else {
        html += '<textarea id="input-'+question_index+'" name="#jspsych-survey-text-response-' + question_index + '" data-name="'+question.name+'" cols="' + question.columns + '" rows="' + question.rows + '" '+autofocus+' '+req+' placeholder="'+question.placeholder+'" style="font-size:100%;"></textarea>';
      }
      if(question.postscript !== null){html += '   ' + question.postscript}
      html += '</div>';
    }

    // add submit button
    html += '<input type="submit" id="jspsych-survey-text-next" class="jspsych-btn jspsych-survey-text" value="'+trial.button_label+'"></input>';

    html += '</form>'
    display_element.innerHTML = html;

    // backup in case autofocus doesn't work
    display_element.querySelector('#input-'+question_order[0]).focus();

    display_element.querySelector('#jspsych-survey-text-form').addEventListener('submit', function(e) {
      e.preventDefault();
      // measure response time
      var endTime = performance.now();
      var response_time = endTime - startTime;

      // create object to hold responses
      var question_data = {};

      for(var index=0; index < trial.questions.length; index++){
        var id = "Q" + index;
        var q_element = document.querySelector('#jspsych-survey-text-'+index).querySelector('textarea, input');
        var val = q_element.value;
        var name = q_element.attributes['data-name'].value;
        if(name == ''){
          name = id;
        }
        var obje = {};
        obje[name] = val;
        Object.assign(question_data, obje);
      }
      // save data
      var trialdata = {
        "rt": response_time,
        "responses": JSON.stringify(question_data)
      };

      display_element.innerHTML = '';

      // next trial
      jsPsych.finishTrial(trialdata);
    });

    var startTime = performance.now();
  };

  return plugin;
})();
