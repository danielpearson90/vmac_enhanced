/**
* jspsych-demographic-response
* Josh de Leeuw
*
* plugin for displaying a stimulus and getting a keyboard response
*
* documentation: docs.jspsych.org
*
**/

jsPsych.plugins["demographic-response"] = (function() {

    var plugin = {};

    plugin.info = {
        name: 'demographic-response',
        description: '',
        parameters: {
            stimulus: {
                type: jsPsych.plugins.parameterType.HTML_STRING,
                pretty_name: 'Stimulus',
                default: undefined,
                description: 'The HTML string to be displayed'
            },
            choices: {
                type: jsPsych.plugins.parameterType.KEYCODE,
                pretty_name: 'Choices',
                default: [],
                array: true,
                description: 'The labels for the buttons.'
            },
            button_html: {
                type: jsPsych.plugins.parameterType.STRING,
                pretty_name: 'Button html',
                default: '<button class="jspsych-btn">%choice%</button>',
                array: true,
                description: 'The html of the button. Can create own style.'
            },
            prompt: {
                type: jsPsych.plugins.parameterType.STRING,
                pretty_name: 'Prompt',
                default: '',
                description: 'Any content here will be displayed under the button.'
            },
            stimulus_duration: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Stimulus duration',
                default: -1,
                description: 'How long to hide the stimulus.'
            },
            trial_duration: {
                type: jsPsych.plugins.parameterType.INT,
                pretty_name: 'Trial duration',
                default: -1,
                description: 'How long to show the trial.'
            },
            margin_vertical: {
                type: jsPsych.plugins.parameterType.STRING,
                pretty_name: 'Margin vertical',
                default: '0px',
                description: 'The vertical margin of the button.'
            },
            margin_horizontal: {
                type: jsPsych.plugins.parameterType.STRING,
                pretty_name: 'Margin horizontal',
                default: '8px',
                description: 'The horizontal margin of the button.'
            },
            response_ends_trial: {
                type: jsPsych.plugins.parameterType.BOOL,
                pretty_name: 'Response ends trial',
                default: true,
                description: 'If true, then trial will end when user responds.'
            },
        }
    }

    plugin.trial = function(display_element, trial) {

        // display stimulus
        display_element.innerHTML = '<div id="jspsych-demographic-response-stimulus">'+trial.stimulus+'</div>';

        //display buttons
        var buttons = [];
        if (Array.isArray(trial.button_html)) {
            if (trial.button_html.length == trial.choices.length) {
                buttons = trial.button_html;
            } else {
                console.error('Error in demographic-response plugin. The length of the button_html array does not equal the length of the choices array');
            }
        } else {
            for (var i = 0; i < trial.choices.length; i++) {
                buttons.push(trial.button_html);
            }
        }
        display_element.innerHTML += '<div id="jspsych-demographic-response-btngroup"></div>';
        for (var i = 0; i < trial.choices.length; i++) {
            var str = buttons[i].replace(/%choice%/g, trial.choices[i]);
            display_element.querySelector('#jspsych-demographic-response-btngroup').insertAdjacentHTML('beforeend',
            '<div class="jspsych-demographic-response-button" style="display: inline-block; margin:'+trial.margin_vertical+' '+trial.margin_horizontal+'" id="jspsych-demographic-response-button-' + i +'" data-choice="'+i+'">'+str+'</div>');
            display_element.querySelector('#jspsych-demographic-response-button-' + i).addEventListener('click', function(e){
                var choice = e.currentTarget.getAttribute('data-choice'); // don't use dataset for jsdom compatibility
                after_response(choice);
            });
        }

        //show prompt if there is one
        if (trial.prompt !== "") {
            display_element.insertAdjacentHTML('beforeend', trial.prompt);
        }

        // store response
        var response = {
            rt: -1,
            button: -1
        };

        // start time
        var start_time = 0;

        // function to handle responses by the subject
        function after_response(choice) {

            // measure rt
            var end_time = Date.now();
            var rt = end_time - start_time;
            response.button = choice;
            response.rt = rt;

            // after a valid response, the stimulus will have the CSS class 'responded'
            // which can be used to provide visual feedback that a response was recorded
            display_element.querySelector('#jspsych-demographic-response-stimulus').className += ' responded';

            // disable all the buttons after a response
            var btns = document.querySelectorAll('.jspsych-demographic-response-button button');
            for(var i=0; i<btns.length; i++){
                //btns[i].removeEventListener('click');
                btns[i].setAttribute('disabled', 'disabled');
            }

            if (trial.response_ends_trial) {
                end_trial();
            }
        };

        // function to end trial when it is time
        function end_trial() {

            p_age = document.getElementById("age").value;
            p_language = document.getElementById("language").value;
            // p_country = document.getElementById("country").value;

            gender_radios = document.getElementsByName("gender");

            var ii;
            for (ii = 0; ii < gender_radios.length; ii++) {
                if (gender_radios[ii].checked) {p_gender = gender_radios[ii].value};
            };


            // kill any remaining setTimeout handlers
            jsPsych.pluginAPI.clearAllTimeouts();

            // gather the data to store for the trial
            var trial_data = {
                "rt": response.rt,
                "stimulus": trial.stimulus,
                "button_pressed": response.button
            };

            // clear the display
            display_element.innerHTML = '';

            // move on to the next trial
            jsPsych.finishTrial(trial_data);
        };

        // start timing
        start_time = Date.now();

        // hide image if timing is set
        if (trial.stimulus_duration > 0) {
            jsPsych.pluginAPI.setTimeout(function() {
                display_element.querySelector('#jspsych-demographic-response-stimulus').style.visibility = 'hidden';
            }, trial.stimulus_duration);
        }

        // end trial if time limit is set
        if (trial.trial_duration > 0) {
            jsPsych.pluginAPI.setTimeout(function() {
                end_trial();
            }, trial.trial_duration);
        }

    };

    return plugin;
})();
