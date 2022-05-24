/**
 *
 * jspsych-visual-search-flex
 * Josh de Leeuw
 *
 * display a set of objects, with or without a target, equidistant from fixation
 * subject responds to whether or not the target is present
 *
 * based on code written for psychtoolbox by Ben Motz
 *
 * documentation: docs.jspsych.org
 *
 **/

jsPsych.plugins["visual-search-flex"] = (function() {

  var plugin = {};

  jsPsych.pluginAPI.registerPreload('visual-search-flex', 'target', 'image');
  jsPsych.pluginAPI.registerPreload('visual-search-flex', 'search_stimuli', 'image');
  jsPsych.pluginAPI.registerPreload('visual-search-flex', 'line_stimuli', 'image');
  jsPsych.pluginAPI.registerPreload('visual-search-flex', 'fixation_image', 'image');

  plugin.info = {
    name: 'visual-search-flex',
    description: '',
    parameters: {
      search_stimuli: {
        type: jsPsych.plugins.parameterType.IMAGE,
        pretty_name: 'search_stimuli',
        default: undefined,
        description: 'Path to image file that is the search_stimuli/distractor.'
      },
      line_stimuli: {
        type: jsPsych.plugins.parameterType.IMAGE,
        pretty_name: 'lines_in_stimuli',
        default: undefined,
        description: 'Path to image file that is the set of lines to appear in the search stimuli.'
      },
      fixation_image: {
        type: jsPsych.plugins.parameterType.IMAGE,
        pretty_name: 'Fixation image',
        default: undefined,
        description: 'Path to image file that is a fixation target.'
      },
      set_size: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Set size',
        default: 6,
        description: 'How many items should be displayed?'
      },
      search_stim_size: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Target size',
        array: true,
        default: [50, 50],
        description: 'Two element array indicating the height and width of the search array element images.'
      },
      fixation_size: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Fixation size',
        array: true,
        default: [16, 16],
        description: 'Two element array indicating the height and width of the fixation image.'
      },
      circle_diameter: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Circle diameter',
        default: 250,
        description: 'The diameter of the search array circle in pixels.'
      },
	  target_type: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Target type',
        default: 0,
        description: 'Target type (0 = horiz, 1 = vert)'
      },
      target_horizontal_key: {
        type: jsPsych.plugins.parameterType.KEYCODE,
        pretty_name: 'Target horizonal key',
        default: 'c',
        description: 'The key to press if the line in the target is horizontal.'
      },
      target_vertical_key: {
        type: jsPsych.plugins.parameterType.KEYCODE,
        pretty_name: 'Target absent key',
        default: 'm',
        description: 'The key to press if the line in the target is vertical.'
      },
      trial_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Trial duration',
        default: null,
        description: 'The maximum duration to wait for a response.'
      },
      fixation_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Fixation duration',
        default: 1000,
        description: 'How long to show the fixation image for before the search array (in milliseconds).'
    },
      check_focus_interval: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Focus check interval',
        default: 0,
        description: 'If set to a positive value, repeatedly returns focus to the current div after this interval (in milliseconds).'
      }
    }
  }

  plugin.trial = function(display_element, trial) {

      document.getElementById("jspsychTargetMLP").focus();
      if (trial.check_focus_interval > 0) {
          var focusChecker = setInterval(function() {moveFocusFn()}, trial.check_focus_interval);
          function moveFocusFn () {
              document.getElementById("jspsychTargetMLP").focus();
          }
      };

    // circle params
    var diam = trial.circle_diameter; // pixels
    var radi = diam / 2;
    var paper_size = diam + trial.search_stim_size[0];

    // stimuli width, height
    var stimh = trial.search_stim_size[0];
    var stimw = trial.search_stim_size[1];
    var hstimh = stimh / 2;
    var hstimw = stimw / 2;

    // fixation location
    var fix_loc = [Math.floor(paper_size / 2 - trial.fixation_size[0] / 2), Math.floor(paper_size / 2 - trial.fixation_size[1] / 2)];

    // possible stimulus locations on the circle
    var display_locs = [];
    var possible_display_locs = trial.set_size;
    var random_offset = 0;
    for (var i = 0; i < possible_display_locs; i++) {
      display_locs.push([
          Math.floor(paper_size / 2 + (sind(random_offset + (i * (360 / possible_display_locs))) * radi) - hstimw),
          Math.floor(paper_size / 2 - (cosd(random_offset + (i * (360 / possible_display_locs))) * radi) - hstimh)
      ]);
    }

    // get target to draw on
    display_element.innerHTML += '<div id="jspsych-visual-search-flex-container" style="position: relative; width:' + paper_size + 'px; height:' + paper_size + 'px"></div>';
    var paper = display_element.querySelector("#jspsych-visual-search-flex-container");

    // check distractors - array?
    if(!Array.isArray(trial.search_stimuli)){
      fa = [];
      for(var i=0; i<trial.set_size; i++){
        fa.push(trial.search_stimuli);
      }
      trial.search_stimuli = fa;
    }

    // check distractors - array?
    if(!Array.isArray(trial.line_stimuli)){
      fa = [];
      for(var i=0; i<trial.set_size; i++){
        fa.push(trial.line_stimuli);
      }
      trial.line_stimuli = fa;
    }

    show_fixation();

    function show_fixation() {
      // show fixation
      //var fixation = paper.image(trial.fixation_image, fix_loc[0], fix_loc[1], trial.fixation_size[0], trial.fixation_size[1]);
      paper.innerHTML += "<img src='"+trial.fixation_image+"' style='position: absolute; top:"+fix_loc[0]+"px; left:"+fix_loc[1]+"px; width:"+trial.fixation_size[0]+"px; height:"+trial.fixation_size[1]+"px;'></img>";

      // wait
      jsPsych.pluginAPI.setTimeout(function() {
        // after wait is over
        show_search_array();
      }, trial.fixation_duration);
    }

    function show_search_array() {

      var search_array_images = [];

      var to_present = trial.search_stimuli;
      var line_to_present = trial.line_stimuli;

      var tempHTML = '';

      for (var i = 0; i < display_locs.length; i++) {
          tempHTML += "<img src='"+to_present[i]+"' style='position: absolute; left:"+display_locs[i][0]+"px; top:"+display_locs[i][1]+"px; width:"+trial.search_stim_size[0]+"px; height:"+trial.search_stim_size[1]+"px;'></img>";
          tempHTML += "<img src='"+line_to_present[i]+"' style='position: absolute; left:"+display_locs[i][0]+"px; top:"+display_locs[i][1]+"px; width:"+trial.search_stim_size[0]+"px; height:"+trial.search_stim_size[1]+"px;'></img>";
          // paper.innerHTML += "<img src='"+to_present[i]+"' style='position: absolute; left:"+display_locs[i][0]+"px; top:"+display_locs[i][1]+"px; width:"+trial.search_stim_size[0]+"px; height:"+trial.search_stim_size[1]+"px;'></img>";
          // paper.innerHTML += "<img src='"+line_to_present[i]+"' style='position: absolute; left:"+display_locs[i][0]+"px; top:"+display_locs[i][1]+"px; width:"+trial.search_stim_size[0]+"px; height:"+trial.search_stim_size[1]+"px;'></img>";
      }

      paper.innerHTML += tempHTML;

      var trial_over = false;

      var after_response = function(info) {

        trial_over = true;

        var correct = 0;

        if ((jsPsych.pluginAPI.compareKeys(info.key,trial.target_horizontal_key) && trial.target_type == 0) ||
            (jsPsych.pluginAPI.compareKeys(info.key,trial.target_vertical_key) && trial.target_type == 1)) {
          correct = 1;
        }

        clear_display();

        end_trial(info.rt, correct, info.key);

      }

      var valid_keys = [trial.target_horizontal_key, trial.target_vertical_key];

      key_listener = jsPsych.pluginAPI.getKeyboardResponse({
        callback_function: after_response,
        valid_responses: valid_keys,
        rt_method: 'performance',
        persist: false,
        allow_held_key: false
      });

      if (trial.trial_duration !== null) {

        jsPsych.pluginAPI.setTimeout(function() {

          if (!trial_over) {

            jsPsych.pluginAPI.cancelKeyboardResponse(key_listener);

            trial_over = true;

            var rt = null;
            var correct = 99;
            var key_press = null;

            clear_display();

            end_trial(rt, correct, key_press);
          }
        }, trial.trial_duration);

      }

      function clear_display() {
        display_element.innerHTML = '';
      }
    }


    function end_trial(rt, correct, key_press) {
        if (trial.check_focus_interval > 0) {clearInterval(focusChecker)};

      // data saving
      var trial_data = {
        correct: correct,
        rt: rt,
        key_press: key_press
        // locations: JSON.stringify(display_locs),
        // target_type: trial.target_type,
        // set_size: trial.set_size
      };


      // go to next trial
      jsPsych.finishTrial(trial_data);
    }
  };

  // helper function for determining stimulus locations

  function cosd(num) {
    return Math.cos(num / 180 * Math.PI);
  }

  function sind(num) {
    return Math.sin(num / 180 * Math.PI);
  }

  return plugin;
})();
