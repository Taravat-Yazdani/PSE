function make_slides(f) {
  var slides = {};

  slides.i0 = slide({
    name: "i0",
    start: function () {
      exp.startT = Date.now();
    },
  });

  slides.consent = slide({
    name: "consent",

    button: function () {
      exp.go();
    },
  });

  slides.instructions = slide({
    name: "instructions",
    button: function () {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    },
  });
  // =====================================================
  // COMPREHENSION CHECK 1
  // =====================================================
  slides.comprehension1 = slide({
    name: "comprehension1",

    start: function () {
      // Record when the comprehension-check slide appeared.
      this.start_time = Date.now();

      // No answer has been selected yet.
      this.selected_answer = null;
      this.is_correct = null;

      // Restore the slide to its original condition.
      $("#comprehension1-correct").removeClass("feedback-visible");
      $("#comprehension1-incorrect").removeClass("feedback-visible");
      $("#comprehension1-next").removeClass("next-visible");

      // Show the reminder that the participant must answer first.
      $("#comprehension1-prompt").removeClass("prompt-hidden");

      // Re-enable both answer buttons.
      $("#comprehension1-answer-a").prop("disabled", false);
      $("#comprehension1-answer-b").prop("disabled", false);

      // Remove any answer-result classes left from an earlier display.
      $("#comprehension1-answer-a").removeClass(
        "selected-correct unselected-answer",
      );

      $("#comprehension1-answer-b").removeClass(
        "selected-incorrect unselected-answer",
      );
    },

    select_answer: function (answer) {
      // Do not allow the participant to change the answer afterward.
      if (this.selected_answer !== null) {
        return;
      }

      this.selected_answer = answer;
      this.is_correct = answer === "a";

      // Hide the initial reminder.
      $("#comprehension1-prompt").addClass("prompt-hidden");

      // Disable both answer buttons.
      $("#comprehension1-answer-a").prop("disabled", true);
      $("#comprehension1-answer-b").prop("disabled", true);

      if (answer === "a") {
        // Mark answer A as the selected correct answer.
        $("#comprehension1-answer-a").addClass("selected-correct");
        $("#comprehension1-answer-b").addClass("unselected-answer");

        // Show correct feedback.
        $("#comprehension1-correct").addClass("feedback-visible");
        $("#comprehension1-incorrect").removeClass("feedback-visible");
      } else {
        // Mark answer B as the selected incorrect answer.
        $("#comprehension1-answer-b").addClass("selected-incorrect");
        $("#comprehension1-answer-a").addClass("unselected-answer");

        // Show incorrect feedback.
        $("#comprehension1-incorrect").addClass("feedback-visible");
        $("#comprehension1-correct").removeClass("feedback-visible");
      }

      // Show the Next Question button only after feedback appears.
      $("#comprehension1-next").addClass("next-visible");
    },

    button: function () {
      // Safety check: do not continue without an answer.
      if (this.selected_answer === null) {
        $("#comprehension1-prompt").removeClass("prompt-hidden");
        return;
      }

      // Save the response.
      exp.comprehension_check_1 = {
        answer: this.selected_answer,
        correct: this.is_correct,
        rt: Date.now() - this.start_time,
      };

      // Continue to the next slide.
      exp.go();
    },
  });

  slides.instructions1 = slide({
    name: "instructions1",
    start: function () {
      $(".bar").css("width", (100 * exp.phase) / exp.nQs + "%");
      var inst1 = "";
      //    	console.log(block_order);
      if (exp.stims_block1[0].block == "ai") {
        inst1 =
          inst1 +
          "First you'll answer questions about what the people at the party are asking about.";
      } else {
        inst1 =
          inst1 +
          "First you'll answer questions about what the people at the party are certain about.";
      }
      $("#inst1").html(inst1);
    },
    button: function () {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    },
  });

  slides.block1 = slide({
    name: "block1",
    present: exp.stims_block1,
    start: function () {
      $(".err").hide();
    },
    present_handle: function (stim) {
      $(".bar").css("width", (100 * exp.phase) / exp.nQs + "%");
      this.stim = stim;
      this.stim.trial_start = Date.now();
      $(".err").hide();
      this.init_sliders();
      exp.sliderPost = null;
      console.log(this.stim);
      var sentence =
        "<strong>" +
        this.stim.name +
        ' asks:</strong> "<i>' +
        this.stim.utterance +
        '</i>"';
      $(".sentence").html(sentence);
      var question =
        "Is " + this.stim.name + " certain that the statement below is true?";
      $(".question").html(question);
      $(".statement").html('<i>' + this.stim.statement + '</i>.');
    },

    button: function () {
      console.log(exp.sliderPost);
      if (exp.sliderPost != null) {
        this.log_responses();
        _stream.apply(this); //use exp.go() if and only if there is no "present" data.
      } else {
        $(".err").show();
      }
    },
    init_sliders: function () {
      utils.make_slider("#single_slider", function (event, ui) {
        exp.sliderPost = ui.value;
      });
    },
    log_responses: function () {
      exp.data_trials.push({
        slide_number_in_experiment: exp.phase,
        item_id: this.stim.item_id,
        trigger: this.stim.trigger,
        trigger_class: this.stim.trigger_class,
        matrix_subject: this.stim.matrix_subject,
        scale: this.stim.scale,
        condition: this.stim.condition,
        utterance: this.stim.utterance,
        statement: this.stim.statement,
        speakerName: this.stim.name,
        speakerGender: this.stim.gender,
        response: exp.sliderPost,
        rt: Date.now() - this.stim.trial_start,
      });
    },
  });

  slides.questionaire = slide({
    name: "questionaire",
    submit: function (e) {
      exp.subj_data = {
        language: $("#language").val(),
        assess: $('input[name="assess"]:checked').val(),
        education: $('input[name="education"]:checked').val(),
        enjoyment: $('input[name="enjoyment"]:checked').val(),
        american: $('input[name="american"]:checked').val(),
        gender: $('input[name="gender"]:checked').val(),
        age: $("#age").val(),
        problems: $("#problems").val(),
        comments: $("#comments").val(),
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    },
  });

  slides.finished = slide({
    name: "finished",
    start: function () {
      exp.data = {
        trials: exp.data_trials,
        catch_trials: exp.catch_trials,
        comprehension_check_1: exp.comprehension_check_1,
        system: exp.system,
        condition: exp.condition,
        subject_information: exp.subj_data,
        time_in_minutes: (Date.now() - exp.startT) / 60000,
      };

      setTimeout(function () {
        proliferate.submit(exp.data);
      }, 1000);
    },
  });

  return slides;
}

/// init ///

// Embedding verbs used to build each trial's question, e.g. "Does Patrick know that ...?"
var triggers = [
  { trigger: "know", trigger_class: "NonProj", aux: "Does", verb_phrase: "know" },
  { trigger: "discover", trigger_class: "NonProj", aux: "Did", verb_phrase: "discover" },
  { trigger: "reveal", trigger_class: "NonProj", aux: "Did", verb_phrase: "reveal" },
  { trigger: "establish", trigger_class: "NonProj", aux: "Did", verb_phrase: "establish" },
  { trigger: "think", trigger_class: "NonProj", aux: "Does", verb_phrase: "think" },
  { trigger: "suggest", trigger_class: "C", aux: "Did", verb_phrase: "suggest" },
  { trigger: "prove", trigger_class: "C", aux: "Did", verb_phrase: "prove" },
  { trigger: "say", trigger_class: "C", aux: "Did", verb_phrase: "say" },
  { trigger: "hear", trigger_class: "C", aux: "Did", verb_phrase: "hear" },
  { trigger: "inform_Sam", trigger_class: "C", aux: "Did", verb_phrase: "inform Sam" },
  { trigger: "acknowledge", trigger_class: "C", aux: "Did", verb_phrase: "acknowledge" },
  { trigger: "confirm", trigger_class: "C", aux: "Did", verb_phrase: "confirm" },
];

function buildUtterance(trigger, matrix_subject, content) {
  return (
    trigger.aux +
    " " +
    matrix_subject +
    " " +
    trigger.verb_phrase +
    " that " +
    content +
    "?"
  );
}

// Within each scale (some/or), split items evenly between the ub_rephrase and lb_rephrase conditions.
function assignConditions(stim_rows) {
  var by_scale = {};
  stim_rows.forEach(function (row) {
    by_scale[row.scale] = by_scale[row.scale] || [];
    by_scale[row.scale].push(row);
  });
  _.each(by_scale, function (group) {
    var shuffled = _.shuffle(group);
    var half = Math.floor(shuffled.length / 2);
    shuffled.forEach(function (row, i) {
      row.condition = i < half ? "ub" : "lb";
      row.statement = row.condition == "ub" ? row.ub_rephrase : row.lb_rephrase;
    });
  });
}

function init(stim_rows) {
  var speaker_names = _.shuffle([
    {
      name: "James",
      gender: "M",
    },
    {
      name: "Robert",
      gender: "M",
    },
    {
      name: "William",
      gender: "M",
    },
    {
      name: "David",
      gender: "M",
    },
    {
      name: "Richard",
      gender: "M",
    },
    {
      name: "Thomas",
      gender: "M",
    },
    {
      name: "Christopher",
      gender: "M",
    },
    {
      name: "Matthew",
      gender: "M",
    },
    {
      name: "Paul",
      gender: "M",
    },
    {
      name: "Mark",
      gender: "M",
    },
    {
      name: "George",
      gender: "M",
    },
    {
      name: "Steven",
      gender: "M",
    },
    {
      name: "Kenneth",
      gender: "M",
    },
    {
      name: "Jennifer",
      gender: "F",
    },
    {
      name: "Elizabeth",
      gender: "F",
    },
    {
      name: "Linda",
      gender: "F",
    },
    {
      name: "Susan",
      gender: "F",
    },
    {
      name: "Margaret",
      gender: "F",
    },
    {
      name: "Jessica",
      gender: "F",
    },
    {
      name: "Dorothy",
      gender: "F",
    },
    {
      name: "Sarah",
      gender: "F",
    },
    {
      name: "Karen",
      gender: "F",
    },
    {
      name: "Nancy",
      gender: "F",
    },
    {
      name: "Betty",
      gender: "F",
    },
    {
      name: "Lisa",
      gender: "F",
    },
    {
      name: "Sandra",
      gender: "F",
    },
    {
      name: "Helen",
      gender: "F",
    },
    {
      name: "Ashley",
      gender: "F",
    },
    {
      name: "Donna",
      gender: "F",
    },
    {
      name: "Kimberly",
      gender: "F",
    },
    {
      name: "Carol",
      gender: "F",
    },
    {
      name: "Michelle",
      gender: "F",
    },
  ]);

  assignConditions(stim_rows);
  var shuffled_items = _.shuffle(stim_rows);
  var shuffled_triggers = _.shuffle(triggers);

  function makeStim(i) {
    var item = shuffled_items[i];
    var trigger = shuffled_triggers[i];
    var name_data = speaker_names[i];

    return {
      name: name_data.name,
      gender: name_data.gender,
      trigger: trigger.trigger,
      trigger_class: trigger.trigger_class,
      item_id: item.item_id,
      matrix_subject: item.matrix_subject,
      scale: item.scale,
      condition: item.condition,
      statement: item.statement,
      utterance: buildUtterance(trigger, item.matrix_subject, item.content),
    };
  }

  control_items = [
    {
      item_id: "control1",
      short_trigger: "control",
      utterance: "Is Zack coming to the meeting tomorrow?",
      content: "Zack is coming to the meeting tomorrow",
    },
    {
      item_id: "control2",
      short_trigger: "control",
      utterance: "Is Mary's aunt sick?",
      content: "Mary's aunt is sick",
    },
    {
      item_id: "control3",
      short_trigger: "control",
      utterance: "Did Todd play football in high school?",
      content: "Todd played football in high school",
    },
    {
      item_id: "control4",
      short_trigger: "control",
      utterance: "Is Vanessa good at math?",
      content: "Vanessa is good at math",
    },
    {
      item_id: "control5",
      short_trigger: "control",
      utterance: "Did Madison have a baby?",
      content: "Madison had a baby",
    },
    {
      item_id: "control6",
      short_trigger: "control",
      utterance: "Was Hendrick's car expensive?",
      content: "Hendrick's car was expensive",
    },
  ];

  function makeControlStim(i) {
    var item = control_items[i];
    // Continue the speaker pool where the main items left off, so no name is reused.
    var name_data = speaker_names[shuffled_items.length + i];

    return {
      name: name_data.name,
      gender: name_data.gender,
      trigger: item.short_trigger,
      short_trigger: item.short_trigger,
      trigger_class: "control",
      item_id: item.item_id,
      utterance: item.utterance,
      statement: item.content,
    };
  }

  exp.stims_block1 = [];
  for (var i = 0; i < shuffled_items.length; i++) {
    exp.stims_block1.push(makeStim(i));
  }

  for (var j = 0; j < control_items.length; j++) {
    exp.stims_block1.push(makeControlStim(j));
  }

  exp.stims_block1 = _.shuffle(exp.stims_block1);

  exp.trials = [];
  exp.catch_trials = [];
  exp.condition = {}; //can randomize between subject conditions here
  exp.system = {
    Browser: BrowserDetect.browser,
    OS: BrowserDetect.OS,
    screenH: screen.height,
    screenUH: exp.height,
    screenW: screen.width,
    screenUW: exp.width,
  };
  //blocks of the experiment:
  exp.structure = [
    "i0",
    "consent",
    "instructions",
    "comprehension1",
    "block1",
    "questionaire",
    "finished",
  ];

  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
  //relies on structure and slides being defined

  // exp.nQs = 2 + 20 + 1;
  $(".nQs").html(exp.nQs);

  $(".slide").hide(); //hide everything

  $("#start_button").on("click", function () {
    exp.go();
  });
  exp.go(); //show first slide
}
document.addEventListener("DOMContentLoaded", function () {
  init(PSE_STIMULI);
});
