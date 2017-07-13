// https://stackoverflow.com/a/5077091/905845
String.prototype.format = function() {
  var args = arguments;
  return this.replace(/\{\{|\}\}|\{(\d+)\}/g, function(m, n) {
    if (m == "{{") { return "{"; }
    if (m == "}}") { return "}"; }
    return args[n];
  });
};

// https://stackoverflow.com/a/3291856/905845
String.prototype.capitalize = function() {
  return this.charAt(0).toUpperCase() + this.slice(1);
}

function make_number_option(value, is_default) {
  return '<option value="{0}"{1}>{2}{3}</option>'.format(
    value,
    is_default ? " selected" : "",
    Math.round(100 * value) / 100,
    is_default ? " (default)" : "",
  );
}

function make_bool_option(value, is_default) {
  return '<option value="{0}"{1}>{2}{3}</option>'.format(
    value,
    is_default ? " selected" : "",
    value,
    is_default ? " (default)" : "",
  );
}

function make_options(default_value) {
  if(default_value === true || default_value === false) {
    var options = make_bool_option(default_value, is_default=true);
    options += make_bool_option(!default_value);
    return options;
  } else {
    var options = make_number_option(0);
    for(var i = 4; i != 0; --i) {
      options += make_number_option(default_value * Math.sqrt(2) ** -i);
    };
    options += make_number_option(default_value, is_default=true);
    for(var i = 1; i != 10; ++i) {
      options += make_number_option(default_value * Math.sqrt(2) ** i);
    };
    return options;
  }
}

var select_html_template = '<div class="form-group"><label for="{0}">{1}:</label><select class="form-control" id="{0}">{2}</select></div>';
function make_settings(selector, settings) {
  for(var name in settings) {
    var default_value = settings[name];
    $(selector).append(select_html_template.format(name, name.replace(/_/g, ' ').capitalize(), make_options(default_value)))
  };
}

function get_settings(selector) {
  var settings = {};
  $(selector).find("select").each(function() {
    var setting = $(this);
    var name = setting.attr("id");
    var value = setting.val();
    if(value == "true") {
      settings[name] = true;
    } else if(value == "false") {
      settings[name] = false;
    } else {
      settings[name] = parseFloat(value);
    }
  });
  return settings;
}
