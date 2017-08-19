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

function make_options(setting_description) {
  var setting_type = setting_description.setting_type;
  var default_value = setting_description.default_value;
  if(setting_type == "bool") {
    var options = make_bool_option(default_value, is_default=true);
    options += make_bool_option(!default_value);
    return options;
  } else if(setting_type == "float") {
    var options = make_number_option(0);
    for(var i = 4; i != 0; --i) {
      options += make_number_option(default_value * Math.sqrt(2) ** -i);
    };
    options += make_number_option(default_value, is_default=true);
    for(var i = 1; i != 10; ++i) {
      options += make_number_option(default_value * Math.sqrt(2) ** i);
    };
    return options;
  } else {
    return "<option>SETTING TYPE NOT HANDLED: {0}</value>".format(setting_type)
  }
}

function make_settings(selector, settings_description) {
  for(var name in settings_description) {
    var setting_description = settings_description[name];
    var control_type = setting_description.control_type;
    var control;
    if(control_type == "select") {
      control = '<div class="form-group"><label for="{0}">{1}:</label><select class="form-control" id="{0}">{2}</select></div>'.format(
        name, name.replace(/_/g, ' ').capitalize(), make_options(setting_description)
      );
    } else if(control_type == "rules_list") {
      control = '<div class="form-group"><label for="{0}">{1} (Select several with Ctrl+click):</label><select multiple class="form-control rules_list" id="{0}"></select></div>'.format(
        name, name.replace(/_/g, ' ').capitalize()
      );
    } else {
      control = "<div><p>Control type not handled for {0}: {1}</p></div>".format(name, control_type);
    }
    $(selector).append(control);
  };
}

function update_rules_list(rules) {
  $(".rules_list").each(function() {
    var select = $(this);
    var value = select.val();
    select.html("");
    for(var i = 0; i != rules.length; ++i) {
      var selected = value.indexOf(rules[i]) > -1;
      select.append(
        '<option value="{0}"{1}>{0}</option>'.format(rules[i], selected ? " selected" : "")
      );
    }
  })
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
      var as_float = parseFloat(value);
      if(isNaN(as_float)) {
        settings[name] = value;
      } else {
        settings[name] = as_float;
      }
    }
  });
  return settings;
}
