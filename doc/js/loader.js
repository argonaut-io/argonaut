window.examples = function (names) {
  var count = names.length;

  _.each(names,  function (name) {
    $.get('../../src/main/scala/argonaut/doc/' + name + '.scala', function (data) {
      $(document).ready(function () {
        $('#example_' + name).html(data);
        count--;
        if (count === 0)
          prettyPrint();
      });
    });
  });
};
