function render() {

  if (typeof localStorage == 'undefined') {
    $("#site").text("HouseTab cannot run on a browser without localStorage.");
    throw new Error("No localStorage.");
  }

  if (localStorage["housetab_token"]) {
    var token = localStorage["housetab_token"];

    var logoutDom = $("<button>").text("Logout").on("click", function () {
      delete localStorage["housetab_token"];
      $("#site").text("Logging out...");
      $.get("/api/accounts/session/delete?token=" + token, function () {
        render();
      });
    });

    $("#nav").html(logoutDom);


    $("#site").text("Loading...");

    $.get("/api/entries?token=" + token, function(r) {
      var tableDom = $("<table>");
      r.forEach(function (e) {
        var trDom = $("<tr>");
        trDom.append($("<td>").text(e.entryCategory));
        trDom.append($("<td>").text(e.entryWhat));
        trDom.append($("<td>").text("$" + e.entryHowMuch % 100 + "." + e.entryHowMuch / 100));
        trDom.append($("<td>").text(e.entryDate));
        tableDom.append(trDom);
      });
      $("#site").html(tableDom);
    });

  } else {

    var formDom = $("<form>").on("submit", function () {
      var form = $(this);

      var name = form.find("input[name='username']").val();
      var pass = form.find("input[name='password']").val();


      form.find("input").prop("disabled", true);

      $.get("/api/accounts/session/new?name=" + name + "&password=" + pass, function(r) {
        form.find("input").prop("disabled", false);

        if (r.tag === 'Authed') {
          localStorage["housetab_token"] = r.contents;
          render();
        } else {
          form.prepend($("<div style='background-color: #FFC0CB'>")
                      .text("Username or password incorrect"));
        }
      });

      return false;
    });

    formDom.append($("<label for='username'>Username: <input name='username'/></label>"));
    formDom.append($("<label for='password'>Password: <input name='password' type='password'/></label>"));
    formDom.append($("<input type='submit' value='Login'/>"));

    $("#nav").html(formDom);
    $("#site").text("");
  }

}


$(render);
