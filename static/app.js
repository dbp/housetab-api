function render() {

  if (typeof localStorage == 'undefined') {
    $("#site").text("HouseTab cannot run on a browser without localStorage.");
    throw new Error("No localStorage.");
  }

  $("#navbar").find(".generated").remove();

  if (localStorage["housetab_token"] && localStorage["housetab_account"]) {
    var token = localStorage["housetab_token"];
    var account = localStorage["housetab_account"];

    var logoutDom = $("<a href='#'>").text("Logout").on("click", function () {
      delete localStorage["housetab_token"];
      $("#site").text("Logging out...");
      $.get("/api/accounts/session/delete?token=" + token, function () {
        render();
      });
    });

    var logoutLiDom = $("<li class='generated'>").append(logoutDom);

    $(".navbar-nav").append(logoutLiDom);

    $(".page-header").text(account);

    $("#main").text("Loading...");

    $.get("/api/entries?token=" + token, function(r) {

      var holderDom = $("<div class='table-responsive'>");

      var tableDom = $("<table class='table table-striped'>");
      holderDom.append(tableDom);
      tableDom.append($("<thead><tr><th>Category</th><th>What</th><th>How Much</th><th>Date</th></tr></thead>"));

      var tableBodyDom = $("<tbody>");
      tableDom.append(tableBodyDom);

      r.forEach(function (e) {
        var trDom = $("<tr>");
        trDom.append($("<td>").text(e.entryCategory));
        trDom.append($("<td>").text(e.entryWhat));
        trDom.append($("<td>").text("$" + e.entryHowMuch));
        trDom.append($("<td>").text(e.entryDate));
        tableBodyDom.append(trDom);
      });
      $("#main").html(holderDom);
    });

  } else {
    var formDom = $("<form class='generated navbar-form navbar-right'>").on("submit", function () {
      var form = $(this);
      form.find('.error').remove();

      var name = form.find("input[name='username']").val();
      var pass = form.find("input[name='password']").val();


      form.find("input").prop("disabled", true);

      $.get("/api/accounts/session/new?name=" + name + "&password=" + pass, function(r) {
        form.find("input").prop("disabled", false);

        if (r.tag === 'Authed') {
          localStorage["housetab_token"] = r.contents;
          localStorage["housetab_account"] = name;
          render();
        } else {
          form.prepend($("<div class='error' style='background-color: #FFC0CB'>")
                       .text("Username or password incorrect"));
        }
      });

      return false;
    });

    formDom.append($("<input class='form-control' name='username' placeholder='Username...'/>"));
    formDom.append($("<input class='form-control' name='password' type='password' placeholder='Password...'/>"));
    formDom.append($("<input class='form-control' type='submit' value='Login'/>"));

    $("#navbar").append(formDom);
    $("#main").text();
  }

}


$(render);
