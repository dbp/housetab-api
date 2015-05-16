if (typeof localStorage == 'undefined') {
  throw new Error("No localStorage.");
}

var NavBar = {
  logout: function (e, ctrl) {
    e.preventDefault();
    var token = localStorage["housetab_token"];
    delete localStorage["housetab_token"];
    delete localStorage["housetab_account"];
    $.get("/api/accounts/session/delete?token=" + token);
    ctrl.error("");
    return;
  },
  login: function (e, ctrl) {
    e.preventDefault();
    var username = ctrl.username();
    var password = ctrl.password();
    if (!username || !password) {
      ctrl.error("Username and password required.");
      return;
    }

    m.request({method: "GET",
               url: "/api/accounts/session/new?name=" +
               username + "&password=" + password}).then(function (data) {
                 if (data.tag === 'Authed') {
                   localStorage["housetab_token"] = data.contents;
                   localStorage["housetab_account"] = username;
                   ctrl.error("");
                 } else {
                   ctrl.error("Username or password incorrect");
                 }
               }, ctrl.error);

    return;
  },

  controller: function() {
    this.error = m.prop("");
    this.username = m.prop("");
    this.password = m.prop("");
  },

  view: function(ctrl) {
    function nav(d) {
      return m("ul.nav.navbar-nav.navbar-right", [d, m("li", m("a", {href: "#"}, "About"))]);
    }

    if (typeof localStorage["housetab_token"] === "undefined") {
      return nav(
        m("li.generated",
          m("form.navbar-form.navbar-right",
            [m("span.label.label-danger", ctrl.error()),
             m("input.form-control",  {placeholder: "Username...",
                                       oninput: m.withAttr("value", ctrl.username),
                                       value: ctrl.username()}),
             m("input.form-control", {type: "password",
                                      placeholder: "Password...",
                                      oninput: m.withAttr("value", ctrl.password),
                                      value: ctrl.password()}),
             m("input.form-control", {type: "submit",
                                      value: "Login",
                                      onclick: function(e) { NavBar.login(e,ctrl) }})
            ]
           )
         )
      );
    } else {
      return nav(
        m("li.generated", m("a", {href: "#",
                                  onclick: function (e) { NavBar.logout(e,ctrl) }
                                 },
                            "Logout"))
      );
    }
  }
};



m.mount(document.getElementById('navbar'), NavBar);



var Entries = {
  controller: function () {
    this.entries = m.prop([]);

    if (localStorage["housetab_token"]) {
      var token = localStorage["housetab_token"];
      m.request({
        method: "GET",
        url: "/api/entries?token=" + token}).then(this.entries, console.error);
    }
  },

  view: function (ctrl) {
    if (localStorage["housetab_token"] && localStorage["housetab_account"]) {
      var token = localStorage["housetab_token"];
      var account = localStorage["housetab_account"];

      var entryNodes = ctrl.entries().map(function (e) {
        return m("tr",
                 [m("td", e.entryCategory),
                  m("td", e.entryWhat),
                  m("td", "$" + e.entryHowMuch),
                  m("td", (new Date(e.entryDate)).toLocaleDateString()),
                 ]);
      });

      return m(".table-responsive",
               [m("table.table.table-striped",
                  [m("thead",
                     [m("tr",
                        [m("th", "Category"),
                         m("th", "What"),
                         m("th", "How Much"),
                         m("th", "Date")
                        ])
                     ]),
                   m("tbody", entryNodes)
                  ])
               ]);
    } else {
      return;
    }
  }
};

m.mount(document.getElementById('main'), Entries);


var Persons = {
  controller: function () {
    this.persons = m.prop([]);

    if (localStorage["housetab_token"]) {
      var token = localStorage["housetab_token"];
      var set_persons = this.persons;
      m.request({
        method: "GET",
        url: "/api/persons?token=" + token}).then(this.persons, console.error);
    }
  },

  view: function (ctrl) {
    var personsNodes = ctrl.persons().map(function (p) {
      return m(".generated.col-xs-6.col-sm-3",
               [m("h4", p.personName),
                m("span.text-muted", p.personCurrentShare)
               ]);
    });

    return m("div", personsNodes);
  }
};

m.mount(document.getElementById('persons'), Persons);


setInterval(function () {
    $.get("/api/accounts/session/check", function (r) {
        if (r) {
            $.get("/api/accounts/session/touch");
        } else {
            colsole.log("TODO");
            logout();
        }
    });
});
