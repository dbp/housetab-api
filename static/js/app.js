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
    this.username = m.prop(localStorage["housetab_account"] || "");
    this.password = m.prop("");
  },

  view: function(ctrl) {
    function nav(d) {
      return m(".container-fluid",
               [m(".navbar-header",
                  m("a.navbar-brand[href='/'", { config: m.route },
                    "HouseTab: " + ctrl.username())),
                m(".navbar-collapse.collapse",
                  m("ul.nav.navbar-nav.navbar-right",
                    [d,
                     m("li", m("a", {href: "#"}, "About"))])
                 )
               ]);
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


var Persons = {
  controller: function () {
    this.persons = m.prop([]);

    if (localStorage["housetab_token"]) {
      var token = localStorage["housetab_token"];
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



// setInterval(function () {
//     $.get("/api/accounts/session/check", function (r) {
//         if (r) {
//             $.get("/api/accounts/session/touch");
//         } else {
//             colsole.log("TODO");
//             logout();
//         }
//     });
// });

function template(main) {
  return m("div",
           [m("nav.navbar.navbar-inverse.navbar-fixed-top",
              m(".container-fluid", m.component(NavBar))),
            m(".container-fluid",
              m(".row",
                [m(".col-sm-3.col-md-2.sidebar",
                   m("ul.nav.nav-sidebar",
                     [m("li.active", m("a[href='/']", { config: m.route }, "Overview")),
                      m("li", m("a[href='#']", "Reports")),
                      m("li", m("a[href='/history']", { config: m.route }, "History")),
                      m("li", m("a[href='#']", "Settings")),
                      m("li", m("a[href='/docs']", { config: m.route }, "Export"))
                     ])),
                 m(".col-sm-9.col-sm-offset-3.col-md-10.col-md-offset-2.main",
                   main)]))
           ]);
}

var Home = {
  controller: function () {

  },

  view: function (ctrl) {
    return template([m("#persons.row", m.component(Persons)),
                     m("h2.sub-header", "Entries"),
                     m("#main", m.component(Entries))]);
  }

};

var Docs = {
  controller: function () {
    this.docs = m.prop("");
    m.request({method: "GET", url: "/api/docs"}).then(this.docs, console.error);
  },

  view: function (ctrl) {
    return template([m("h2.sub-header", "Exporting Data"),
                     m("p", "Currently, the best way to get data out is to use the API. Full documentation of it follows:"),
                     m("pre", ctrl.docs())
                    ]);
  }
};

var History = {
  controller: function () {
    this.log = m.prop([]);

    if (localStorage["housetab_token"]) {
      var token = localStorage["housetab_token"];
      m.request({method: "GET",
                 url: "/api/logs?token=" + token}).then(this.log, console.error);
    }
  },

  view: function (ctrl) {

    var logNodes = ctrl.log().map(function (e) {
        return m("tr",
                 [m("td", e.logCategoryOld),
                  m("td", e.logCategoryNew),
                  m("td", e.logWhatOld),
                  m("td", e.logWhatNew),
                  m("td", "$" + e.logHowMuchOld),
                  m("td", "$" + e.logHowMuchNew),
                  m("td", (new Date(e.logDateOld)).toLocaleDateString()),
                  m("td", (new Date(e.logDateNew)).toLocaleDateString()),
                 ]);
      });

    return template([m("h2.sub-header", "History"),
                     m("#main",
                       m(".table-responsive",
                         [m("table.table.table-striped",
                            [m("thead",
                               [m("tr",
                                  [m("th", "Category (Old)"),
                                   m("th", "Category (New)"),
                                   m("th", "What (Old)"),
                                   m("th", "What (New)"),
                                   m("th", "How Much (Old)"),
                                   m("th", "How Much (New)"),
                                   m("th", "Date (Old)"),
                                   m("th", "Date (New)")
                                  ])
                               ]),
                             m("tbody", logNodes)
                            ])
                         ]
                        )
                      )
                    ]);
  }
};

m.route(document.body, "/", {
  "/": Home,
  "/docs": Docs,
  "/history": History
});
