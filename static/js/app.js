if (typeof localStorage == 'undefined') {
  throw new Error("No localStorage.");
}

var NavBar = {
  logout: function (e, ctrl) {
    e.preventDefault();
    var token = localStorage["housetab_token"];
    delete localStorage["housetab_token"];
    delete localStorage["housetab_account"];
    m.request({method: "GET", url: "/api/accounts/session/delete?token=" + token}).then(function() { ctrl.error(""); m.route("/"); }, ctrl.error);
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
                   m.route("/");
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
  controller: function (args) {
    this.entries = m.prop([]);

    if (localStorage["housetab_token"]) {
      var token = localStorage["housetab_token"];
      m.request({
        method: "GET",
        url: "/api/entries?token=" + token}).then(this.entries, console.error);
    }
  },

  view: function (ctrl, args) {
    var lookup_table = {};
    args.persons().forEach(function (e) { lookup_table[e.personId] = e.personName; });
    function person_name(id) {
      return lookup_table[id];
    }
    if (localStorage["housetab_token"] && localStorage["housetab_account"]) {
      var token = localStorage["housetab_token"];
      var account = localStorage["housetab_account"];

      var entryNodes = ctrl.entries().map(function (e) {
        return m("tr",
                 [m("td", person_name(e.entryWho)),
                  m("td", e.entryCategory),
                  m("td", e.entryWhat),
                  m("td", "$" + e.entryHowMuch),
                  m("td", (new Date(e.entryDate)).toLocaleDateString()),
                  m("td", e.entryWhoPays.map(person_name).join(", "))
                 ]);
      });

      return m("div",
               [m("h2.sub-header", "Entries"),
                m(".table-responsive",
                  [m("table.table.table-striped",
                     [m("thead",
                        [m("tr",
                           [m("th", "Who"),
                            m("th", "Category"),
                            m("th", "What"),
                            m("th", "How Much"),
                            m("th", "Date"),
                            m("th", "Who Pays")
                           ])
                        ]),
                      m("tbody", entryNodes)
                     ])
                  ])
               ]);
    } else {
      return m("div");
    }
  }
};


var Persons = {
  controller: function (args) {
    if (localStorage["housetab_token"]) {
      var token = localStorage["housetab_token"];
      m.request({
        method: "GET",
        url: "/api/persons?token=" + token}).then(args.persons, console.error);
    }
  },

  view: function (ctrl, args) {
    var personsNodes = args.persons().map(function (p) {
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
  if (localStorage["housetab_token"]) {
    if (m.route() === "/") { var e_active = ".active"; } else { var e_active = "" }
    if (m.route() === "/history") { var h_active = ".active"; } else { var h_active = "" }
    if (m.route() === "/docs") { var d_active = ".active"; } else { var d_active = "" }

    var menu = [m("li" + e_active, m("a[href='/']", { config: m.route }, "Entries")),
                m("li", m("a[href='#']", "Reports")),
                m("li" + h_active, m("a[href='/history']", { config: m.route }, "History")),
                m("li", m("a[href='#']", "Settings")),
                m("li" + d_active, m("a[href='/docs']", { config: m.route }, "Export"))
               ];
  } else {
    var menu = [];
  }

  return m("div",
           [m("nav.navbar.navbar-inverse.navbar-fixed-top",
              m(".container-fluid", m.component(NavBar))),
            m(".container-fluid",
              m(".row",
                [m(".col-sm-3.col-md-2.sidebar",
                   m("ul.nav.nav-sidebar",
                     menu)),
                 m(".col-sm-9.col-sm-offset-3.col-md-10.col-md-offset-2.main",
                   main)]))
           ]);
}

var Home = {
  controller: function () {
    this.persons = m.prop([]);
  },

  view: function (ctrl) {
    return template([m("#persons.row", m.component(Persons, { persons: ctrl.persons })),
                     m("#main", m.component(Entries, { persons: ctrl.persons }))]);
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
    this.persons = m.prop([]);

    if (localStorage["housetab_token"]) {
      var token = localStorage["housetab_token"];
      m.request({method: "GET",
                 url: "/api/logs?token=" + token}).then(this.log, console.error);
      m.request({method: "GET",
                 url: "/api/persons?token=" + token}).then(this.persons, console.error);
    }
  },

  view: function (ctrl) {
    var lookup_table = {};
    ctrl.persons().forEach(function (e) { lookup_table[e.personId] = e.personName; });
    function person_name(id) {
      return lookup_table[id];
    }

    var logNodes = ctrl.log().map(function (e) {
      if (e.logDateOld) {
        var old_date = (new Date(e.logDateOld)).toLocaleDateString();
      }
      if (e.logDateNew) {
        var new_date = (new Date(e.logDateNew)).toLocaleDateString();
      }

      if (e.logHowMuchOld) {
        var old_howmuch = "$" + e.logHowMuchOld;
      }
      if (e.logHowMuchNew) {
        var new_howmuch = "$" + e.logHowMuchNew;
      }

      if (e.logWhoPaysOld) {
        var old_whopays = e.logWhoPaysOld.map(person_name).join(", ");
      }
      if (e.logWhoPaysNew) {
        var new_whopays = e.logWhoPaysNew.map(person_name).join(", ");
      }

      return m("tr",
               [m("td", e.logType),
                m("td", person_name(e.logWhoOld)),
                m("td", person_name(e.logWhoNew)),
                m("td", e.logCategoryOld),
                m("td", e.logCategoryNew),
                m("td", e.logWhatOld),
                m("td", e.logWhatNew),
                m("td", old_howmuch),
                m("td", new_howmuch),
                m("td", old_date),
                m("td", new_date),
                m("td", old_whopays),
                m("td", new_whopays),
               ]);
    });

    return template([m("h2.sub-header", "History"),
                     m("#main",
                       m(".table-responsive",
                         [m("table.table.table-striped",
                            [m("thead",
                               [m("tr",
                                  [m("th", "Type"),
                                   m("th", "Who (Old)"),
                                   m("th", "Who (New)"),
                                   m("th", "Category (Old)"),
                                   m("th", "Category (New)"),
                                   m("th", "What (Old)"),
                                   m("th", "What (New)"),
                                   m("th", "How Much (Old)"),
                                   m("th", "How Much (New)"),
                                   m("th", "Date (Old)"),
                                   m("th", "Date (New)"),
                                   m("th", "Who Pays (Old)"),
                                   m("th", "Who Pays (New)")
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
