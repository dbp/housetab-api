if (typeof localStorage == 'undefined') {
  throw new Error("No localStorage.");
}

var app = {};

app.session = {};
app.session.init = function () {
  this.username = m.prop(localStorage["housetab_account"] || "");
  this.token = m.prop(localStorage["housetab_token"] || "");
};

app.session.init();

app.data = {};
app.data.init = {};

// Data definitions
app.data.init.entries = function () {
  app.data.entries = m.prop([]);
  m.request({
    method: "GET",
    url: "/api/entries?token=" + app.session.token()
  }).then(app.data.entries, console.error);
};

app.data.init.persons = function () {
  app.data.persons = m.prop([]);
  m.request({
    method: "GET",
    url: "/api/persons?token=" + app.session.token()
  }).then(app.data.persons, console.error);
};

app.data.init.result = function () {
  app.data.result = m.prop([]);
  m.request({
    method: "GET",
    url: "/api/result?token=" + app.session.token()}).then(app.data.result, console.error);
};

app.data.init.log = function () {
  app.data.log = m.prop([]);
  m.request({method: "GET",
             url: "/api/logs?token=" + app.session.token()}).
    then(app.data.log, console.error);
};

// Data requirements for pages
app.data.init.Home = function () {
  app.data.init.entries();
  app.data.init.persons();
  app.data.init.result();
};

app.data.init.Log = function () {
  app.data.init.persons();
  app.data.init.log();
};


app.Signup = {
  controller: function () {
  },
  view: function (ctrl) {
    return m("div");
  }
};

app.NavBar = {
  logout: function (e, ctrl) {
    e.preventDefault();
    var token = app.session.token();
    delete localStorage["housetab_token"];
    delete localStorage["housetab_account"];
    app.session.token("");
    app.session.username("");
    m.request({method: "GET", url: "/api/accounts/session/delete?token=" + token}).
      then(function() { ctrl.error(""); m.route("/"); },
           ctrl.error);
    return;
  },
  login: function (e, ctrl) {
    e.preventDefault();
    var username = app.session.username();
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
                   app.session.token(data.contents);
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
    this.password = m.prop("");
  },

  view: function(ctrl) {

    if (app.session.token() === "") {
      return m("ul.nav.nav-sidebar",
               [m("li",
                  m("form",
                    [m("span.label.label-danger", ctrl.error()),
                     m("input.form-control",  {placeholder: "Username...",
                                               oninput: m.withAttr("value", app.session.username),
                                               value: app.session.username()}),
                     m("input.form-control", {type: "password",
                                              placeholder: "Password...",
                                              oninput: m.withAttr("value", ctrl.password),
                                              value: ctrl.password()}),
                     m("input.form-control", {type: "submit",
                                              value: "Login",
                                              onclick: function(e) { app.NavBar.login(e,ctrl) }})
                    ]
                   )
                 ),
                m("li",
                  m(".signup",
                    m("a.btn[href='/signup']", { config: m.route }, "Signup")))
               ]);
    } else {
      if (m.route() === "/") { var e_active = ".active"; } else { var e_active = "" }
      if (m.route() === "/history") { var h_active = ".active"; } else { var h_active = "" }
      if (m.route() === "/docs") { var d_active = ".active"; } else { var d_active = "" }

      return m("ul.nav.nav-sidebar",
               [m("li" + e_active, m("a[href='/']", { config: m.route }, "Entries")),
                m("li", m("a[href='#']", "Reports")),
                m("li" + h_active, m("a[href='/history']", { config: m.route }, "History")),
                m("li", m("a[href='#']", "Settings")),
                m("li" + d_active, m("a[href='/docs']", { config: m.route }, "Export")),
                m("li",
                  m("a", {href: "#",
                          onclick: function (e) { app.NavBar.logout(e,ctrl) }
                         },
                    "Logout")
                 )]);
    }
  }
};


app.Entries = {
  controller: function () {
    this.what_search = m.prop("");
    this.category_selection = m.prop("all");
    this.categories = ["all",
                       "groceries",
                       "toiletries",
                       "misc",
                       "rent",
                       "alcohol",
                       "furnishings",
                       "household",
                       "entertainment",
                       "cash",
                       "food",
                       "utilities"
                      ];

  },

  filter: function (ctrl, entries) {
    if (ctrl.what_search().length !== 0 || ctrl.category_selection() !== "all") {
      var cat_selected = ctrl.category_selection() !== "all";
      return entries.filter(function (item) {
        if (cat_selected) {
          return item.entryCategory === ctrl.category_selection() &&
            item.entryWhat.toLowerCase().indexOf(ctrl.what_search().toLowerCase()) > -1;
        } else {
          return item.entryWhat.toLowerCase().indexOf(ctrl.what_search().toLowerCase()) > -1;
        }
      });
    } else {
      return entries;
    }
  },

  view: function (ctrl) {
    var lookup_table = {};
    app.data.persons().forEach(function (e) { lookup_table[e.personId] = e.personName; });
    function person_name(id) {
      return lookup_table[id];
    }
    if (app.session.token() !== "") {
      var entryNodes = app.Entries.filter(ctrl, app.data.entries()).
          map(function (e) {
            return m(".row.well",
                     [m(".col-md-1", person_name(e.entryWho)),
                      m(".col-md-2", e.entryCategory),
                      m(".col-md-4", e.entryWhat),
                      m(".col-md-1", "$" + e.entryHowMuch),
                      m(".col-md-1", (new Date(e.entryDate)).toLocaleDateString()),
                      m(".col-md-1", e.entryWhoPays.map(person_name).join(", "))
                     ]);
          });

      return m("div",
               [m("h2.sub-header", "Entries"),
                m(".entries",
                  [m(".row.well",
                     [m(".col-md-1", "Who"),
                      m(".col-md-2.form-inline",
                        [m("span", "Category"),
                         m("select.form-control.pull-right",
                           { onchange: m.withAttr("value", ctrl.category_selection) },
                           ctrl.categories.map(function (e) {
                             return m("option", e);
                           }))]),
                      m(".col-md-4.form-inline",
                        [m("span", "What"),
                         m("input.form-control.pull-right",
                           {oninput: m.withAttr("value", ctrl.what_search),
                            placeholder: "Search..."}),]),
                      m(".col-md-1", "How Much"),
                      m(".col-md-1", "Date"),
                      m(".col-md-2", "Who Pays")
                     ]),
                   m("div", entryNodes)])
               ]);
    } else {
      return m("div");
    }
  }
};


app.Persons = {
  controller: function () {},

  view: function (ctrl) {
    if (app.data.result() !== {}) {
      var get_result = function (p) {
        var r = app.data.result().people.filter(function (e) {
          return e[0].personId === p;
        });
        if (r.length !== 0) {
          return r[0];
        }
      };
    } else {
      var get_result = function (p) {
        return null;
      }
    }
    function format_money(m) {
      if (m < 0) {
        return "-$" + (m * -1).toFixed(2);
      } else {
        return "$" + m.toFixed(2);
      }
    }
    var personsNodes = app.data.persons().map(function (p) {

      var r = get_result(p.personId);
      if (typeof r !== "null") {
        var spent = "spent " + format_money(r[1]);
        var owes = format_money(r[2]);
      }
      return m(".col-sm-2",
               m(".panel.panel-default",
                 [m(".panel-heading", m(".panel-title",
                                        [p.personName + " ",
                                         m("span.label.label-default.pull-right", spent),
                                         m("span.label.label-primary.pull-right",
                                           "at " + p.personCurrentShare + " shares"),
                                         m(".clearfix")
                                        ])),
                  m(".panel-body", [
                                    m("h3.text-center", owes)])
                 ]));
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

  if (app.session.username() !== "") {
    var about = "for the account of";
  } else {
    var about = "";
  }

  return m(".row",
           [m(".col-sm-2.col-md-1.sidebar",
              [m(".title", [m(".about", about),
                            m("img[src=/img/glyph.png]"),
                            app.session.username()]),
               m.component(app.NavBar)
              ]),
            m(".col-sm-10.col-md-11.main",
              main)]) ;
}

app.Login = {
  controller: function () {
    if (app.session.token() !== "") {
      m.route("/");
    }
  },

  view: function (ctrl) {
    return template(m("div"));
  }
};

app.Home = {
  controller: function () {
    if (app.session.token() === "") {
      m.route("/login");
    } else {
      app.data.init.Home();
    }
  },

  view: function (ctrl) {
    return template([m("#persons.row", m.component(app.Persons)),
                     m("#main", m.component(app.Entries))]);
  }

};

app.Docs = {
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

app.History = {
  controller: function () {
    if (app.session.token() === "") {
      m.route("/login");
    } else {
      app.data.init.Log();
    }
  },

  view: function (ctrl) {
    var lookup_table = {};
    app.data.persons().forEach(function (e) { lookup_table[e.personId] = e.personName; });
    function person_name(id) {
      return lookup_table[id];
    }

    var logNodes = app.data.log().map(function (e) {
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
  "/": app.Home,
  "/login": app.Login,
  "/signup" : app.Signup,
  "/docs": app.Docs,
  "/history": app.History
});
