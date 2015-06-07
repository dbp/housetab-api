if (typeof localStorage == 'undefined') {
  throw new Error("No localStorage.");
}

var app = {};

app.session = {};
app.session.init = function () {
  this.username = m.prop(localStorage["housetab_account"] || "");
  this.account_id = m.prop(localStorage["housetab_account_id"] || "");
  this.token = m.prop(localStorage["housetab_token"] || "");
};

app.session.init();

app.data = {};
app.data.init = {};

// Data definitions
function mk_editable(entry) {
  return { i: entry, n: { entryId: m.prop(entry.entryId),
                          entryWhoPays: m.prop(entry.entryWhoPays),
                          entryDate: m.prop(entry.entryDate),
                          entryWhat: m.prop(entry.entryWhat),
                          entryWho: m.prop(entry.entryWho),
                          entryCategory: m.prop(entry.entryCategory),
                          entryHowMuch: m.prop(entry.entryHowMuch),
                        } };
}

function get_entry_data(entry) {
  return { entryWhoPays: entry.entryWhoPays(),
           entryDate: entry.entryDate(),
           entryWhat: entry.entryWhat(),
           entryWho: Number(entry.entryWho()),
           entryCategory: entry.entryCategory(),
           entryHowMuch: Number(entry.entryHowMuch()),
           entryAccountId: Number(app.session.account_id()),
           entryId: entry.entryId() || []
         };
}

function save_entry(entry) {
  var data = get_entry_data(entry.n);
  data.entryId = [];

  m.request({ method: "POST",
              url: "/api/entries/" + entry.i.entryId + "?token=" + app.session.token(),
              data: data
            }, console.error).then(function (_) {
              data.entryId = entry.i.entryId;
              entry.i = data;
              console.log("Saved entry " + entry.i.entryId);
            });
}

function entry_save_button(entry) {
  var current = get_entry_data(entry.n);
  if (Object.keys(entry.i).every(function (key) {
    return entry.i[key] === current[key];
  })) {
    return [];
  } else {
    return [
      // m("span.label.label-danger", ctrl.entry_validation()),
      m("button.btn",
        {onclick: function () { save_entry(entry); }},
        "Save")
    ];
  }
}

app.data.init.entries = function () {
  app.data.entries = m.prop([]);
  m.request({
    method: "GET",
    url: "/api/entries?token=" + app.session.token()
  }).then(function (entries) { app.data.entries(entries.map(mk_editable)); }, console.error);
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
    delete localStorage["housetab_account_id"];
    app.session.token("");
    app.session.username("");
    app.session.account_id("");
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
                   localStorage["housetab_token"] = data.contents[1];
                   app.session.token(localStorage["housetab_token"]);
                   localStorage["housetab_account_id"] = data.contents[0];
                   app.session.account_id(localStorage["housetab_account_id"]);
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

var pikaday = function (date) {
  return function (el, isInitialized) {
    if (isInitialized) {
      return;
    }

    // Everything here is Pikaday-related...
    var input = document.createElement('input');
    input.className = "form-control";

    if (Date.parse(date()) === NaN) {
      var current = null;
    } else {
      var current = new Date(date());
    }

    function setValue() {
      if (current) {
        input.value = current.getFullYear() + "/" + (current.getMonth() + 1) + "/" + current.getDate();
      }
    }

    setValue();

    el.appendChild(input);

    new Pikaday({defaultDate: current,
                 field: input,
                 onSelect: function () {
	           // Except here, where we bind Pikaday's events back to the Mithril model
	           date(this.getDate().toISOString());
                   current = this.getDate();
                   setValue();
                   m.redraw();
                 }
                });
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

    this.page = 0;

    this.page_size = 50;

    this.new_entry = { entryWhoPays: m.prop([]),
                       entryDate: m.prop(new Date().toISOString()),
                       entryWhat: m.prop(""),
                       entryWho: m.prop(0),
                       entryCategory: m.prop(""),
                       entryHowMuch: m.prop(""),
                     };

    this.entry_validation = m.prop("");

    this.submit_new_entry = function (ctrl) {
      return function (e) {
        e.preventDefault();
        var data = { entryWhoPays: ctrl.new_entry.entryWhoPays(),
                     entryDate: ctrl.new_entry.entryDate(),
                     entryWhat: ctrl.new_entry.entryWhat(),
                     entryWho: ctrl.new_entry.entryWho(),
                     entryCategory: ctrl.new_entry.entryCategory(),
                     entryHowMuch: ctrl.new_entry.entryHowMuch(),
                     entryAccountId: Number(app.session.account_id()),
                     entryId: []
                   };
        if (data.entryWho === 0) {
          ctrl.entry_validation("Must select Who.");
          return;
        }
        if (data.entryWhoPays.length === 0) {
          ctrl.entry_validation("Must choose at least one person for Who Pays.");
          return;
        }
        if (data.entryDate === "" || data.entryDate === NaN) {
          ctrl.entry_validation("Must enter date, like 2015/6/1.");
          return;
        }
        if (data.entryCategory === "") {
          ctrl.entry_validation("Must select category.");
          return;
        }
        m.request({ method: "POST",
                    url: "/api/entries?token=" + app.session.token(),
                    data: data
                  }, console.error).then(function (new_entry) {
                    app.data.entries([mk_editable(new_entry)].concat(app.data.entries()));
                  });
      };
    };
  },

  filter: function (ctrl, entries) {
    if (ctrl.what_search().length !== 0 || ctrl.category_selection() !== "all") {
      var cat_selected = ctrl.category_selection() !== "all";
      return entries.filter(function (item) {
        if (cat_selected) {
          return item.i.entryCategory === ctrl.category_selection() &&
            item.i.entryWhat.toLowerCase().indexOf(ctrl.what_search().toLowerCase()) > -1;
        } else {
          return item.i.entryWhat.toLowerCase().indexOf(ctrl.what_search().toLowerCase()) > -1;
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
      var entries = app.Entries.filter(ctrl, app.data.entries());

      if (entries.length > ctrl.page_size) {
        var lis = [];
        function mk_page_set(i) {
          return function () {
            ctrl.page = i;
            return false;
          };
        }
        for (var i = 0; i < entries.length / ctrl.page_size; i++) {
          if (ctrl.page === i) {
            lis.push(m("li.active", m("a[href=#]", { onclick: mk_page_set(i) }, i + 1)));
          } else {
            lis.push(m("li", m("a[href=#]", { onclick: mk_page_set(i) }, i + 1)));
          }
        }
        var pagination = m("ul.pagination", lis);
      } else {
        var pagination = m("div");
      }

      var entryNodes = entries.
          slice(ctrl.page*ctrl.page_size, ctrl.page*ctrl.page_size + ctrl.page_size).
          map(function (item) {
            var e = item.i;
            return m(".row",
                     [m(".col-md-1", m("select.form-control",
                                       {onchange: function () {} },
                                       [{personId: 0,personName:""}].concat(app.data.persons()).map(function (p) {
                                         if (p.personId === e.entryWho) {
                                           return m("option[selected=selected][value=" + p.personId + "]",
                                                    p.personName);
                                         } else {
                                           return m("option[value=" + p.personId + "]",
                                                    p.personName);
                                         }
                                       }))),
                      m(".col-md-2", m("select.form-control.pull-right",
                                       { onchange: function () {} },
                                       ctrl.categories.slice(1).map(function (c) {
                                         if (c === e.entryCategory) {
                                           return m("option[selected]", c);
                                         } else {
                                           return m("option", c);
                                         }
                                       }))),
                      m(".col-md-4", m("input.form-control",
                                       { oninput: m.withAttr("value", item.n.entryWhat),
                                         value: item.n.entryWhat()
                                       })),
                      m(".col-md-1", m(".input-group",
                                       [m(".input-group-addon", "$"),
                                        m("input.form-control",
                                          { oninput: m.withAttr("value", item.n.entryHowMuch),
                                            value: item.n.entryHowMuch()
                                          })])),
                      m(".col-md-1", { config: pikaday(item.n.entryDate) }),
                      m(".col-md-1",
                        app.data.persons().map(function (p) {
                          if (e.entryWhoPays.indexOf(p.personId) !== -1) {
                            return m("label",
                                     [m("input[type=checkbox][checked][value=" + p.personId + "].form-control",
                                        {onchange: function () {}}),
                                      p.personName])
                          } else {
                            return m("label",
                                     [m("input[type=checkbox][value=" + p.personId + "].form-control",
                                        {onchange: function () {}}),
                                      p.personName]);
                          }
                        })),
                      m(".col-md-2", entry_save_button(item))
                     ]);
          });


      return m("div",
               [m("h2.sub-header", "Entries"),
                m(".entries.container-fluid",
                  [m(".row",
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
                      m(".col-md-1", "Who Pays"),
                      m(".col-md-2")
                     ]),
                   m(".row",
                     [m(".col-md-1", m("select.form-control",
                                       {onchange:
                                        m.withAttr("value", function (v) {
                                          ctrl.new_entry.entryWho(Number(v));
                                        })},
                                       [{personId: 0,personName:""}].concat(app.data.persons()).map(function (p) {
                                         return m("option[value=" + p.personId + "]",
                                                  p.personName);
                                       }))),
                      m(".col-md-2", m("select.form-control",
                                       {onchange: m.withAttr("value",
                                                             ctrl.new_entry.entryCategory)},
                                       [""].concat(ctrl.categories.slice(1)).map(function (e) {
                                         return m("option", e);
                                       }))),
                      m(".col-md-4", m("input.form-control",
                                       {onchange: m.withAttr("value",
                                                             ctrl.new_entry.entryWhat)})),
                      m(".col-md-1", m("input.form-control",
                                       {onchange:
                                        m.withAttr("value",
                                                   function (v) {
                                                     ctrl.new_entry.entryHowMuch(Number(v));
                                                   })})),
                      m(".col-md-1", { config : pikaday(ctrl.new_entry.entryDate) }),
                      m(".col-md-1", app.data.persons().map(function (p) {
                        return m("label",
                                 [m("input[type=checkbox][value=" + p.personId + "].form-control",
                                    {onchange:
                                     m.withAttr("value",
                                                function (v) {
                                                  var v = Number(v);
                                                  var c = ctrl.new_entry.entryWhoPays();
                                                  if (c.indexOf(v) === -1) {
                                                    ctrl.new_entry.entryWhoPays(c.concat([v]));
                                                  } else {
                                                    ctrl.new_entry.entryWhoPays(c.filter(function (e) {
                                                      return e !== v;
                                                    }));
                                                  }
                                                })}),
                                  p.personName]);
                      })),
                      m(".col-md-2", [
                        m("span.label.label-danger", ctrl.entry_validation()),
                        m("button.btn",
                          {onclick: ctrl.submit_new_entry(ctrl)},
                          "Add")
                      ])
                     ]),
                   m("div", entryNodes),
                   m(".row", m(".col-md-12", pagination))
                  ])
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
