{application, yi_server,
 [{description, "game server"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, {yi_server_app, []}},
  {applications,
   [kernel,
    stdlib
   ]},
  {env,[]},
  {modules, []},

  {licenses, ["Apache 2.0"]},
  {links, []}
 ]}.
