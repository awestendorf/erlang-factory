{application, ebank, [
  {description, "ebank 1.0"},
  {vsn, "1.0"},
  {modules, [ebank, atm, atm_hw, atm_sups, backend, db_list, ebank_sup, err_handler, gen_gs_server]},
  {registered, [backend, atm1]},
  {applications, [kernel, stdlib, sasl]},
  {mod, {ebank, [atm2]}}
]}.
