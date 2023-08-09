if (!interactive() && !env_var_is_true("NOT_CRAN")) {
  DBI::dbDisconnect(con, shutdown = TRUE)
  Andromeda::close(andromedaSetup)
}
