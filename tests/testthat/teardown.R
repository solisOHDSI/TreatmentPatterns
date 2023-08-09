if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
  DBI::dbDisconnect(con, shutdown = TRUE)
  Andromeda::close(andromedaSetup)
}
