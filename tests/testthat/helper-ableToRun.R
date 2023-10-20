ableToRunCG <- function(method) {
  switch(
    method,
    CohortGenerator = {
      invisible(all(
        require("Eunomia", character.only = TRUE),
        require("CirceR", character.only = TRUE),
        require("CohortGenerator", character.only = TRUE)
      ))
    },
    CDMConnector = {
      invisible(all(
        require("CDMConnector", character.only = TRUE),
        require("CirceR", character.only = TRUE),
        require("DBI", character.only = TRUE),
        require("duckdb", character.only = TRUE)
      ))
    }
  )
}