localAndromeda <- Andromeda::andromeda()

withr::defer({
  Andromeda::close(localAndromeda)
})

if (ableToRunCDMC()) {
  test_that("Method: new", {
    expect_true(R6::is.R6(
      TreatmentPatterns:::CDMInterface$new(
        cdm = cdm
      )
    ))
  })

  cdmInterface <- TreatmentPatterns:::CDMInterface$new(
    cdm = cdm
  )

  test_that("Method: validate", {
    expect_true(R6::is.R6(cdmInterface$validate()))
  })

  test_that("Method: fetchMetadata", {
    cdmInterface$fetchMetadata(localAndromeda)

    metadata <- localAndromeda$metadata %>% collect()

    expect_in(
      c("cdmSourceName", "cdmSourceAbbreviation", "cdmReleaseDate", "vocabularyVersion"),
      names(metadata)
    )

    expect_identical(metadata$rVersion, base::version$version.string)
    expect_identical(metadata$platform, base::version$platform)
    expect_identical(nrow(metadata), 1L)
    expect_identical(ncol(metadata), 8L)
  })

  localAndromeda$treatmentHistory <- CDMCAndromeda$treatmentHistory %>%
    select(-"age", -"sex")

  test_that("Method: fetchCohortTable", {
    # Update CDM with new dummy data
    cdmInterface <- TreatmentPatterns:::CDMInterface$new(
      cdm = cdm
    )
    
    # Viral Sinusitis
    cdmInterface$fetchCohortTable(
      cohorts = cohorts,
      cohortTableName = "cohort_table",
      andromeda = localAndromeda,
      andromedaTableName = "cohortTable",
      minEraDuration = 5
    )
    
    res <- localAndromeda$cohortTable

    expect_identical(ncol(res), 6L)
    expect_identical(res %>% collect() %>% nrow(), 11386L)

    # Empty
    cdmInterface$fetchCohortTable(
      cohorts = data.frame(
        cohortId = numeric(),
        cohortName = character(),
        type = character()
      ),
      cohortTableName = "cohort_table",
      andromeda = localAndromeda,
      andromedaTableName = "cohortTable",
      minEraDuration = 5
    )
    
    res <- localAndromeda$cohortTable
    
    expect_identical(ncol(res), 6L)
    expect_identical(res %>% collect() %>% nrow(), 0L)
  })
}
