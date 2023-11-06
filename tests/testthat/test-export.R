# General ----
library(TreatmentPatterns)
library(testthat)

# Tests ----
test_that("void", {
  expect_error(
    export()
  )
})

# CohortGenerator ----
test_that("outputPath", {
  ## file.path(tempDirCG) ----
  tempDirLocal <- file.path(tempdir(), "output")

  export(andromedaCG, outputPath = tempDirLocal)

  expect_true(
    file.exists(file.path(tempDirLocal, "treatmentPathways.csv"))
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "summaryStatsTherapyDuraion.csv"))
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "countsYear.csv"))
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "countsAge.csv"))
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "countsSex.csv"))
  )

  ## 3 ----
  expect_error(
    export(andromedaCG, outputPath = 3),
    "Variable 'outputPath': No path provided"
  )

  withr::defer({
    unlink(tempDirLocal, recursive = TRUE)
    rm("tempDirLocal")
  })
})

test_that("ageWindow", {
  tempDirLocal <- file.path(tempdir(), "output")

  ## 10 ----
  expect_message(
    export(
      andromeda = andromedaCG,
      outputPath = tempDirLocal,
      ageWindow = 10
    )
  )

  treatmentPathways <- read.csv(file.path(tempDirLocal, "treatmentPathways.csv"))

  expect_true(
    all(c("0-10", "10-20", "20-30", "30-40", "40-50", "all") %in% treatmentPathways$age))

  ## c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 150) ----
  expect_message(
    export(
      andromeda = andromedaCG,
      outputPath = tempDirLocal,
      ageWindow = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 150)
    )
  )

  treatmentPathways <- read.csv(file.path(tempDirLocal, "treatmentPathways.csv"))

  expect_true(all(
    c("0-2", "2-4", "4-6", "6-8", "8-10", "10-12",
      "12-14", "14-16", "16-18", "18-150", "all") %in% treatmentPathways$age
  ))

  withr::defer({
    unlink(tempDirLocal, recursive = TRUE)
    rm("treatmentPathways", "tempDirLocal")
  })
})

test_that("minFreq", {
  tempDirLocal <- file.path(tempdir(), "output")
  ## 10 ----
  expect_message(
    export(
      andromeda = andromedaCG,
      outputPath = tempDirLocal,
      minFreq = 10
    ),
    "Removed \\d+ pathways with a frequency < 10."
  )

  treatmentPathways <- read.csv(file.path(tempDirLocal, "treatmentPathways.csv"))

  expect_equal(min(treatmentPathways$freq), 10)

  ## "10" ----
  expect_error(
    export(
      andromeda = andromedaCG,
      outputPath = tempDirLocal,
      minFreq = "10"
    )
  )

  withr::defer({
    unlink(tempDirLocal, recursive = TRUE)
    rm("treatmentPathways", "tempDirLocal")
  })
})

test_that("archiveName", {
  tempDirLocal <- file.path(tempdir(), "output")
  ## "output.zip" ----
  expect_message(
    export(
      andromeda = andromedaCG,
      outputPath = tempDirLocal,
      archiveName = "output.zip"
    )
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "output.zip"))
  )

  ## 3 ----
  expect_error(
    export(
      andromeda = andromedaCG,
      outputPath = tempDirLocal,
      archiveName = 3
    )
  )

  withr::defer({
    unlink(tempDirLocal, recursive = TRUE)
    rm("tempDirLocal")
  })
})

# CDMConnector ----
test_that("outputPath", {
  tempDirLocal <- file.path(tempdir(), "output")

  export(andromedaCDMC, outputPath = tempDirLocal)

  expect_true(
    file.exists(file.path(tempDirLocal, "treatmentPathways.csv"))
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "summaryStatsTherapyDuraion.csv"))
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "countsYear.csv"))
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "countsAge.csv"))
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "countsSex.csv"))
  )

  ## 3 ----
  expect_error(
    export(andromedaCDMC, outputPath = 3),
    "Variable 'outputPath': No path provided"
  )

  withr::defer({
    unlink(tempDirLocal, recursive = TRUE)
    rm("tempDirLocal")
  })
})

test_that("ageWindow", {
  tempDirLocal <- file.path(tempdir(), "output")

  ## 10 ----
  expect_message(
    export(
      andromeda = andromedaCDMC,
      outputPath = tempDirLocal,
      ageWindow = 10
    )
  )

  treatmentPathways <- read.csv(file.path(tempDirLocal, "treatmentPathways.csv"))

  expect_true(all(c("0-10", "10-20", "all") %in% treatmentPathways$age))

  ## c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 150) ----
  expect_message(
    export(
      andromeda = andromedaCDMC,
      outputPath = tempDirLocal,
      ageWindow = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 150)
    )
  )

  treatmentPathways <- read.csv(file.path(tempDirLocal, "treatmentPathways.csv"))

  expect_true(all(
    c("0-2", "2-4", "4-6", "6-8", "8-10", "10-12",
      "12-14", "14-16", "16-18", "18-150", "all") %in% treatmentPathways$age
  ))

  withr::defer({
    unlink(tempDirLocal, recursive = TRUE)
    rm("treatmentPathways", "tempDirLocal")
  })
})

test_that("minFreq", {
  tempDirLocal <- file.path(tempdir(), "output")
  ## 10 ----
  expect_message(
    export(
      andromeda = andromedaCDMC,
      outputPath = tempDirLocal,
      minFreq = 10
    ),
    "Removed \\d+ pathways with a frequency < 10."
  )

  treatmentPathways <- read.csv(file.path(tempDirLocal, "treatmentPathways.csv"))

  expect_equal(min(treatmentPathways$freq), 10)

  ## "10" ----
  expect_error(
    export(
      andromeda = andromedaCDMC,
      outputPath = tempDirLocal,
      minFreq = "10"
    )
  )

  withr::defer({
    unlink(tempDirLocal, recursive = TRUE)
    rm("treatmentPathways", "tempDirLocal")
  })
})

test_that("archiveName", {
  tempDirLocal <- file.path(tempdir(), "output")
  ## "output.zip" ----
  expect_message(
    export(
      andromeda = andromedaCDMC,
      outputPath = tempDirLocal,
      archiveName = "output.zip"
    )
  )

  expect_true(
    file.exists(file.path(tempDirLocal, "output.zip"))
  )

  ## 3 ----
  expect_error(
    export(
      andromeda = andromedaCDMC,
      outputPath = tempDirLocal,
      archiveName = 3
    )
  )

  withr::defer({
    unlink(tempDirLocal, recursive = TRUE)
    rm("tempDirLocal")
  })
})
