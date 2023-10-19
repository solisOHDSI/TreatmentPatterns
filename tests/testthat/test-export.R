# General ----
library(TreatmentPatterns)
library(testthat)

andromedaCG <- Andromeda::loadAndromeda(andromedaCGPath)
# andromedaCDMC <- Andromeda::loadAndromeda(andromedaCDMCPath)

# Tests ----
test_that("void", {
  expect_error(
    export()
  )
})

# CohortGenerator ----
test_that("outputPath", {
  ## file.path(tempDirCG) ----
  export(andromedaCG, outputPath = tempDirCG)
  
  expect_true(
    file.exists(file.path(tempDirCG, "treatmentPathways.csv"))
  )
  
  expect_true(
    file.exists(file.path(tempDirCG, "summaryStatsTherapyDuraion.csv"))
  )
  
  expect_true(
    file.exists(file.path(tempDirCG, "countsYear.csv"))
  )
  
  expect_true(
    file.exists(file.path(tempDirCG, "countsAge.csv"))
  )
  
  expect_true(
    file.exists(file.path(tempDirCG, "countsSex.csv"))
  )
  
  ## 3 ----
  expect_error(
    export(andromedaCG, outputPath = 3),
    "Variable 'outputPath': No path provided"
  )
})

test_that("ageWindow", {
  ## 10 ----
  expect_message(
    export(
      andromeda = andromedaCG,
      outputPath = tempDirCG,
      ageWindow = 10
    )
  )
  
  treatmentPathways <- read.csv(file.path(tempDirCG, "treatmentPathways.csv"))
  
  expect_true(all(c("0-10", "10-20", "all") %in% treatmentPathways$age))
  
  ## c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 150) ----
  expect_message(
    export(
      andromeda = andromedaCG,
      outputPath = tempDirCG,
      ageWindow = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 150)
    )
  )
  
  treatmentPathways <- read.csv(file.path(tempDirCG, "treatmentPathways.csv"))
  
  expect_true(all(
    c("0-2", "2-4", "4-6", "6-8", "8-10", "10-12",
      "12-14", "14-16", "16-18", "18-150", "all") %in% treatmentPathways$age
  ))
})

test_that("minFreq", {
  ## 10 ----
  expect_message(
    export(
      andromeda = andromedaCG,
      outputPath = tempDirCG,
      minFreq = 10
    ),
    "Removed \\d+ pathways with a frequency < 10."
  )
  
  treatmentPathways <- read.csv(file.path(tempDirCG, "treatmentPathways.csv"))
  
  expect_equal(min(treatmentPathways$freq), 10)
  
  ## "10" ----
  expect_error(
    export(
      andromeda = andromedaCG,
      outputPath = tempDirCG,
      minFreq = "10"
    )
  )
})

test_that("archiveName", {
  ## "output.zip" ----
  expect_message(
    export(
      andromeda = andromedaCG,
      outputPath = tempDirCG,
      archiveName = "output.zip"
    )
  )

  expect_true(
    file.exists(file.path(tempDirCG, "output.zip"))
  )
  
  ## 3 ----
  expect_error(
    export(
      andromeda = andromedaCG,
      outputPath = tempDirCG,
      archiveName = 3
    )
  )
})

# # CDMConnector ----
# test_that("outputPath", {
#   ## file.path(tempDirCDMC) ----
#   export(andromedaCDMC, outputPath = tempDirCDMC)
#   
#   expect_true(
#     file.exists(file.path(tempDirCDMC, "treatmentPathways.csv"))
#   )
#   
#   expect_true(
#     file.exists(file.path(tempDirCDMC, "summaryStatsTherapyDuraion.csv"))
#   )
#   
#   expect_true(
#     file.exists(file.path(tempDirCDMC, "countsYear.csv"))
#   )
#   
#   expect_true(
#     file.exists(file.path(tempDirCDMC, "countsAge.csv"))
#   )
#   
#   expect_true(
#     file.exists(file.path(tempDirCDMC, "countsSex.csv"))
#   )
#   
#   ## 3 ----
#   expect_error(
#     export(andromedaCDMC, outputPath = 3),
#     "Variable 'outputPath': No path provided"
#   )
# })
# 
# test_that("ageWindow", {
#   ## 10 ----
#   expect_message(
#     export(
#       andromeda = andromedaCDMC,
#       outputPath = tempDirCDMC,
#       ageWindow = 10
#     )
#   )
#   
#   treatmentPathways <- read.csv(file.path(tempDirCDMC, "treatmentPathways.csv"))
#   
#   expect_true(all(c("0-10", "10-20", "all") %in% treatmentPathways$age))
#   
#   ## c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 150) ----
#   expect_message(
#     export(
#       andromeda = andromedaCDMC,
#       outputPath = tempDirCDMC,
#       ageWindow = c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 150)
#     )
#   )
#   
#   treatmentPathways <- read.csv(file.path(tempDirCDMC, "treatmentPathways.csv"))
#   
#   expect_true(all(
#     c("0-2", "2-4", "4-6", "6-8", "8-10", "10-12",
#       "12-14", "14-16", "16-18", "18-150", "all") %in% treatmentPathways$age
#   ))
# })
# 
# test_that("minFreq", {
#   ## 10 ----
#   expect_message(
#     export(
#       andromeda = andromedaCDMC,
#       outputPath = tempDirCDMC,
#       minFreq = 10
#     ),
#     "Removed \\d+ pathways with a frequency < 10."
#   )
#   
#   treatmentPathways <- read.csv(file.path(tempDirCDMC, "treatmentPathways.csv"))
#   
#   expect_equal(min(treatmentPathways$freq), 10)
#   
#   ## "10" ----
#   expect_error(
#     export(
#       andromeda = andromedaCDMC,
#       outputPath = tempDirCDMC,
#       minFreq = "10"
#     )
#   )
# })
# 
# test_that("archiveName", {
#   ## "output.zip" ----
#   expect_message(
#     export(
#       andromeda = andromedaCDMC,
#       outputPath = tempDirCDMC,
#       archiveName = "output.zip"
#     )
#   )
#   
#   expect_true(
#     file.exists(file.path(tempDirCDMC, "output.zip"))
#   )
#   
#   ## 3 ----
#   expect_error(
#     export(
#       andromeda = andromedaCDMC,
#       outputPath = tempDirCDMC,
#       archiveName = 3
#     )
#   )
# })
# 
# # Clean-up ----
# withr::defer({
#   Andromeda::close(andromedaCG)
#   Andromeda::close(andromedaCDMC)
# })
