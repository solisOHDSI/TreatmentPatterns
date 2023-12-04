library(TreatmentPatterns)
library(testthat)
library(R6)

test_that("InputHandler", {
  ## new() / initialize() ----
  inputHandler <- InputHandler$new("app")
  expect_true(is.R6(inputHandler))
  
  ## validate() ----
  expect_true(is.R6(inputHandler$validate()))
  
  ## namespace ----
  expect_identical(inputHandler$namespace, "app")
  
  ## reactiveValues ----
  expect_s3_class(inputHandler$reactiveValues, "reactivevalues")
})

test_that("InputHandler: uiMenu()", {
  inputHandler <- InputHandler$new("app")
  expect_s3_class(inputHandler$uiMenu(), "shiny.tag")
})

test_that("InputHandler: uiBody()", {
  inputHandler <- InputHandler$new("app")
  expect_s3_class(inputHandler$uiBody(), "shiny.tag")
})

test_that("InputHandler: uiDatabaseSelector()", {
  inputHandler <- InputHandler$new("app")
  expect_s3_class(inputHandler$uiDatabaseSelector(), "shiny.tag")
})

test_that("InputHandler: server()", {
  moduleInputHandler <- function(id, inputHandler) {
    shiny::moduleServer(id, function(input, output, session) {
      inputHandler$setDataPath(tag = "uploadField", input = input, path = NULL)
      inputHandler$server(input, output, session)
    })
  }
  
  shiny::testServer(
    app = moduleInputHandler,
    args = list(inputHandler = InputHandler$new("app")), {
      path <- system.file(package = "TreatmentPatterns", "DummyOutput", "output.zip")
      
      session$setInputs(
        uploadField = list(
          datapath = path,
          name = "output.zip"
        )
      )
      
      # Files
      expect_s3_class(inputHandler$reactiveValues$treatmentPathways, "data.frame")
      expect_s3_class(inputHandler$reactiveValues$countsAge, "data.frame")
      expect_s3_class(inputHandler$reactiveValues$countsSex, "data.frame")
      expect_s3_class(inputHandler$reactiveValues$countsYear, "data.frame")
      expect_s3_class(inputHandler$reactiveValues$summaryStatsTherapyDuration, "data.frame")
      expect_s3_class(inputHandler$reactiveValues$metadata, "data.frame")
      
      # Fields
      expect_identical(inputHandler$reactiveValues$dataPath, path)
      expect_identical(inputHandler$reactiveValues$dbNames, basename(path))
  })
})

test_that("InputHandler: setDataPath()", {
  expect_true(is.R6(inputHandler$setDataPath(path = "some/data/path/to/a/file.zip")))
})
