library(testthat)
library(R6)
library(shiny)
library(TreatmentPatterns)

test_that("InteracivePlots", {
  interactivePlots <- InteractivePlots$new("app")
  expect_true(is.R6(interactivePlots))
})

test_that("UI", {
  interactivePlots <- InteractivePlots$new("app")

  expect_s3_class(interactivePlots$uiBody(), "shiny.tag")
  expect_s3_class(interactivePlots$uiMenu(), "shiny.tag")
})


test_that("server", {
  moduleInteractivePlots <- function(id, inputHandler, interactivePlots) {
    shiny::moduleServer(id, function(input, output, session) {
      inputHandler$setDataPath(input = input, path = NULL)
      
      path <- system.file(package = "TreatmentPatterns", "DummyOutput", "output.zip")
      
      session$setInputs(
        uploadField = list(
          datapath = path,
          name = "output.zip"
        )
      )
      
      inputHandler$server(input, output, session)
      interactivePlots$server(input, output, session, inputHandler)
      
      session$setInputs(
        sexOption = "all",
        ageOption = "all",
        indexYearOption = "all",
        noneOption = TRUE
      )
      
      htmlOutput(shiny::NS(id, "sunburst"))
      htmlOutput(shiny::NS(id, "sankey"))
    })
  }
  
  shiny::testServer(
    app = moduleInteractivePlots,
    args = list(
      inputHandler = InputHandler$new("app"),
      interactivePlots = InteractivePlots$new("app")), {
        expect_identical(names(output$sunburst), c("html", "deps"))
        expect_identical(names(output$sankey), c("html", "deps"))
    }
  )
})
