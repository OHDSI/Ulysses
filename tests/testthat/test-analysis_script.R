test_that("make analysis Script works", {

  dir <- fs::file_temp(pattern = "testproj")

  create_local_study(dir = dir)

  makeAnalysisScript(scriptName = "buildCohorts", projectPath = dir, open = FALSE)

  expect_proj_file("analysis/studyTasks/01_buildCohorts.R")
  expect_snapshot(writeLines(read_utf8(proj_path("analysis/studyTasks/01_buildCohorts.R"))),
                  transform = scrub_testpkg)



})
