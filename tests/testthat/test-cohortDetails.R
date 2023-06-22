test_that("makeCohortDetails works", {

  dir <- fs::file_temp(pattern = "testproj")

  create_local_study(dir = dir)

  makeCohortDetails()

  expected_path <- "cohortsToCreate/CohortDetails.md"
  expect_proj_file(expected_path)
  expect_snapshot(writeLines(read_utf8(proj_path(expected_path))),
                  transform = scrub_testpkg)



})
