test_that("make news works", {

  dir <- fs::file_temp(pattern = "testproj")

  create_local_study(dir = dir)

  makeNews(projectPath = dir, open = FALSE)
  expect_proj_file("NEWS.md")
  expect_snapshot(writeLines(read_utf8(proj_path("NEWS.md"))),
                  transform = scrub_testpkg)


})
