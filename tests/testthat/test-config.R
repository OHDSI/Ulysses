test_that("makeConfig works", {

  dir <- fs::file_temp(pattern = "testproj")

  create_local_study(dir = dir)

  makeConfig(block = "test", database = "synpuf_110k")

  expect_proj_file("config.yml")
  expect_snapshot(writeLines(read_utf8(proj_path("config.yml"))),
                  transform = scrub_testpkg)



})
