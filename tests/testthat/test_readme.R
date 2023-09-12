# test_that("make readme works", {
#
#   dir <- fs::file_temp(pattern = "testproj")
#
#   create_local_study(dir = dir)
#
#   makeReadMe(projectPath = dir, open = FALSE)
#
#   expect_proj_file("README.md")
#   expect_snapshot(writeLines(read_utf8(proj_path("README.md"))),
#                   transform = scrub_testpkg)
#
#
#
# })
