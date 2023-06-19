library(magrittr)
tmpDir <- fs::file_temp(pattern = "testproj") %>%
  fs::dir_create()

test_that("Check new study is built", {

  dirPath <- newOhdsiStudy(path = tmpDir,
                author = "Ulysses",
                type = "Characterization",
                open = FALSE)

  allFiles <- fs::dir_ls(dirPath, type = "file") %>%
    basename()
  allFolders <- fs::dir_ls(dirPath, type = "directory") %>%
    basename()

  expect_equal(
    allFolders,
    c("analysis", "cohortsToCreate", "documentation", "extras", "logs", "results")
  )

  expect_equal(
    allFiles,
    c("test.Rproj", "_study.yml")
  )

})


