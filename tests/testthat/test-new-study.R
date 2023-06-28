library(magrittr)
tmpDir <- fs::file_temp(pattern = "testproj") %>%
  fs::dir_create()

test_that("Check new study is built", {

  dirPath <- newOhdsiStudy(path = tmpDir,
                author = "Ulysses",
                type = "Characterization",
                open = FALSE,
                verbose = FALSE)

# check folders correct
  allFolders <- fs::dir_ls(dirPath, type = "directory") %>%
    basename()

  expect_equal(
    allFolders,
    c("analysis", "cohortsToCreate", "documentation", "extras", "logs", "results")
  )
  # check study yml created
  ymlFile <- fs::dir_ls(dirPath, type = "file", glob = "*.yml") %>%
    basename()

  expect_equal(
    ymlFile,
    "_study.yml"
  )
  # check rproj created
  RProjFile <- fs::dir_ls(dirPath, type = "file", glob = "*.Rproj") %>%
    basename()

  projFile <- basename(tmpDir) %>%
    fs::path(ext = "Rproj") %>%
    as.character()

  expect_equal(
    RProjFile,
    projFile
  )

})


