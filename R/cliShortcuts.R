notification <- function(txt) {
  cli::cat_bullet(
    txt,
    bullet = "info",
    bullet_col = "blue"
  )
  invisible(txt)
}

actionItem <- function(txt) {
  cli::cat_bullet(
    txt,
    bullet = "pointer",
    bullet_col = "yellow"
  )
  invisible(txt)
}


writeFileAndNotify <- function(x, repoPath, fileName) {

  filePath <- fs::path(repoPath, fileName)

  readr::write_lines(
    x = x,
    file = filePath
  )

  actionItem(glue::glue_col("Write {green {fileName}} to: {cyan {filePath}}"))
  invisible(filePath)
}
