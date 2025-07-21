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
