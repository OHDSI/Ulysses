# These functions come directly from usethis

# template -----------------
find_template <- function(template_name, package = "Ulysses") {
  rlang::check_installed(package)
  path <- tryCatch(
    fs::path_package(
      package = package, "templates", template_name
    ),
    error = function(e) ""
  )
  if (identical(path, "")) {
    usethis::ui_stop("Could not find template {ui_value(template_name)} \\\n      in package {ui_value(package)}.")
  }
  path
}

render_template <- function(template, data = list(), package = "Ulysses") {
  template_path <- find_template(template, package = package)
  strsplit(whisker::whisker.render(read_utf8(template_path),
                                   data), "\n")[[1]]
}
#
# # Read/write -----------------------
read_utf8 <- function(path, n = -1L) {
  base::readLines(path, n = n, encoding = "UTF-8", warn = FALSE)
}

write_utf8 <- function(path, lines, append = FALSE, line_ending = NULL) {

  file_mode <- if (append) "ab" else "wb"
  con <- file(path, open = file_mode, encoding = "utf-8")
  withr::defer(close(con))

  line_ending <- ifelse(.Platform$OS.type == "windows", "\r\n", "\n")

  # convert embedded newlines
  lines <- gsub("\r?\n", line_ending, lines)
  base::writeLines(enc2utf8(lines), con, sep = line_ending, useBytes = TRUE)

  invisible(TRUE)
}
