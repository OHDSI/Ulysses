# set the full string vector
.setCharacter <- function(private, key, value) {
  checkmate::assert_character(x = value, min.chars = 1, null.ok = FALSE)
  private[[key]] <- value
  invisible(private)
}

# set the full integer vector
.setInteger <- function(private, key, value) {
  checkmate::assert_integer(x = value, null.ok = FALSE)
  private[[key]] <- value
  invisible(private)
}

.setNumeric <- function(private, key, value) {
  checkmate::assert_numeric(x = value, null.ok = FALSE)
  private[[key]] <- value
  invisible(private)
}


.setClass <- function(private, key, value, class, nullable = FALSE) {
  checkmate::assert_class(x = value, classes = class, null.ok = nullable)
  private[[key]] <- value
  invisible(private)
}

#set a single item in string vector
.setString <- function(private, key, value, idx = NULL) {
  checkmate::assert_string(x = value, na.ok = FALSE, min.chars = 1, null.ok = FALSE)
  if (!is.null(idx)) {
    private[[key]][idx] <- value
  } else {
    private[[key]] <- value
  }
  invisible(private)
}

.setTimeStamp <- function(private, key, value) {
  checkmate::assert_posixct(x = value)
  private[[key]] <- value
  invisible(private)
}

#
# .setActiveString <- function(private, key, value, idx) {
#   # return the value if nothing added
#   if(missing(value)) {
#     vv <- private[[key]][idx]
#     return(vv)
#   }
#   # replace the value if value assigned
#   .setString(private = private, key = key, value = value, idx = idx)
# }



.setActiveCharacter <- function(private, key, value) {
  # return the value if nothing added
  if(missing(value)) {
    vv <- private[[key]]
    return(vv)
  }
  # replace the value if value assigned
  .setCharacter(private = private, key = key, value = value)
}



.setActiveInteger <- function(private, key, value) {
  # return the value if nothing added
  if(missing(value)) {
    vv <- private[[key]]
    return(vv)
  }
  # replace the value if value assigned
  .setInteger(private = private, key = key, value = value)
}


# .setGeneratedCohorts <- function(private, key = ".generatedCohorts", value) {
#   # return the value if nothing added
#   if(missing(value)) {
#     vv <- private[[key]]
#     if (is.null(vv)) {
#       invisible(NULL)
#     } else {
#       genCohorts <- vv$showTable()
#       return(genCohorts)
#     }
#
#   }
#   # replace the value if value assigned
#   .setClass(private = private, key = key, class = "GeneratedCohorts", value = value)
# }
