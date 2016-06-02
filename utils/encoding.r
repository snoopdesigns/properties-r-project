source_utf8 <- function(f) {
  l <- readLines(f, encoding="UTF-8")
  eval(parse(text=l),envir=.GlobalEnv)
}