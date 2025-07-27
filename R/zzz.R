# Startup message - show package version

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(pkgname, " geladen: v", packageVersion(pkgname))
}

# Setup mutable package environment to manage state

the <- new.env(parent = emptyenv())
the$mvwizr_lang <- "de_CH"

get_lang <- function() {
  the$mvwizr_lang
}
set_lang <- function(newlang) {
  old <- the$mvwizr_lang
  the$mvwizr_lang <- newlang
  invisible(old)
}
