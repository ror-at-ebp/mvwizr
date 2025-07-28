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

get_ch_locale <- function(lang = c("de", "fr")) {
  lang <- match.arg(lang)
  platform <- tolower(Sys.info()[["sysname"]])

  locale_string <- switch(platform,
                          "windows" = switch(lang,
                                             de = "German_Switzerland.utf8",
                                             fr = "French_Switzerland.utf8"
                          ),
                          "linux" = switch(lang,
                                           de = "de_CH.utf8",
                                           fr = "fr_CH.utf8"
                          ),
                          # darwin is the macOS kernel
                          "darwin" = switch(lang,
                                            de = "de_CH",
                                            fr = "fr_CH"
                          ),
                          # Fallback (linux-like)
                          switch(lang,
                                 de = "de_CH.utf8",
                                 fr = "fr_CH.utf8"
                          )
  )

  invisible(locale_string)
}
