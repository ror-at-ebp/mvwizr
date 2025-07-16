#' Set CH locale globally
#'
#' Diese Funktion setzt (plattformabhängig) die korrekte locale (Spracheinstellung) für die Erzeugung der Plots.
#'
#' @param lang Sprache für locale. Entweder "de" oder "fr"
#'
#' @returns TRUE
#' @export
set_ch_locale <- function(lang = c("de", "fr")) {
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

  Sys.setlocale("LC_ALL", locale_string) # problematic, because it may or may not return a warning, depending on the platform and locale may be changed or ignored.

  invisible(TRUE)
}
