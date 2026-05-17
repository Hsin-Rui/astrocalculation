#' load fonts
#'
#' @param libname default parameter
#' @param pkgname package name
#'
#' @import showtext
#' @importFrom sysfonts font_add
#' @importFrom swephR swe_set_ephe_path
#'

.onLoad <- function(libname, pkgname) {

  font_path <- system.file("fonts", "HamburgSymbols.ttf", package = pkgname)

  if (font_path != "") {

    sysfonts::font_add(family = "HamburgSymbols", regular = font_path)

    if (requireNamespace("showtext", quietly = TRUE)) {
      showtext::showtext_auto()
    }

  } else {
    warning("'HamburgSymbols.ttf' not found")
  }

  font_path <- system.file("fonts", "AstroDotBasic.ttf", package = pkgname)

  if (font_path != "") {

    sysfonts::font_add(family = "AstroDotBasic", regular = font_path)

    if (requireNamespace("showtext", quietly = TRUE)) {
      showtext::showtext_auto()
    }

  } else {
    warning("'AstroDotBasic.ttf' not found")
  }

  font_path <- system.file("fonts", "AstroParts.ttf", package = pkgname)

  if (font_path != "") {

    sysfonts::font_add(family = "AstroParts", regular = font_path)

    if (requireNamespace("showtext", quietly = TRUE)) {
      showtext::showtext_auto()
    }

  } else {
    warning("'AstroParts.ttf' not found")
  }


  se_path <- system.file("se_data", package = pkgname)

  if (se_path != "") {
    # Set the path globally for the session when the package loads
    swephR::swe_set_ephe_path(se_path)
  }

  invisible()
}
