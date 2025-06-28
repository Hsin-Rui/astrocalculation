#' load fonts
#'
#' @param libname default parameter
#' @param pkgname package name
#'
#' @import showtext
#' @importFrom sysfonts font_add
#'

.onLoad <- function(libname, pkgname) {

  library(showtext)

  font_path <- system.file("fonts", "AstroGadget.ttf", package = pkgname)

  if (font_path != "") {

    sysfonts::font_add(family = "AstroGadget", regular = font_path)
    showtext_auto()

  } else {
    warning("'AstroGadget.ttf' not found")
  }

  font_path <- system.file("fonts", "HamburgSymbols.ttf", package = pkgname)

  if (font_path != "") {

    sysfonts::font_add(family = "HamburgSymbols", regular = font_path)
    showtext_auto()

  } else {
    warning("'HamburgSymbols.ttf' not found")
  }

  font_path <- system.file("fonts", "AstroDotBasic.ttf", package = pkgname)

  if (font_path != "") {

    sysfonts::font_add(family = "AstroDotBasic", regular = font_path)
    showtext_auto()

  } else {
    warning("'AstroDotBasic.ttf' not found")
  }

  invisible()
}
