.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    "phrasenets-assets",
    system.file("app/assets", package = "phrasenets")
  )
}
