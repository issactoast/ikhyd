#' Cat citation message when the package is loading
#'
#' @name .onAttach 
#' @param libname libname
#' @param pkgname pakagename
#' @return start up message
.onAttach <- function(libname, pkgname) {
    packageStartupMessage(
        paste("To cite this package use citation('ikhyd') \n\n",
              "Text citation: \n\n",
              crayon::bold("Issac Lee (2020)."),
              crayon::italic("ikhyd"), ": I Know How You Drive - Collection of R functions to make telematics analysis easier.\n",
              "R package version", crayon::red("0.1.0."),
              crayon::blue("URL https://github.com/issactoast/ikhyd"))
    )
}