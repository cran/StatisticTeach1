.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\nTo start with the Shiny web interface, please run:\n",
    "  runStatisticTeach1()\n\n",
    "To cite this package, run: citation('StatisticTeach1')\n"
  )
}
