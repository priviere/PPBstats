# to print welcome message when loading the package
.onLoad <- function(...) {
packageStartupMessage("
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Thanks for using PPBstats :-)
!!! For a tutorial, type vignette(\"PPBstats\")
!!! To cite PPBstats, type citation(\"PPBstats\")
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
"
)
}
