# to print welcome message when loading the package
.onAttach <- function(libname, pkgname) {
packageStartupMessage("
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Thanks for using PPBstats :-)

A website dedicated to PPBstats and a exhaustive tutorial 
to collaborate and use the package can be found here : 
https://priviere.github.io/PPBstats_web_site

To cite PPBstats, type citation(\"PPBstats\")
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
"
)
}
