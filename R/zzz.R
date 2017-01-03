initState = function() {
    knitrtracer(TRUE)
    if(!is.null(getOption("knitr.in.progress"))) {
        evaltracer(TRUE)
        addTaskCallback(function(...) {
            histry:::evaltracer(FALSE)
            FALSE
        })
    }
    

}


.onLoad= function(libname, pkgName, ...) {
  
    ns <- asNamespace(pkgName)
    
    delayedAssign("histropts",
                  { state$new(evalHistory = historyTracker("auto_tracker"),
                            knitrHistory = knitrTracker(),
                            inKnitr = !is.null(getOption("knitr.in.progress")))},
                  eval.env = ns, assign.env = ns)
    namespaceExport(ns, "histropts")
    tmp = histry::histropts
    initState()
#    if(!is.null(getOption("knitr.in.progress")))
#        stop("thingthangs")
    NULL
  
}
