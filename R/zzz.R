initState = function() {
    knitrtracer(TRUE)
    evaltracer(TRUE)
    
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
    NULL
  
}

.onUnload = function(libname, pkgname, ...) {
    nms = getTaskCallbackNames()
    nms = grep("_tracker", nms, value=TRUE)
    lapply(nms, removeTaskCallback)



}
