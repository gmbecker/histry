histrstate = new.env()
histrstate$histropts = NULL

#' @export
histropts = function() histrstate$histropts


##' histry
##'
##' These functions return the set of successfully evaluated expressions (\code{histry})
##' and the \code{HistoryTracker} object (\code{histry_tracker}) for the current context.
##'
##' This means that when called during a knitr-based weaving process, the
##' expressions will be those previously evaluted durign that process, and the
##' tracker will be a KnitrHistoryTracker. When called outside of the weaving
##' context, the top-level-expression history and history tracker will be returned,
##' respectively.
##'
##' @return A list of R code expressions for \code{histry} and a HistoryTracker
##' object for \code{histry_tracker}
##' @rdname histry
##' @export

histry = function() histry_tracker()$exprs

##' @rdname histry
##' @export
histry_tracker = function() histropts()$history

##' @rdname histry
##' @export
histry_addinfo = function(...) histry_tracker()$addInfo(...)

##' @rdname histry
##' @export
histry_setinknitr = function(val) {
    state = histropts()
    state$inKnitr = val
    state
}
