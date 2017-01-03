

##' @title is history tracking on
##' @return logical indicating whether history is currently being tracked.
##' @export
trackingHistory = function() {
    is(histropts$history, "VirtHistoryTracker") && histropts$history$tracking
}

##' @title Automatically track history within an R session
##' @param tracker VirtHistoryTracker subclass or NULL. For NULL, if
##' a default tracker is set, toggle tracking with default tracker, otherwise
##' set a default tracker. For a VirtHistoryTracker (subclass) set as
##' the default tracker and turn it on.
##' @export
trackHistory = function( tracker = NULL) {
    if(!missing(tracker) && is(tracker, "VirtHistoryTracker")) {
        histropts$history = tracker
        if(!trackingHistory())
            tracker$toggleTracking()
        message("Tracking session history. To turn off tracking call trackHistory().")
    } else if (is.null(tracker) && is.null(histropts$history)) {
        message("Starting automatic session history tracking. To turn off tracking call trackHistory().")
        histropts$history = history_tracker("auto_history")
    } else {

        if(trackingHistory()) {
            message("Suspending automatic history tracking. To turn it back on call trackHistory() again.")
        } else {
            message("Reinstating history tracking with existing default tracker.")
        }
        histropts$history$toggleTracking()
    }
    invisible(NULL)

}


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
histry_tracker = function() histropts$history
