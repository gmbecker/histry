##' @title histry session options environment
##' Environment where the default history tracker is set
##' Users should not directly set or retrieve things from this environemnt,
##' it is exported for use by packages that use histry.
##' @export

histropts = new.env()
histropts$history=NULL

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
