
#' @title Construct and activate a HistoryTracker object
#' @param id character. The id of the taskback
#' @return A HistoryTracker object
#' @rdname tracker-constructors
#' @export
historyTracker = function(id = "history_tracker") {
    h_tracker$new(id = id)
}

#' @rdname tracker-constructors
#' @aliases knitr_tracker
#' @export
knitrTracker = function() {
    kh_tracker$new()
}

setMethod("show", "VirtHistoryTracker",
          function(object) {

    if(is.language(object$exprs) && length(object$exprs) >0)
        msg = deparse(object$exprs)
    else if (length(object$exprs) > 1)
        msg = paste(tail(object$exprs,5), collapse="\n")
    else
        msg = "No messages in history record"
    cat("A history tracker of class", class(object), "\nRecent expressions:",
        msg, "\n(For full history do obj$exprs)\n")
})

