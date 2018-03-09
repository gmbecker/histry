saveHistry = function(trackr = histry_tracker(), append = TRUE, file = "./.histry.rds") {
    out = hData(trackr)
    
    if(append && file.exists(file)) {
        old = readRDS(file)
        out = combineHistry(out, old, before=TRUE)
    }
    saveRDS(out, file = file)
}

loadHistry = function(file = "./.histry.rds", trackr = histry_tracker()) {
    trackr$importHistory(impdata = file)
}


##' @title Combine History data from two sources/records
##' @description Combines the underlying history data from two
##'     representations.
##' @return The same class of object as \code{x}. I.e., if x is a
##'     HistoryTracker object, a HistoryTracker (of the same subclass)
##'     is returned. If X is a \code{HistoryData} object, that class
##'     of object is returned.
##' @param x An object representing captured history
##' @param y An object representing captured history
##' @param before logical. Should the data from y be placed before the
##'     data from x in the combined history. Defaults to \code{FALSE}
##' @note HistoryTracker objects are reference classes, meaning that
##'     if \code{x} is a HistoryTracker the changes will be reflected
##'     in all variables representing that trackr.
##' @docType methods
##' @export
##' @rdname combineHistry
setGeneric("combineHistry", function(x,y, before = FALSE) standardGeneric("combineHistry"))

.combineHist = function(x, y, before = FALSE) {
    if(before) {
        frst  = hData(y)
        scnd = hData(x)
    } else {
        frst = hData(x)
        scnd = hData(y)
    }

    ret = x ## ensure endomorphism
    exprs(ret) = c(exprs(frst), exprs(scnd))
    ret_classes(ret) = c(ret_classes(frst), ret_classes(scnd))
    hashes(ret) = c(hashes(frst), hashes(scnd))
    ret
}
##' @rdname combineHistry
##' @export
##' @aliases combineHistry,HistoryData-method
setMethod("combineHistry", "HistoryData", .combineHist)

##' @rdname combineHistry
##' @export
##' @aliases combineHistry,HistoryData-method
setMethod("combineHistry", "VirtHistoryTracker", .combineHist)
    
