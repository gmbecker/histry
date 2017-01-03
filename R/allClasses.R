
#' @importFrom fastdigest fastdigest
ht_callback = function(expr, value, success, printed, tracker) {
    if(!success)
        return(TRUE)
    tracker$addInfo(expr, class(value), fastdigest(value))
    TRUE
}


ignorepattern = "^.ess"
#' @name HistoryTracker
#' @title A reference class for tracking code history
#' @docType methods
#' @exportClass "HistoryTracker"

vh_tracker = setRefClass("VirtHistoryTracker",
                        fields = c(
                                   exprs = "ANY",
                                   classes = "character",
                                   hashes = "character",
                                   tracking = "logical"),
                        methods = list(
                            addInfo = function(expr, class, hash) {
                            if(is.character(expr) && length(expr) > 1)
                                expr = paste(expr, collapse="\n")

                            if(any(grepl(ignorepattern,expr)))
                                return(NULL)
                            if(is.null(.self$exprs))
                                .self$exprs = list(expr)
                            else
                                .self$exprs = c(.self$exprs, expr)
                            .self$classes = c(.self$classes, class)
                        },
                        toggleTracking = function() stop("Not implemented on virtual class"),
                        clear = function() {
                            .self$exprs = NULL
                            .self$classes = character()
                            .self$hashes = character()
                        },
                        filter = function(syms = ls(ns, all.names=TRUE), ns = emptyenv()) {
                            


                        }))



h_tracker = setRefClass("HistoryTracker",
                        contains = "VirtHistoryTracker",
                        fields = c(
                            id = "character"
                            ),
                        methods = list(
                            initialize = function(id = "hist_tracker", .exprs = NULL,
                                                  .classes = character(), ...) {
                            exstcbs = getTaskCallbackNames()
                               
                            if(id %in% exstcbs)
                                stop("A Task Callback of name ", id, "already exists")
                            obj = callSuper( exprs = .exprs, classes = .classes,
                                            ...)
                            obj$id = id
                            obj$tracking = FALSE
                            obj$toggleTracking()
                            obj
                            ## .self$id = id
                            ## .self$tracking = FALSE
                            ## .self$toggleTracking()
                            ## .self
                        },
                        toggleTracking = function() {
                            if(.self$tracking) {
                                removeTaskCallback(.self$id)
                                .self$tracking = FALSE
                            } else {
                                addTaskCallback(ht_callback, data = force(.self), name = id)
                                .self$tracking = TRUE
                            }
                            .self$tracking
                        })
                        )


##' @title knitr history tracking
##'
##' These functions turn on history tracking within the knitr weaving
##' process. \code{knitrtracer} does so from the R session, whereas
##' \code{hist_within_doc} is meant to be called within the first
##' code block of a knitr document.
##' 
##'
##'
##' @param on logical. Should tracking be turned on (TRUE) or off (FALSE)
##' @rdname knitrhist
##' @export
knitrtracer = function(on) {
    if(!require("evaluate") || !require("knitr"))
        return(NULL)
    
    if(on) {
        
        suppressMessages(trace("knit",
              where = asNamespace("knitr"),
              trace = quote(histry:::evaltracer(TRUE)),
              exit = quote(histry:::evaltracer(FALSE)),
              print=FALSE))
    } else {
        suppressMessages(untrace("knit",
                where = asNamespace("knitr")))
    }
}

##' @rdname knitrhist
##' @export

hist_within_doc = function() {
    ## if(is(histropts$history, "KnitrHistoryTracker")) {
    ##     histropts$history$clear()
    ## } else {
    ##     stop("must pre-create  a KnitrHistoryTracker via knitr_trackr() before calling hist_within_doc")
    ## }
    evaltracer(TRUE)
    addTaskCallback(function(...) {
        evaltracer(FALSE)
        FALSE
    })
}


##' evaltracer
##'
##' This function is exported due to vagaries of how tracing works. It should not
##' ever be called directly by the end user
##'@param on logical. Are we turning evaluation tracing on or off?
##' @export
evaltracer = function(on=TRUE) {
     if(on) {
        histropts$inKnitr = TRUE
        histropts$history$clear()
        suppressMessages(trace("evaluate_call",
              where = asNamespace("evaluate"),
              at = length(as.list(body(evaluate:::evaluate_call))),#list(c(28, 4,4)), ## FRAGILE!!!!!!!!!!!
              tracer = quote(if(!is(ev$value, "try-error")) {
                                 histropts$history$addInfo(expr = expr,
                                                   class = class(ev$value))
                             }),
              print = FALSE
              )
              )
    } else {
        histropts$inKnitr = FALSE
        suppressMessages(untrace("evaluate_call",
                                 where = asNamespace("evaluate"))
                         )
    }
}



#' @name KnitrHistoryTracker
#' @title A reference class for tracking code history in a knitr document
#' @docType methods
#' @exportClass "KnitrHistoryTracker"

kh_tracker = setRefClass("KnitrHistoryTracker",
                         contains = "VirtHistoryTracker",
                         methods = list(
                             initialize = function( .exprs = NULL,
                                                  .classes = character(), ...) {
                             if(!require("knitr"))
                                 stop("Can't use KnitrHistoryTracker without knitr, which failed to load")
                             obj = callSuper(exprs = .exprs,
                                             classes = .classes, ...)
                             obj$tracking=TRUE
                             obj
                         }## ,
                         ## toggleTracking = function() {
                                          
                         ##     if (.self$tracking) {
                         ##         evaltracer(FALSE)
                         ##     } else {
                         ##         force(.self)
                         ##         if(!identical(histropts$history, .self))
                         ##             trackHistory(.self)
                         ##         evaltracer(TRUE)
                         ##     }
                         ## }
                         )
                         )


setClassUnion("KnitrHistoryTrackerOrNULL", c("KnitrHistoryTracker", "NULL"))
state = setRefClass("HistryState",
                  fields = list(evalHistory = "HistoryTracker",
                                knitrHistory = "KnitrHistoryTrackerOrNULL",
                                inKnitr = "logical",
                                history = function(val) {
                      if(missing(val)) {
                          if(.self$inKnitr)
                              .self$knitrHistory
                          else
                              .self$evalHistory
                      } else {
                          stop("Can't set history tracker this way, set evalHistory or knitrHistory directly")
                      }
                  }))

