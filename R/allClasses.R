
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
                            addInfo = function(expr, class, hash, envir = .GlobalEnv) {

                          
                            if(is.character(expr) && length(expr) > 1)
                                expr = paste(expr, collapse="\n")

                            if(any(grepl(ignorepattern,expr)))
                                return(NULL)
                            if(is.null(.self$exprs))
                                .self$exprs = list(expr)
                            else
                                .self$exprs = c(.self$exprs, expr)
                            .self$classes = c(.self$classes, class)

                            if(is.character(expr))
                                pexpr = parse(text = expr)
                            else if(is.expression(expr))
                                pexpr = expr[[1]]
                            else
                                pexpr = expr
                            ## if(CodeDepends:::isAssignment(pexpr)) {
                            ##     message("dealing with assignment")
                            ##     sym = as.character(pexpr[[2]])
                            ##     assign(sym, structure(get(sym, envir),
                            ##            fullcode = .self$exprs), envir = envir)
                            ## }
                            
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
                            origid = id
                            while(id %in% exstcbs)
                                id = paste0(origid, digest(id))
                            
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
##' These functions are exported due to the vagaries of how tracing
##' functions works in R. Knitr history support is now turned on when
##' the histry package is loaded; They should never be called directly
##' by an end user.
##' 
##'
##' @param on logical. Should tracking be turned on (TRUE) or off (FALSE)
##' @param record logical. Should visibly printed results within the weaving
##' process be recorded (if trackr is available).
##' @rdname tracers
##' @export
knitrtracer = function(on, record = FALSE) {
    if(!require("evaluate") || !require("knitr"))
        return(NULL)
    
    if(on) {
        ## evaltracer is always on now. Only reason to ever turn it off is to turn it back on immediately with different record value.
        ## The tracer for evaluate:::evaluate_call now checks if we're in knitr, so no need to make sure it's off
        ## in when we aren't.
        suppressMessages(trace(knitr::knit,
                               where = asNamespace("knitr"),
                               tracer = parse(text = 'if(!opts_knit$get("child")) histropts$knitrHistory$clear(); histropts$inKnitr = TRUE'),
                               exit = quote(if(!opts_knit$get("child")) histropts$inKnitr = FALSE),
                               print=FALSE))
    } else {
        suppressMessages(untrace(knitr::knit,
                where = asNamespace("knitr")))
    }
}

##' evaltracer
##'
##' @rdname tracers
##' @export
evaltracer = function(on=TRUE, record = FALSE) {
    if(on) {
        if(record)
            stopifnot(requireNamespace("trackr"))
        ## histropts$inKnitr = TRUE
        ## histropts$history$clear()
        if(record) {
            suppressMessages(trace(evaluate:::evaluate_call, #"evaluate_call",
                                   ##where = asNamespace("evaluate"),
                                   at = length(as.list(body(evaluate:::evaluate_call))),#list(c(28, 4,4)), ## FRAGILE!!!!!!!!!!!
                                   tracer = quote(if( histropts$inKnitr && !is(ev$value, "try-error")) {
                                                      expr2 = deparse(expr)
                                                      histropts$history$addInfo(expr = expr2,
                                                                                class = class(ev$value))
                                                      if(ev$visible)
                                                          record(ev$value, symorpos = length(histropts$history$exprs))
                                                  }),
                                   print = FALSE
                                   )
                             )
        } else {
            suppressMessages(trace(evaluate:::evaluate_call, #"evaluate_call",
                                   ##where = asNamespace("evaluate"),
                                   at = length(as.list(body(evaluate:::evaluate_call))),#list(c(28, 4,4)), ## FRAGILE!!!!!!!!!!!
                                   tracer = quote(if(histropts$inKnitr && !is(ev$value, "try-error")) {
                                                      expr2 = deparse(expr)
                                                      histropts$history$addInfo(expr = expr2,
                                                                                class = class(ev$value))
                                                  }),
                                   print = FALSE
                                   )
                             )
        }            
    } else {
    ##    histropts$inKnitr = FALSE
        ## suppressMessages(untrace("evaluate_call",
        ##                          where = asNamespace("evaluate"))
        ##                  )
        suppressMessages(untrace(evaluate:::evaluate_call))
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
                         }
                         )
                         )


setClassUnion("KnitrHistoryTrackerOrNULL", c("KnitrHistoryTracker", "NULL"))
state = setRefClass("HistryState",
                  fields = list(evalHistory = "HistoryTracker",
                                knitrHistory = "KnitrHistoryTrackerOrNULL",
                                inKnitr = "logical",
                                history = function(val) {
                      if(length(.self$inKnitr) == 0 || is.na(.self$inKnitr))
                          .self$inKnitr = getOption("knitr.in.progress", FALSE)
                      if(missing(val)) {
                          if(.self$inKnitr)
                              .self$knitrHistory
                          else
                              .self$evalHistory
                      } else {
                          stop("Can't set history tracker this way, set evalHistory or knitrHistory directly")
                      }
                  }))

