#' @import methods
#' @importFrom utils tail



#' @importFrom fastdigest fastdigest
ht_callback = function(expr, value, success, printed, tracker) {
    if(!success)
        return(TRUE)
    tracker$addInfo(expr, class(value), fastdigest(value))
    TRUE
}


ignorepattern = "^.ess"
    
#' @name HistoryTracker
#' @description These classes implement history tracking in various contexts
#' @title A reference class for tracking code history
#' @docType methods
#' @exportClass VirtHistoryTracker
#' @rdname HistoryTracker
#' @aliases VirtHistoryTracker-class

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
                            
                            
                        },
                        toggleTracking = function() stop("Not implemented on virtual class"),
                        clear = function() {
                            .self$exprs = NULL
                            .self$classes = character()
                            .self$hashes = character()
                        },
                        filter = function(syms = ls(ns, all.names=TRUE), ns = emptyenv()) {
                            

                            
                        }))


#' @rdname HistoryTracker
#' @docType methods
#' @exportClass HistoryTracker
#' @aliases HistoryTracker-class
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
                            id2 = id
                            while(id2 %in% exstcbs)
                                id2 = paste0(origid, fastdigest(id2))
                            
                            obj = callSuper( exprs = .exprs, classes = .classes,
                                            ...)
                            obj$id = id2
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
    if(!requireNamespace("evaluate") || !requireNamespace("knitr"))
        return(NULL)
    
    if(on) {
        ## evaltracer is always on now. Only reason to ever turn it off is to turn it back on immediately with different record value.
        ## The tracer for evaluate:::evaluate_call now checks if we're in knitr, so no need to make sure it's off
        ## in when we aren't.
        suppressMessages(trace(knitr::knit,
                               where = asNamespace("knitr"),
                               tracer = parse(text = 'if(!opts_knit$get("child")) histropts()$knitrHistory$clear(); histry_setinknitr(TRUE)'),
                               exit = quote(if(!opts_knit$get("child")) histry_setinknitr(FALSE)),
                               print=FALSE))
    } else {
        suppressMessages(untrace(knitr::knit,
                where = asNamespace("knitr")))
    }
}

obfu_colons = eval(parse(text = paste0("`", paste(rep(":", times=3),collapse = ""), "`")))
ev_call_txt = paste0("evaluate:", "::evaluate_call")
ev_call_expr = parse(text = ev_call_txt)
ev_call_untrace = parse(text = paste0("untrace(", ev_call_txt, ")"))

trc_code_txt = function(record) {
    paste(c(paste0("suppressMessages(trace(evaluate::", ":evaluate_call,"),
      "at = length(body(", paste0("evaluate::", ":evaluate_call"), ")),",
      "tracer = quote(if( histropts()$inKnitr && !is(ev$value, 'try-error')) {",
      "                                                     expr2 = deparse(expr)",
      "                                                      histry_addinfo(expr = expr2,",
      "                                                              class = class(ev$value))",
      if(record) { paste(
      "                                                      if(ev$visible)",
      "                                                    trackr::record(ev$value, symorpos = length(histry()))", sep="\n")
      } else { "" },
      "                                            }),",
      "                             print = FALSE))"
      ), collapse = "\n")
}

parseEval =  function(txt) eval(parse(text=txt))



##' evaltracer
##' 
##' @rdname tracers
##' @export
evaltracer = function(on=TRUE, record = FALSE) {
    if(on) {
        if(record) {
            if(!requireNamespace("trackr"))
                stop("Can't have record=TRUE without the trackr package installed")
        }

        ev_call = eval(ev_call_expr)
        ev_call_len = length(as.list(body(eval(ev_call_expr))))
        ## the things I do to make CRAN/R CMD check happy...
        
        parseEval(trc_code_txt(record))
        ## if(record) {
        ##     suppressMessages(trace(ev_call, ##evaluate:::evaluate_call, #"evaluate_call",
        ##                            ##where = asNamespace("evaluate"),
        ##                            at = ev_call_len,##list(c(28, 4,4)), ## FRAGILE!!!!!!!!!!!
        ##                            tracer = quote(if( histropts()$inKnitr && !is(ev$value, "try-error")) {
        ##                                               expr2 = deparse(expr)
        ##                                               histry_addinfo(expr = expr2,
        ##                                                              class = class(ev$value))
        ##                                               if(ev$visible)
        ##                                                   record(ev$value, symorpos = length(histry()))
        ##                                           }),
        ##                            print = FALSE
        ##                            )
        ##                      )
        ## } else {
        ##     suppressMessages(trace(ev_call, #obfu_colons("evaluate", "evaluate_call"), ##evaluate:::evaluate_call, #"evaluate_call",
        ##                            ##where = asNamespace("evaluate"),
        ##                            at = length(as.list(body(ev_call))), ##evaluate:::evaluate_call))),#list(c(28, 4,4)), ## FRAGILE!!!!!!!!!!!
        ##                            tracer = quote(if(histropts()$inKnitr && !is(ev$value, "try-error")) {
        ##                                               expr2 = deparse(expr)
        ##                                               histry_addinfo(expr = expr2,
        ##                                                              class = class(ev$value))
        ##                                           }),
        ##                            print = FALSE
        ##                            )
        ##                      )
        ## }            
    } else {
        ##suppressMessages(untrace(ev_call_expr))# obfu_colons("evaluate", "evaluate_call"))) #ev_call))##evaluate:::evaluate_call))
        suppressMessages(eval(ev_call_untrace))
    }
}



#' @name KnitrHistoryTracker
#' @docType methods
#' @exportClass KnitrHistoryTracker
#' @rdname HistoryTracker
#' @aliases KnitrHistoryTracker-class

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

