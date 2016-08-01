
#' @importFrom fastdigest fastdigest
ht_callback = function(expr, value, success, printed, tracker) {
    if(!success)
        return(TRUE)
    tracker$addInfo(expr, class(value), fastdigest(value))
    TRUE
}

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
                            if(is.null(.self$exprs))
                                .self$exprs = expr
                            else
                                .self$exprs = c(.self$exprs, expr)
                            .self$classes = c(.self$classes, class)
                        },
                        toggleTracking = function() stop("Not implemented on virtual class"),
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

kh_tracker = setRefClass("KnitrHistoryTracker",
                         contains = "VirtHistoryTracker",
                         fields = c(
                             oldhndlrs = "list",
                             valuehndlr = "function"
                         ),
                         methods = list(
                             initialize = function( .exprs = NULL,
                                                  .classes = character(), ...) {
                             if(!require("knitr"))
                                 stop("Can't use KnitrHistoryTracker without knitr, which failed to load")
                             obj = callSuper(exprs = .exprs,
                                             classes = .classes, ...)
                             obj$tracking=FALSE
                             obj$toggleTracking()
                             obj
                             
                             

                         },
                         toggleTracking = function() {
                             if(!.self$tracking) {
                                 force(.self)
                                 .self$oldhndlrs = knit_hooks$get()
                                 sourcefun = function(x, opts) {
                                     .self$addInfo(expr = x, class = character())
                                     .self$oldhndlrs$source(x,opts)
                                 }
                                 errfun = function(x, opts) {
                                     .self$removeLast()
                                     .self$oldhndlrs$error(x, opts)
                                 }
                                     
                                 #XXX deal with this later
                                 inlinefun = function(x) {
                                     .self$addInfo(expr = x, class=character())
                                     .self$oldhndlrs$inline(x)
                                 }
                                 knit_hooks$set(inline = inlinefun,
                                                error = errfun,
                                                source = sourcefun)
                                 .self$tracking = TRUE
                             } else {
                                 
                                 knit_hooks$merge(.self$oldhndlrs)
                                 .self$tracking = FALSE
                             }
                             .self$tracking
                         },
                         removeLast = function() {
                             if(!length(.self$exprs) > 0)
                                 return(TRUE)
                             .self$exprs = .self$exprs[[-length(.self$exprs)]]
                             .self$classes = .self$classes[[-length(.self$classes)]]
                             TRUE
                         }
                             
                         )
                         )
                            
                        


