##' @import CodeDepends roprov
NULL

.alloutputs = function(x) {
    stopifnot(is(x, "ScriptNodeInfo"))
    c(x@outputs, x@updates)
}

##' @title Generate ProvStoreDF from histry
##' @description Generates a tabular 'provenance store' from the history
##' of evaluated expressions captured by histry
##' @return a ProvStoreDF object
##' @export
histryProvDF = function() {
    
    hdata = histry_tracker()$hdata
    outhashes = hdata@hashes
    code = sapply(hdata@exprs, function(x) paste(deparse(x), collapse = "\n"))
    scr = readScript(txt = code, type="R")
    codeinfo = getInputs(scr)
    outputs = lapply(codeinfo, .alloutputs)
    nout = sapply(outputs, length)
    if(max(nout) > 1)
        stop("One or more expressions captured by histry have >1 output. Not currently supported.")
    ## because of above, equivalent to == 1
    hasout = nout > 0
    ## make it not a list
    outputs = unlist(outputs[hasout])
    outhashes = outhashes[hasout]
    codeinfo = codeinfo[hasout]
    ## should always be a single output var at this point
    inpvars = lapply(codeinfo, function(x) x@inputs)
    rows = list(length(outputs))
    posinds = seq(along = outputs)
    for(i in rev(posinds)){
        if(length(inpvars[[i]]) > 0) {
            previnds = posinds < i
            rows[[i]] = provFromHistory(outputs[i], outhashes[i], invars = inpvars[[i]],
                                  prevouts = outputs[previnds],
                                  prevouthashes = outhashes[previnds],
                                  code = deparse(codeinfo[[i]]@code))
        } else {
            rows[[i]] = makeProvStore(outvarhashes = outhashes[i], code = deparse(codeinfo[[i]]@code),
                                   outvars = outputs[i])
        }
    }
    do.call(rbind, rows)
}



.fixcode = function(code) paste(code, collapse="\n")
