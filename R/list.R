# allows directly assigning list outputs to variables
# http://stackoverflow.com/questions/1826519/function-returning-more-than-one-value
#' @export
list <- structure(NA,class="result")

#' @export
"[<-.result" <- function(x,...,value) {
    args <- as.list(match.call())
    args <- args[-c(1:2,length(args))]
    length(value) <- length(args)
    for(i in seq(along=args)) {
        a <- args[[i]]
        if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
    }
    x
}
