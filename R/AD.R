## system("R CMD SHLIB ../src/ADinf.c ../src/AnDarl.c")
##dyn.load("../src/ADinf.so")

##' Anderson-Darling test of uniformity
##' 
##' @param x a numeric vector (preferably longer than 7 elements)
##' @return an object of class \code{htest} containing the test statistic and p-value for a test against a null hypothesis of uniformity
##' @export
##' @references George Marsaglia and John Marsaglia (2004) "Evaluating the Anderson-Darling distribution", \emph{J. Statistical Software} vol. 9, issue i02: \url{http://www.jstatsoft.org/v09/i02/paper}
##' @examples
##' u <- c(.0392,.0884,.260,.310,.454,.644,.797,.813,.921,.960)
##' (t1 <- ADtest(u))
##' AD_pval(length(u),t1$statistic)

ADtest <- function(x) {
    n <- length(x)
    if (n<7)
        warning("Not well-suited for n<7 (accuracy could drop to 3 digits)")
    ans <- .C("ADtest",as.integer(n),
              as.double(sort(x)),
              pval=double(1),
              stat=double(1))
    RVAL <- list(statistic = setNames(ans$stat,"AD"),
                 parameter = NULL, p.value = ans$pval,
                 method="Anderson-Darling",
                 data.name = deparse(substitute(x)))
    class(RVAL) <- "htest"
    RVAL
}

##' Anderson-Darling p-value
##' 
##' @rdname ADtest
##' @param n length of original sequence
##' @param z Anderson-Darling test statistic
##' @return Anderson-Darling p-value
##' @export
AD_pval <- function(n,z) {
    ans <- .C("ADp",as.integer(n),as.double(z),pval=double(1))
    ans$pval
}
