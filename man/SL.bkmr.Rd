\name{SL.bkmr}
\alias{SL.bkmr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ SLMix package
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SL.bkmr(Y, X, newX, family, obsWeights, id, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{newX}{
%%     ~~Describe \code{newX} here~~
}
  \item{family}{
%%     ~~Describe \code{family} here~~
}
  \item{obsWeights}{
%%     ~~Describe \code{obsWeights} here~~
}
  \item{id}{
%%     ~~Describe \code{id} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Y, X, newX, family, obsWeights, id, ...) 
{
    if (is.matrix(X)) {
        X = as.data.frame(X)
    }
    if (is.matrix(newX)) {
        newX = as.data.frame(newX)
    }
    if (family$family == "gaussian") {
        fit = kmbayes(y = Y, Z = X, iter = 1000, family = "gaussian", 
            verbose = FALSE)
    }
    if (family$family == "binomial") {
        fit = kmbayes(y = Y, Z = X, iter = 1000, family = "binomial", 
            verbose = FALSE)
    }
    pMat <- SamplePred(fit, Znew = newX, Xnew = cbind(0), type = "response")
    pred <- as.numeric(apply(pMat, 2, mean))
    class(fit) <- c("SL.bkmr")
    out <- list(pred = pred, fit = fit)
    return(out)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
