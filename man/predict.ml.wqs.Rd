\name{predict.ml.wqs}
\alias{predict.ml.wqs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ SLMix package
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
predict.ml.wqs(ml.wqs.res, newdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ml.wqs.res}{
%%     ~~Describe \code{ml.wqs.res} here~~
}
  \item{newdata}{
%%     ~~Describe \code{newdata} here~~
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
function (ml.wqs.res, newdata) 
{
    w = ml.wqs.res$weights
    q.train.quantiles = ml.wqs.res$training.quantiles
    dimw = length(w)
    dimx = dim(newdata)[2]
    q = ml.quantile.fn(newdata[, 1:dimw], q.train.quantiles)
    newWQS <- as.numeric(q \%*\% w)
    newZ <- data.frame(WQS = newWQS)
    if (dimw < dimx) {
        newZ <- cbind(newZ, newdata[, (dimw + 1):dimx])
        names(newZ) <- cbind("WQS", names(newdata)[(dimw + 1):dimx])
    }
    predict.glm(ml.wqs.res$fit, newZ, envir = ml.wqs.res$env)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
