\name{predict.ml.gwqs}
\alias{predict.ml.gwqs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ SLMix package
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
predict.ml.gwqs(fit, mix_name, newdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{
%%     ~~Describe \code{fit} here~~
}
  \item{mix_name}{
%%     ~~Describe \code{mix_name} here~~
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
function (fit, mix_name, newdata) 
{
    qname = paste(mix_name, "q", sep = "_")
    w = fit$final_weights$mean_weight
    q.train.quantiles = fit$training.quantiles
    dimw = length(qname)
    dimx = dim(newdata)[2]
    q = ml.quantile_f(newdata, mix_name, q.train.quantiles)[, 
        qname, drop = FALSE]
    newWQS <- as.numeric(as.matrix(q) \%*\% w)
    newZ <- data.frame(wqs = newWQS)
    if (dimw < dimx) {
        newZ <- cbind(newZ, newdata[, (dimw + 1):dimx])
        names(newZ) <- cbind("wqs", names(newdata)[(dimw + 1):dimx])
    }
    predict.glm(fit$fit, newZ, envir = fit$env, type = "response")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
