\name{ml.quantilefind_f}
\alias{ml.quantilefind_f}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ml.quantilefind_f(data, v_n, q)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{v_n}{
%%     ~~Describe \code{v_n} here~~
}
  \item{q}{
%%     ~~Describe \code{q} here~~
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
function (data, v_n, q) 
{
    ncols = dim(data)[2]
    br = matrix(0, q + 1, length(v_n))
    for (i in 1:length(v_n)) {
        dat_num = as.numeric(unlist(data[, v_n[i]]))
        br[, i] = quantile(dat_num, probs = seq(0, 1, by = 1/q), 
            na.rm = TRUE)
        br[1, i] = -1e+32
        br[q + 1, i] = 1e+32
    }
    return(br)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
