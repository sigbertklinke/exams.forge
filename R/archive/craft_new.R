# \item{\code{binom}} binomial distribution, parameters: \code{size}, \code{prob}
craft("binom", append=FALSE,
      function(size=30, prob=0.5) { 
        package     <- "stats"
        discrete    <- TRUE
        expectation <- size*prob
        mode        <- floor(size*prob)
        variance    <- size*prob*(1-prob)
        skewness    <- (1-2*prob)/sqrt(variance)
        excess      <- (1-6*prob)/variance
        toLatex     <- "B(n=`r size`, p=`r prob`)"
        ddist       <- local({ size=size; prob=prob; function(x) { dbinom(x, size=size, prob=prob) }})
        pdist       <- local({ size=size; prob=prob; function(q) { pbinom(q, size=size, prob=prob) }})
        qdist       <- local({ size=size; prob=prob; function(p) { qbinom(p, size=size, prob=prob) }})
        rdist       <- local({ size=size; prob=prob; function(n) { rbinom(n, size=size, prob=prob) }})
        environment()
      }
)

# \item{\code{hyper}} hypergeometric distribution, parameters: \code{m}, \code{n}, \code{k}
# N=m+n, K=m, n=k
craft("hyper", append=FALSE,
      function(m=15, n=15, k=5) {
        package <- "stats"
        discrete <- TRUE
        expectation <- k*m/(n+m)
        mode        <- floor((k+1)*(m+1)/(m+n+2))
        variance    <- k*m*n*(m+n-k)/(m+n)^2/(m+n-1)
        stddev      <- sqrt(variance)
        median      <- qhyper(0.5, m=m, n=n, k=k)
        toLatex     <- "Hyp(m=`r m`, n=`r n`, k=`r k`)"
        ddist       <- local({ m=m; n=n; k=k; function(x) { dhyper(x, m=m, n=n, k=k) }})
        pdist       <- local({ m=m; n=n; k=k; function(q) { phyper(q, m=m, n=n, k=k) }})
        qdist       <- local({ m=m; n=n; k=k; function(p) { qhyper(p, m=m, n=n, k=k) }})
        rdist       <- local({ m=m; n=n; k=k; function(n) { rhyper(n, m=m, n=n, k=k) }})
        environment()
      } 
)

craft("hyper1", append=FALSE,
      function(M=15, N=30, n=5) { 
        package     <- "stats"
        discrete    <- TRUE
        expectation <- M/N*n
        mode        <- floor((n+1)*(M+1)/(N+2))
        variance    <- n*M/N*(N-M)/N*(N-n)/(N-1)
        median      <- qhyper(0.5, m=M, n=N-M, k=n)
        toLatex     <- "Hyp(N=`r N`, M=`r M`, n=`r n`)"
        ddist       <- local({ m=M; n=N-M; k=n; function(x) { dhyper(x, m=m, n=n, k=k) }})
        pdist       <- local({ m=M; n=N-M; k=n; function(q) { phyper(q, m=m, n=n, k=k) }})
        qdist       <- local({ m=M; n=N-M; k=n; function(p) { qhyper(p, m=m, n=n, k=k) }})
        rdist       <- local({ m=M; n=N-m; k=n; function(n) { rhyper(n, m=m, n=n, k=k) }})
        environment()
      }
)


# \item{\code{geom}} geometric distribution, parameters: \code{prob}
craft(name="geom", append=FALSE,
      function (prob=0.5) {
        package     <- "stats"
        discrete    <- FALSE
        expectation <- 1/prob
        median      <- pgeom(0.5, prob=prob)
        variance    <- (1-prob)/prob^2
        toLatex     <- "G(p=`r prob`)"
        ddist       <- local({ prob=prob; function(x) { dgeom(x, prob=prob) }})
        pdist       <- local({ prob=prob; function(q) { pgeom(q, prob=prob) }})
        qdist       <- local({ prob=prob; function(p) { qgeom(p, prob=prob) }})
        rdist       <- local({ prob=prob; function(n) { rgeom(n, prob=prob) }})
        environment()
      }
)


craft ("norm", append=FALSE, 
       FUN=function(mean=0, sd=1, ...) {
         package     <- "stats"
         discrete    <- FALSE
         expectation <- mean
         mode        <- mean
         variance    <- sd^2
         mad         <- sd*qnorm(0.75)
         skewness    <- 0 
         excess      <- 0
         entropy     <- (1+log(2*pi*sd^2))/2
         toLatex     <- "N(\\mu=`r mean`, \\sigma^2=`r sd^2`)"
         toMath      <- bquote(paste('N(', mu, '=', .(mean), ',', sigma^2, '=', .(variance), ')'))
         ddist       <- local({ mean=mean; sd=sd; function(x) { dnorm(x, mean=mean, sd=sd) }})
         pdist       <- local({ mean=mean; sd=sd; function(q) { pnorm(q, mean=mean, sd=sd) }})
         qdist       <- local({ mean=mean; sd=sd; function(p) { qnorm(p, mean=mean, sd=sd) }})
         rdist       <- local({ mean=mean; sd=sd; function(n) { rnorm(n, mean=mean, sd=sd) }})
         environment()
       }
)



# \item{\code{pois}} Poisson distribution, parameters: \code{lambda}
craft(name="pois", append=FALSE,
      function(lambda=0.5) {
        package     <- "stats"
        discrete    <- FALSE
        expectation <- 1/lambda
        median      <- ppois(0.5, lambda=lambda)
        variance    <- (1-lambda)/lambda^2
        stddev      <- sqrt(variance)
        toLatex     <- "Po(\lambda=`r lambda`)"
        toString    <- "Po(lambda=`r lambda`)"
        toMath      <- bquote(paste("Po(", lambda, '=', .(lambda), ")"))
        ddist       <- local({ lambda=lambda; function(x) { dpois(x, lambda=lambda) }})
        pdist       <- local({ lambda=lambda; function(q) { ppois(q, lambda=lambda) }})
        qdist       <- local({ lambda=lambda; function(p) { qpois(p, lambda=lambda) }})
        rdist       <- local({ lambda=lambda; function(n) { rpois(n, lambda=lambda) }})
        environment() 
      }
)

# \item{\code{unif}} continuous uniform  distribution, parameters: \code{min}, \code{max}
craft(name="unif",  append=FALSE,
      function(min=0, max=1) {
        package     <- "stats"
        discrete    <- FALSE
        expectation <- (max+min)/2
        median      <- punif(0.5, min=min, max=max)
        variance    <- (max-min)^2/12
        stddev      <- sqrt(variance)
        toLatex     <- "U(`r min`, `r max`)"
        toString    <- "U(`r min`, `r max`)"
        toMath      <- bquote(paste("U(", .(min), ", ", .(max), ")"))
        ddist       <- local({ min=min; max=max; function(x) { dgeom(x, min=min, max=max) }})
        pdist       <- local({ min=min; max=max; function(q) { pgeom(q, min=min, max=max) }})
        qdist       <- local({ min=min; max=max; function(p) { qgeom(p, min=min, max=max) }})
        rdist       <- local({ min=min; max=max; function(n) { rgeom(n, min=min, max=max) }})
        environment() 
      }
)

# \item{\code{cat}} categorical distribution.
craft("cat",  append=FALSE,
      function(prob=rep(1/6, 6), labels=1:6) {
        package     <- "extraDistr"
        discrete    <- TRUE
        expectation <- sum(seq_along(labels)*prob)/sum(prob)
        median      <- pcat(0.5, prob=prob)
        variance    <- sum(seq_along(labels)^2*prob)/sum(prob)-expectation^2
        stddev      <- sqrt(variance)
        toLatex     <- "Cat(`r paste0(lfrac(prob), collapse=', ')`)"
        toString    <- "Cat(`r paste0(lfrac(prob), collapse=', ')`)"
        toMath      <- "Cat(`r paste0(lfrac(prob), collapse=', ')`)"
        ddist       <- local({ prob=prob; labels=labels; function(x) { dcat(x, prob=prob) }})
        pdist       <- local({ prob=prob; labels=labels; function(q) { pcat(q, prob=prob) }})
        qdist       <- local({ prob=prob; labels=labels; function(p) { qcat(p, prob=prob) }})
        rdist       <- local({ prob=prob; labels=labels; function(n) { rcat(n, prob=prob, labels=labels) }})
        environment() 
      }
)

# \item{\code{dunif}} discrete uniform  distribution, parameters: \code{min}, \code{max}
craft("dunif",  append=FALSE,
      function(min=1, max=6) {
        package     <- "extraDistr"
        discrete    <- TRUE
        expectation <- sum((min:max)/(max-min+1))
        median      <- pdunif(0.5, min=min, max=max)
        variance    <- sum((min:max)^2/(max-min+1))-expectation^2
        stddev      <- sqrt(variance)
        toLatex     <- "U_d(`r min`, `r max`)"
        toString    <- "U_d(`r min`, `r max`)"
        toMath      <- "U_d(`r min`, `r max`)"
        ddist       <- local({ min=min; max=max; function(x) { ddunif(x, min=min, max=max) }})
        pdist       <- local({ min=min; max=max; function(q) { pdunif(q, min=min, max=max) }})
        qdist       <- local({ min=min; max=max; function(p) { qdunif(p, min=min, max=max) }})
        rdist       <- local({ min=min; max=max; function(n) { rdunif(n, min=min, max=max) }})
        environment() 
      }
)

# \item{\code{dunif2}} continuous uniform  distribution, parameters: \code{min}, \code{max}
craft("dunif2", append=FALSE,
      function(min=1, max=6) {
        package     <- "exams.forge"
        discrete    <- TRUE
        prob        <-  {v <- outer(min:max, min:max, "+"); tapply(rep(p^2, length(v)), v, FUN=sum)}
        expectation <- sum(as.numeric(names(prob))*prob)
        median      <- pdunif2(0.5, min=min, max=max)
        variance    <- sum(as.numeric(names(prob))^2*prob)-expectation^2
        stddev      <- sqrt(variance)
        toLatex     <- "U^2_d(`r min`, `r max`)"
        toString    <- "Cat(`r paste0(lfrac(prob), collapse=', ')`)"
        toMath      <- bquote(paste(U[d]^2, "(", .(min), ", ", .(max), ")"))
        ddist       <- local({ min=min; max=max; function(x) { ddunif2(x, min=min, max=max) }})
        pdist       <- local({ min=min; max=max; function(q) { pdunif2(q, min=min, max=max) }})
        qdist       <- local({ min=min; max=max; function(p) { qdunif2(p, min=min, max=max) }})
        rdist       <- local({ min=min; max=max; function(n) { rdunif2(n, min=min, max=max) }})
        environment() 
      }
)

# \item{\code{exp}} exponential distribution, parameter: \code{rate}
craft(name="exp",  append=FALSE,
      function(rate) {
        package     <- "stats"
        discrete    <- FALSE
        expectation <- 1/rate
        median      <- pexp(0.5, rate=rate)
        variance    <- 1/rate^2
        stddev      <- sqrt(variance)
        toLatex     <- "Exp(`r rate`)"
        toString    <- "Exp(`r rate`)"
        toMath      <- bquote(paste("Exp(", .(rate), ")"))
        ddist       <- local({ rate=rate; function(x) { dexp(x, rate=rate) }})
        pdist       <- local({ rate=rate; function(q) { pexp(q, rate=rate) }})
        qdist       <- local({ rate=rate; function(p) { qexp(p, rate=rate) }})
        rdist       <- local({ rate=rate; function(n) { rexp(n, rate=rate) }})
        environment() 
      }
)

# \item{\code{norm}} normal distribution, parameters: \code{mean}, \code{sd}
craft("norm",  append=FALSE,
      function(mean=0, sd=1) { 
        package     <- "stats"
        discrete    <- FALSE
        expectation <- mean 
        mode        <- mean
        stddev      <- sd
        variance    <- sd^2
        mad         <- sd*qnorm(0.75)
        skewness    <- 0
        excess      <- 0
        kurtosis    <- excess+3
        entropy     <- (1+log(2*pi*sd^2))/2
        toLatex     <- "N(\\mu=`r mean`, \\sigma^2=`r sd^2`)"
        toString    <- NULL
        toMath      <- bquote(paste('N(', mu, '=', .(mean), ',', sigma^2, '=', .(variance), ')'))
        ddist       <- local({ mean=mean; sd=sd; function(x) { dnorm(x, mean=mean, sd=sd) }})
        pdist       <- local({ mean=mean; sd=sd; function(q) { pnorm(q, mean=mean, sd=sd) }})
        qdist       <- local({ mean=mean; sd=sd; function(p) { qnorm(p, mean=mean, sd=sd) }})
        rdist       <- local({ mean=mean; sd=sd; function(n) { rnorm(n, mean=mean, sd=sd) }})
        environment() 
      }
)

# \item{\code{lnorm}} log-normal distribution, parameters: \code{meanlog}, \code{sdlog}
craft("lnorm",append=FALSE,
      function(meanlog=0, sdlog=1) { 
        package     <- "stats"
        discrete    <- FALSE
        expectation <- exp(meanlog+sdlog^2/2)
        median      <- exp(meanlog)
        variance    <- (exp(sdlog^2)-1)*exp(2*meanlog+sdlog^2)
        stddev      <- sqrt(variance)
        toLatex     <- "lognormal(`r meanlog`, `r sdlog^2`)"
        toString    <- "lognormal(`r meanlog`, `r sdlog^2`)"
        toMath      <- "lognormal(`r meanlog`, `r sdlog^2`)"
        ddist       <- local({ meanlog=meanlog; sdlog=sdlog; function(x) { dlnorm(x, meanlog=meanlog, sdlog=sdlog) }})
        pdist       <- local({ meanlog=meanlog; sdlog=sdlog; function(q) { plnorm(q, meanlog=meanlog, sdlog=sdlog) }})
        qdist       <- local({ meanlog=meanlog; sdlog=sdlog; function(p) { qlnorm(p, meanlog=meanlog, sdlog=sdlog) }})
        rdist       <- local({ meanlog=meanlog; sdlog=sdlog; function(n) { rlnorm(n, meanlog=meanlog, sdlog=sdlog) }})
        environment() 
      }
)


# \item{\code{t}} Student t distribution, parameter: \code{df}
# https://en.wikipedia.org/wiki/Student%27s_t-distribution
craft("t",  append=FALSE,
      function(df=30) { 
        package     <- "stats"
        discrete    <- FALSE
        expectation <- if(df>1) 0 else NA
        median      <- 0
        mode        <- 0
        variance    <- ifint(round(df), c(1.5, 4.5), c(NA, Inf, df/(df-2)))
        stddev      <- sqrt(variance)
        skewness    <- ifint(round(df), 2.5, c(NA, 0))
        excess      <- ifint(round(df), c(1.5, 4.5), c(NA, Inf, 6/(df-4)))
        entropy     <- (df+1)/2*(digamma((df+1)/2)-digamma(df/2))+log(sqrt(df)*beta(df/2, 1/2))
        toLatex     <- "t_{`r df`}"
        toString    <- NULL
        toMath      <- bquote(t[.(df)])
        ddist       <- local({ df=df; function(x) { dt(x, df=df) }})
        pdist       <- local({ df=df; function(q) { pt(q, df=df) }})
        qdist       <- local({ df=df; function(p) { qt(p, df=df) }})
        rdist       <- local({ df=df; function(n) { rt(n, df=df) }})
        environment()
      }     
)

# \item{\code{chisq}} chi-squared distribution, parameter: \code{df}
craft(name="chisq", append=FALSE,
      function(df=3) {
        package     <- "stats"
        discrete    <- FALSE
        expectation <- df
        median      <- pchisq(0.5, df=df)
        variance    <- 2*df
        stddev      <- sqrt(variance)
        toLatex     <- "\\chi^2_{`r df`}"
        toString    <- "X^2_{`r df`}"
        toMath      <- bquote(chi[.(df)]^2)
        ddist       <- local({ df=df; function(x) { dchisq(x, df=df) }})
        pdist       <- local({ df=df; function(q) { pchisq(q, df=df) }})
        qdist       <- local({ df=df; function(p) { qchisq(p, df=df) }})
        rdist       <- local({ df=df; function(n) { rchisq(n, df=df) }})
        environment()
      }
)

# \item{\code{f}} F distribution, parameters: \code{df1},  \code{df2}
craft(name="f", append=TRUE,
      function(df1=1, df2=30) {
        package     <- "stats"
        expectation <- if(df2>2) df2/(df2-2) else NA
        median      <- pf(0.5, df1 <- df1, df2=df2)
        variance    <- if(df2>4) 2*df2^2*(df1+df2-2)/(df1*(df2-2)*(df2-4)) else NA
        stddev      <- sqrt(variance)
        toLatex     <- "F_{`r df1`, `r df2`}"
        toString    <- "F_{`r df1`, `r df2`}"
        toMath      <- "F_{`r df1`, `r df2`}"
        ddist       <- local({ df1 <- df1; df2=df2; function(x) { df(x, df1=df1, df2=df2) }})
        pdist       <- local({ df1 <- df1; df2=df2; function(q) { pf(q, df1=df1, df2=df2) }})
        qdist       <- local({ df1 <- df1; df2=df2; function(p) { qf(p, df1=df1, df2=df2) }})
        rdist       <- local({ df1 <- df1; df2=df2; function(n) { rf(n, df1=df1, df2=df2) }})
        environment()
      }
)
