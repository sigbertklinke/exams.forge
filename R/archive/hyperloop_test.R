library("exams.forge")
x <- runif(100)
correct <- ttest_num(x=x, mu0=0.5, sigma=sqrt(1/12))
str(correct)

res1 <- hyperloop(ttest_num, 
                 n           = list(1, correct$n, correct$n+1),
                 mu0         = list(correct$mu0, correct$mean),
                 mean        = list(correct$mu0, correct$mean), 
                 sigma       = list(correct$sigma, correct$sd, sqrt(correct$sigma), sqrt(correct$sd)),
                 sd          = list(correct$sigma, correct$sd, sqrt(correct$sigma), sqrt(correct$sd)),
                 norm        = list(TRUE, FALSE)
)

res2 <- runloop(ttest_num, 
                  n           = list(1, correct$n, correct$n+1),
                  mu0         = list(correct$mu0, correct$mean),
                  mean        = list(correct$mu0, correct$mean), 
                  sigma       = list(correct$sigma, correct$sd, sqrt(correct$sigma), sqrt(correct$sd)),
                  sd          = list(correct$sigma, correct$sd, sqrt(correct$sigma), sqrt(correct$sd)),
                  norm        = list(TRUE, FALSE)
)


x   <- rnorm(100)
trm1 <- hyperloop(mean, x=list(x), trim=as.list(seq(0, 0.5, by=0.05)))
# automatic conversion of x to list(x)
trm2 <- runloop(mean, x=x, trim=as.list(seq(0, 0.5, by=0.05))) 


trm3 <- hyperloop(mean, x=list(x), trim=as.list(seq(0, 0.5, by=0.05)))
# automatic conversion of x to list(x)
trm4 <- runloop(mean, x=list(x), trim=as.list(seq(0, 0.5, by=0.05))) 

