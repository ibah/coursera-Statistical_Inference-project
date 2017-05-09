

    # 1. investigate the exponential distribution in R and compare it with the Central Limit Theorem

# The exponential distribution can be simulated in R with rexp(n, lambda) where lambda is the rate parameter.
# The mean of exponential distribution is 1/lambda and the standard deviation is also 1/lambda.

# hint:
The question asks to investigate the behavior of averages of random variables via simulations.
To do this, you need to do many simulations (maybe a 1,000) of averages from that distribution. 
To do this exercise, you really only need to modify the code from lecture 7
(https://github.com/bcaffo/courses/blob/master/06_StatisticalInference/07_Asymptopia/index.Rmd)

# Illustrate via simulation and associated explanatory text
# the properties of the distribution of the mean of 40 exponentials.


    # Simulations
lambda <- .2; n <- 40; nosim <- 1000
set.seed(1234)
sim <- rexp(n*nosim, lambda)
mtx <- matrix(sim, nosim)
mtx2 <- replicate(nosim, rexp(n, lambda))


    # sample mean vs. theoretical mean
mns <- apply(mtx, 1, mean)
# 1. Show the sample mean and compare it to the theoretical mean of the distribution.
    # show the numbers
t_mn = 1/lambda
t_sd = 1/lambda
mean(sim)
mean(mns)
# by CLT approximatelly normally distributed with mn = mn and sd = s/sqrt(n)


# XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxxxxxxxxxxxxxxxxxxxxxxx
plot(sapply(0:10, function(x) dnorm(x,mean=t_mn,sd=t_sd)))
head(data)
ggplot(data, aes(x=x))+geom_histogram(alpha = .20)
ggplot(data, aes(x=x))+geom_histogram(alpha = .20, binwidth=.3)
ggplot(data, aes(x=x))+geom_histogram(alpha = .20, binwidth=.3, colour = "black")
ggplot(data, aes(x=x))+geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..))

# good: <--------------------------------------------------------------------------------
library(ggplot2)
data <- data.frame(Sample_Means = mns)
g <- ggplot(data, aes(x=Sample_Means), )
g <- g + geom_histogram(alpha = .5, fill="blue", aes(y = ..density..,)) # binwidth=.3
# g <- g + geom_density(alpha=.1, size=2, color="blue", alpha=0.1) # alpha doesn't work
g <- g + geom_vline(aes(xintercept=mean(mns)), color="blue", size=1) #  show_guide=TRUE
g <- g + stat_function(fun = dnorm, alpha = .5, size = 2, color="green", args=list(mean=t_mn, sd=t_sd/sqrt(n)))
g <- g + geom_vline(aes(xintercept=t_mn), size=1, color="green") # linetype="Mean", color="green", show_guide=TRUE
g <- g +  scale_x_continuous(name="Sample Means", breaks=seq(2,8,1)) # limits=c(2,8)
g <- g + ggtitle("Histogram of Means of Samples of 40 Exponential Randoms")
g

# XXXXXXXXXXXXXXXXXXXXXxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
g <- g +
    theme(legend.background=element_blank(),
          panel.grid.major=element_blank(), panel.grid.minor=element_blank(), 
          panel.background=element_rect(colour="black", fill="white"))
g
cfunc <- function(x, n) sqrt(n) * (mean(x) - 3.5) / 1.71
dat <- data.frame(
    x = c(apply(matrix(sample(1 : 6, nosim * 10, replace = TRUE), 
                       nosim), 1, cfunc, 10),
          apply(matrix(sample(1 : 6, nosim * 20, replace = TRUE), 
                       nosim), 1, cfunc, 20),
          apply(matrix(sample(1 : 6, nosim * 30, replace = TRUE), 
                       nosim), 1, cfunc, 30)
    ),
    size = factor(rep(c(10, 20, 30), rep(nosim, 3))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(alpha = .20, binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2)
g + facet_grid(. ~ size)


    # Sample Variance versus Theoretical Variance <-------------------------------------------------------
# Include figures (output from R) with titles. Highlight the variances you are comparing.
# Include text that explains your understanding of the differences of the variances.
# 2. Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
    
    # show the numbers for the sample & theoretical
(1/lambda)^2
mn <- mean(sim)
sum((sim-mn)^2)/(n*nosim-1)
var(sim)
vars <- apply(mtx, 1, var)
mean(vars)
t_var = (t_sd)^2
hist(vars)
qplot(vars)
library(ggplot2)
data <- data.frame(Sample_Variances=vars)
g <- ggplot(data, aes(x=Sample_Variances))
g <- g + geom_histogram(alpha = .50, fill="blue", aes(y = ..density..,)) # binwidth=.3
g <- g + geom_vline(aes(xintercept=mean(vars)), color="blue", size=1) #  show_guide=TRUE
g <- g + geom_vline(aes(xintercept=t_var), size=1, color="green") # linetype="Mean", color="green", show_guide=TRUE
g <- g + scale_x_continuous(name="Sample Variances", breaks=seq(0,100,5)) # limits=c(2,8)
g <- g + ggtitle("Histogram of Variances of Samples of 40 Exponential Randoms")
g

    # show the numbers for the sample mean & theoretical
# Variance of Means of Samples of 40 Exponential Randoms
(1/lambda)^2/n
var(mns)


    # Distribution
# Via figures and text, explain how one can tell the distribution is approximately normal.
# 3. Show that the distribution is approximately normal.
# Focus on the difference between the distribution of a large collection of random exponentials
# and the distribution of a large collection of averages of 40 exponentials.
# e.g. xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# 1000 rand. unif.
hist(runif(1000))
# 1000 avg. of 40 rand. unif.
mns = NULL
for (i in 1:1000) mns = c(mns, mean(runif(40)))
hist(mns)
qplot(sim)
qplot(x)
# <-------------------------------------------------------------------------------------------
data <- data.frame(
    x <- c(sim, mns),
    t <- factor(rep(c("Exponential Randoms","Sample Means"),c(nosim*n,nosim)))
    )
g <- ggplot(data, aes(x=x, fill=t))
g <- g + geom_histogram(alpha=.5, binwidth=.3, aes(y = ..density..,))
g <- g + stat_function(fun=dnorm, args=list(mean=t_mn, sd=t_sd/sqrt(n)))
g <- g + scale_x_continuous(name="value", limits=c(0,20), breaks=seq(0,100,5))
g <- g + theme(legend.position="bottom")
g <- g + ggtitle("Histogram of Exponential Randoms and Means of Samples of 40 Exponential Randoms")
g

# xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
# g <- g + stat_function(fun = dnorm, size = 2)
g <- g + facet_grid(.~t, scales = "free") + theme(legend.position="none") # + geom_density()
g







    # 2. analyze the ToothGrowth data in the R datasets package. 

# 1. Load the ToothGrowth data and perform some basic exploratory data analyses
# add efficient graphs in basic, lattice, ggplot2 (panel plots, colors etc.) <--------------------------
? ToothGrowth
head(ToothGrowth)
ToothGrowth
str(ToothGrowth)
summary(ToothGrowth)
attach(ToothGrowth)
hist(len)
qplot(len)
plot(dose, len)
qplot(dose, len)
boxplot(tapply(len, dose, as.vector))
plot(supp, len)
qplot(supp, len)
sum(!complete.cases(ToothGrowth))
require(graphics)
coplot(len ~ dose | supp, data = ToothGrowth, panel = panel.smooth,
       xlab = "ToothGrowth data: length vs dose, given type of supplement")
# exclude this:
lm.fit <- lm(len~.,data=ToothGrowth)
summary(lm.fit)
# 2. Provide a basic summary of the data.
# copy from above <------------------------------------------------------------------------------------
# 3. Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose.
    # (Only use the techniques from class, even if there's other approaches worth considering)
    
    # by sup
g1 <- ToothGrowth[supp=='OJ',]$len; g2 <- ToothGrowth[supp=='VC',]$len
n1 <- length(g1); n2 <- length(g2)
s1 <- var(g1); s2 <- var(g2)
    # Confidence intervals
    # paired observations
diff <- g1-g2
mn <- mean(diff); s <- sd(diff)
mn + c(-1,1)*qt(.975,n1-1)*s/sqrt(n1)
t.test(diff)$conf.int
t.test(g1, g2, paired=T)$conf.int
    # independent groups, equal variance
mn <- mean(g1) - mean(g2)
pooled_var = ((n1-1)*s1 + (n2-1)*s2)/(n1+n2-2)
mn + c(-1,1)*qt(.975,n1+n2-2)*sqrt(pooled_var * (1/n1+1/n2)) # remember pooled_var * (1/n1+1/n2)
t.test(g1, g2, paired=F, var.equal=T)$conf.int
    # independent groups, unequal variance
t.test(g1, g2, paired=F, var.equal=F)$conf.int
    # Hypotheses testing
# use lectures to perform this correctly <--------------------------------------------------------------
    # paired observations
$H_0: diff = 0$
$H-a: diff \neq 0$
$\alpha = .05$
Critical value for two sided test:
ct <- qt(0.975, n1-1)
Test statistic $ TS = \frac{\bar{X} - \mu_0}{S / \sqrt{n}} $
ts <- (mn - 0) / (s/sqrt(n1))
so reject H0
the same:
t.test(diff)
    # independent groups, equal and unequal variance
$H_0: mn1 = mn2$
$H-a: mn1 \neq mn2$
$\alpha = .05$
t.test(g1, g2, paired=F, var.equal=T)
t.test(g1, g2, paired=F, var.equal=F)
# both can't reject the H0

    # by dose
# checking XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
str(ToothGrowth)
tapply(ToothGrowth$len, ToothGrowth$dose, function(x) length(x))
combn(c(.5,1,2),2)
apply(combn(c(.5,1,2),2), 2, function(x) paste(x, collapse=" "))
l <- list(1)
names(l) <- "a"
l[[1]] <- t.test(g1, g2, paired=F, var.equal=F)
l[[1]]
apply(combn(c(.5,1,2),2), 2, function(x) {
    # cat("H0: dose of", x[1], "vs. dose of", x[2], "\n")
    l <- list(1)
    names(l) <- cat("H0: dose of", x[1], "vs. dose of", x[2])
    l[[1]] <- t.test(subset(ToothGrowth, dose==x[1])$len - subset(ToothGrowth, dose==x[2])$len)
    # class(x[1])
    # subset(ToothGrowth, dose==x[1])$len
    # data.frame(a=subset(ToothGrowth, dose==x[1])$len, b=subset(ToothGrowth, dose==x[2])$len)
})

# cont <--------------------------------------------------------------------------------------------
doses <- combn(c(.5,1,2),2)
for(i in seq_len(ncol(doses))) {
    cat("H0: dose of", doses[1,i], "vs. dose of", doses[2,i], "\n")
    print(t.test(subset(ToothGrowth, dose==doses[1,i])$len - subset(ToothGrowth, dose==doses[2,i])$len))
}

# 4. State your conclusions and the assumptions needed for your conclusions. 
random population of guinea pigs (no confounding factors)
paired - the same guinea pigs 1-10, order independent
t-stat assumes normal underlying, we should assume len is close to normal (or at least symmetric, mound shaped

# Some criteria that you will be evaluated on
# 1. Did you  perform an exploratory data analysis of at least a single plot or table highlighting basic features of the data?
# 2. Did the student perform some relevant confidence intervals and/or tests?
# 3. Were the results of the tests and/or intervals interpreted in the context of the problem correctly? 
# 4. Did the student describe the assumptions needed for their conclusions?

