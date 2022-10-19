#Question 1f
chi_test <- function (dat, res.type = "pearson") {
  #Define Variables
  observed <- dat
  rowsum <- apply(observed, MARGIN = 1, FUN = sum)
  colsum <- apply(observed, MARGIN = 2, FUN = sum)
  n <- sum(observed)
  df <- (nrow(observed)-1)*(ncol(observed)-1)
  #Calculate expected, TS, and p-value
  expected <- rowsum%*%t(colsum)/n
  tstat <- sum((observed-expected)^2/expected)
  p.value <- pchisq(tstat, df = df, lower.tail = FALSE)
  #Create condition based on res.type argument
  if (res.type=="std") {residuals <- (observed-expected)/sqrt(expected*(1-(rowsum/n))*(1-(colsum/n)))}
  else {residuals <- (observed-expected)/sqrt(expected)}
  #Name dimensions of results 
  results <- list(tstat,p.value,expected,res.type,residuals)
  names(results) <- c("Test Statistic","p-value","Expected Count", 
                      "Residual Type", "Residuals")
  return(results)
  }

#Question 1g
#Generate contingency table matrix
(dat <- matrix(c(6,115,256,18,256,442,13,136,155), nrow = 3))
chi_test(dat, res.type = "std")
#At the 0.01 level of significance (alpha level), reject the null hypothesis
#that one's marital status is independent of their education level.

#Question 2e
f_test <- function (x1, x2, alt = "two-sided", lev = 0.95) {
  #Define Variables
  alpha.lvl <- 1-lev
  c.l <- as.character(100*lev)
  cl <- paste(c.l,"% Confidence Level",sep="")
  s.1 <- var(x1)
  s.2 <- var(x2)
  fstat <- (s.1)/(s.2)
  df.1 <- length(x1)-1
  df.2 <- length(x2)-1
  ci <- c(fstat*(1/(qf(1-alpha.lvl/2, df1 = df.1, df2 = df.2))),
          fstat*(1/(qf(alpha.lvl/2, df1 = df.1, df2 = df.2))))
  #Conduct two separate Shapiro-Wilk Normality Test
  st1 <- shapiro.test(x1)
  st2 <- shapiro.test(x2)
  mode(st1)
  if(st1[2] < alpha.lvl) {
    warning(paste("Reject the null hypothesis that the x1 sample is normally distributed"))
  }
  if(st2[2] < alpha.lvl) {
    warning(paste("Reject the null hypothesis that the x2 sample is normally distributed"))
  }
  if(st1[2] < alpha.lvl & st2[2] < alpha.lvl) {
    warning(paste("Reject the null hypothesis of normal distribution for both samples"))
  } 
  #Create separate conditions for varying alternative hypotheses
  if(alt == "less") {
    p.value <- pf(fstat, df1 = df.1, df2 = df.2)}
  if(alt == "two-sided") {
    p.value <- 2 * min(c(pf(fstat, df1 = df.1, df2 = df.2), 
                         pf(fstat, df1 = df.1, df2 = df.2, lower.tail = FALSE)))}
  if(alt == "greater") {
    p.value <- pf(fstat, df1 = df.1, df2 = df.2, lower.tail = FALSE)}
  results <- list(fstat,p.value,cl,ci)
  names(results) <- c("Test Statistic","p-value","Confidence Level", 
                     "Confidence Interval")
  return(results)
}

#Question 2f
#Generate sample vectors
(x1 <- c(136, 129, 143, 110, 122, 128, 137, 140, 92, 107))
(x2 <- c(182, 245, 138, 142, 119, 131, 116, 142))
#Run test using function
f_test(x1, x2, alt="two-sided", lev = 0.99)
#Fail to reject the null hypothesis that the true ratio of variances is equal to 1
#at the 0.01 significance level as the p-value is greater than the alpha level.

#We are 99% confident that the true ratio of variances lies between 0.01775751
#and 1.04088993.

#Question 3
#Input values from question
n1 <- 15
n2 <- 17
d.f <- n1+n2-2
x <- seq(-4,4,length=200)
y <- dt(x,d.f)
vd.f <- bquote(n[1]+n[2]-2)
#Plot the distribution and add title
plot(x,y, type = "l", axes=FALSE, ylab = "", xlab = "", main = bquote("Distribution of T"
~ "=" ~ over((bar(X)[1]-bar(X)[2])-(mu[1]-mu[2]),sqrt(S[p]^2~(over(1,n[1])+over(1,n[2])))) ~ "~" 
~ t[n[1]+n[2]-2]*","~ "where" ~ S[p]^2 ~ "=" ~ over((n[1]-1)*S[1]^2~(n[2]-2)*S[2]^2,n[1]+n[2]-2)), font.main = 1)
abline(h=0)
alpha <- paste(format(0.10,nsmall=2))
#Create legend
legend("topright", legend = c(bquote(n[1] ~ "=" ~ 15), bquote(n[2] ~ "=" ~ 17), 
      bquote(nu==.(vd.f)), bquote(alpha==.(alpha))), bty = "n")
#Shade underneath the distribution
x.2 <- seq(-4,qt(0.05,df=d.f),length=100)
y.2 <- dt(x.2,df = d.f)
polygon(c(-4,x.2,qt(0.05,df=d.f)),c(0,y.2,0),col="#F4B9C2")
x.3 <- seq(4,qt(0.95,df=d.f),length=100)
y.3 <- dt(x.3, df= d.f)
polygon(c(4,x.3,qt(0.95,df=d.f)),c(0,y.3,0),col="#F4B9C2")
x.4 <- seq(qt(0.05,df=d.f),qt(0.95,df=d.f), length=100)
y.4 <- dt(x.4, df = d.f)
polygon(c(qt(0.05,df=d.f),x.4,qt(0.95,df=d.f)),c(0,y.4,0),col="#CBE1FC")
#Add axis
axis(side=1, a=c(qt(0.05,df=d.f),0,qt(0.95,df=d.f)), 
     labels = c(expression("-"*t[alpha/2]*","*nu),0,expression(t[alpha/2]*","*nu)), font = 2)
#Label distribution segments
text(0,0.20, labels = expression(1 - alpha),font = 2,cex=2.5)
text(median(x.2), 0.10, labels = expression(over(alpha,2)), font = 2, cex = 1.5)
text(median(x.3), 0.10, labels = expression(over(alpha,2)), font = 2, cex = 1.5)
#Add arrows
arrows(x0 = median(x.2)+.08, y0 = 0.10, x1 = -2, y1 = 0.025)
arrows(x0 = median(x.3)-.08, y0 = 0.10, x1 = 2, y1 = 0.025)
