\name{kmrskplot-package}
\alias{kmrskplot-package}
\alias{kmrskplot}
\docType{package}
\title{
  \packageTitle{kmrskplot}
}
\description{
  \packageDescription{kmrskplot}
}
\details{
\packageDESCRIPTION{kmrskplot}
  \packageIndices{kmrskplot}
  This package outputs Kaplan-Meier plots and cumulative incidence plots for competing risks models. A table of the number of observations in the risk set at each specified time point is printed below the plot.
}
\author{
\packageAuthor{kmrskplot}

Maintainer: \packageMaintainer{kmrskplot}
}
%\references{
%}

\keyword{ package }
%\seealso{
%}
\examples{
library(survival)
library(cmprsk)

#KM plot for time to death
#Censor those who receive a transplant or are indicated by the dataset as censored
pbc.km <- na.omit(pbc[, c("time", "status", "trt")])
pbc.km$status[pbc.km$status==1] <- 0
pbc.km$status[pbc.km$status==2] <- 1
pbc.km$SurvObj <- with(pbc.km, Surv(time=time, event=status))
fit <- survfit(SurvObj ~ trt, data=pbc.km, conf.type="log-log")

#Competing risks plot
#Pretend transplant and death are competing risks
#pbc.cr <- na.omit(pbc[, c("time", "status", "trt")])
#test <- cuminc(ftime=pbc.cr$time, fstatus=pbc.cr$status, group=pbc.cr$trt, cencode=0)
#cr <- crr(ftime=pbc.cr$time, fstatus=pbc.cr$status, cov1=pbc.cr$trt) 

#t <- pbc.cr[pbc.cr$time>0, ]

kmrsk.plot(fit=fit,
           type="km",
           col=c("darkblue", "darkred"),
           risk.table.title=bquote(underline("Patients at risk:")),
           plot.title=c("Patient survival after transplant registration"),
           plot.subtitle=c("By treatment status"),
           group.names=c("D-penicillamine", "Placebo"),
           xlim.major=c(0, 1000, 2000, 3000, 4000, 4500, 5000))
}
