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
\references{
Risk table formatting was borrowed heavily from \link{http://biostat.mc.vanderbilt.edu/wiki/pub/Main/TatsukiRcode/TatsukiRcodeKMplot.r}.
}

\keyword{ package }
\seealso{
  \code{\link{km.plot}}
  \code{\link{cmprsk.plot}}
}
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

#If gridlines are extending outside the plot border, run
#"dev.off()" and then run the plotting function again.
km.plot(fit=fit,
        type="km",
        #Add an extra major gridline and risk table column at time 4500
        xlim.major=c(0, 1000, 2000, 3000, 4000, 4500, 5000),
        col=c("orange3", "olivedrab"),
        plot.title="Primary biliary cirrhosis survival",
        plot.subtitle="By randomized treatment status",
        y.axis.label="Survival",
        x.axis.label="Time since registration (days)",
        group.names=c("D-penacillamine", "Placebo"),
        risk.table.title=bquote(italic("Patients at risk:")))
}
