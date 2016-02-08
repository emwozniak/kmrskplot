\name{kmrsk.plot}
\alias{kmrsk.plot}

\title{Survival Plot}

\description{This function generates Kaplan-Meier or cumulative incidence plots for survival and competing risks models.
}
\usage{kmrsk.plot(fit, type, xlim.major, xlim.minor, ylim.major, ylim.minor, grid, col.grid.major, col.grid.minor, lwd.grid.major, lwd.grid.minor, conf.int, mark.time, col, lty, lwd, plot.title, plot.title.size, plot.subtitle, plot.subtitle.size, axis.label.size, extra.left.margin, print.risk.table, print.group.lines, print.group.names, risk.table.title, group.names, group.order)
}

\arguments{
  \item{fit}{
  Fitted model object from \code{\link{survfit}} or \code{\link{cuminc}}. Required.
  }
  \item{type}{
  Type of plot or complement. Choose from "km", "1-km", "cmprsk", "1-cmprsk". Required.
  }
  \item{xlim.major}{
  Vector of x-axis locations for labels and major gridlines. Optional.
  }
  \item{xlim.minor}{
  Vector of x-axis locations for minor gridlines. Optional.
  }
  \item{ylim.major}{
  Vector of y-axis locations for labels and major gridlines. Optional.
  }
  \item{ylim.minor}{
  Vector of y-axis locations for minor gridlines. Optional.
  }
  \item{grid}{
  Logical indicator for gridded background. Optional; defaults to TRUE.
  }
  \item{col.grid.major}{
  Color of major gridlines. Optional.
  }
  \item{col.grid.minor}{
  Color of minor gridlines. Optional.
  }
  \item{lwd.grid.major} {
  Line width of major gridlines. Optional.
  }
  \item{lwd.grid.minor}{
  Line width of minor gridlines. Optional.
  }
  \item{conf.int}{
  Logical indicator for confidence intervals on plots. Optional; defaults to FALSE.
  }
  \item{mark.time}{
  Logical indicator for tick marks at censoring locations. Optional; defaults to FALSE.
  }
  \item{col}{
  Vector of colors for plotted curves. Optional; defaults to all black.
  }
  \item{lty}{
  Vector of line types for plotted curves. Optional; defaults to all solid.
  }
  \item{lwd}{
  Vector of line widths for plotted curves. Optional; defaults to 1.
  }
  \item{plot.title}{
  Main plot title. Optional.
  }
  \item{plot.title.size}{
  Font size for plot title. Optional.
  }
  \item{plot.subtitle}{
  Subtitle for plot. Optional.
  }
  \item{plot.subtitle.size}{
  Font size for plot subtitle. Optional.
  }
  \item{axis.label.size}{
  Font size for axis labels. Optional.
  }
  \item{extra.left.margin}{
  Increase numeric value for added left margin padding to accomodate long labels. Optional.
  }
  \item{print.risk.table}{
  Logical indicator for printing risk table at specified time points. Optional; defaults to TRUE.
  }
  \item{print.group.lines}{
  Logical indicator for printing legend lines next to risk table. Optional; defaults to TRUE.
  }
  \item{print.group.names}{
  Logical indicator for printing group/strata names next to risk table. Optional; defaults to TRUE.
  }
  \item{risk.table.title}{
  Header for risk table. Optional; defaults to "Total at risk".
  }
  \item{group.names}{
  Vector of group names to print in risk table. Optional.
  }
  \item{group.order}{
  Numeric vector giving order of groups to be printed in risk table. Optional.
  }
  \item{failtype}{
  The failure type that should be plotted, if only one, for a competing risks model. Optional.
  }
  \item{legend}{
  Logical indicator for printing a legend on the plot. Optional; defaults to FALSE since the information is found by default in the risk table.
  }
}

\value{Returns survival plot with the requested attributes.}

\author{Erica Wozniak}

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
