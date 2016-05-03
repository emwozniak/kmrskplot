\name{km.plot}
\alias{km.plot}

\title{Kaplan-Meier Survival Plot}

\description{This function generates Kaplan-Meier survival curves.
}
\usage{km.plot(fit, type, xlim.major, xlim.minor, ylim.major, ylim.minor, grid, col.grid.major, col.grid.minor, lwd.grid.major, lwd.grid.minor, conf.int, mark.time, col, lty, lwd, plot.title, plot.title.size, plot.subtitle, plot.subtitle.size, x.axis.label, y.axis.label, axis.label.size, extra.left.margin, font.family, print.risk.table, print.group.lines, print.group.names, risk.table.title, group.names, group.order, legend, legend.x.loc, legend.y.loc)
}

\arguments{
  \item{fit}{
  Fitted model object from \code{\link{survfit}}. Required.
  }
  \item{type}{
  Survival plot ("km") or complement ("1-km"). Optional; defaults to "km".
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
  Plot gridlines? Optional logical; defaults to TRUE.
  }
  \item{col.grid.major}{
  Color of major gridlines. Optional.
  }
  \item{col.grid.minor}{
  Color of minor gridlines. Optional.
  }
  \item{lwd.grid.major}{
  Line width of major gridlines. Optional.
  }
  \item{lwd.grid.minor}{
  Line width of minor gridlines. Optional.
  }
  \item{conf.int}{
  Plot confidence intervals? Optional logical; defaults to FALSE.
  }
  \item{mark.time}{
  Plot censoring tick marks? Optional logical; defaults to FALSE.
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
  \item{x.axis.label}{
  Label for the x-axis. Optional; defaults to "Time".
  }
  \item{y.axis.label}{
  Label for the y-axis. Optional; defaults to "Probability".
  }
  \item{axis.label.size}{
  Font size for axis labels. Optional.
  }
  \item{extra.left.margin}{
  Increase numeric value for added left margin padding to accomodate long labels. Optional.
  }
  \item{font.family}{
  Choice of "sans", "serif", "mono", or a specified font set up to work on the user's system. Optional; defaults to "serif".
  }
  \item{print.risk.table}{
  Print risk table beneath plot? Optional logical; defaults to TRUE.
  }
  \item{print.group.lines}{
  Print legend lines next to risk table? Optional logical; defaults to TRUE.
  }
  \item{print.group.names}{
  Print group/strata names next to risk table? Optional logical; defaults to TRUE.
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
  \item{legend}{
  Logical indicator for printing a legend on the plot. Optional; defaults to FALSE since the information is found by default in the risk table.
  }
  \item{legend.x.loc}{
  X-axis coordinate or position for the legend location. Optional; defaults to "bottomleft".
  }
  \item{legend.y.loc}{
  Y-axis coordinate for the legend location. Optional; defaults to NULL.
  }
}

\value{Returns a Kaplan-Meier plot with the requested attributes.}

\author{Erica Wozniak}

\examples{
library(survival)

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
