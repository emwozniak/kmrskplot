\name{km_plot}
\alias{km_plot}

\title{Kaplan-Meier Survival Plot}

\description{This function generates Kaplan-Meier survival curves.
}
\usage{km_plot(fit, type, xlim_major, xlim_minor, ylim_major, ylim_minor, grid, col_grid_major, col_grid_minor, lwd_grid_major, lwd_grid_minor, conf_int, mark_time, col, lty, lwd, plot_title, plot_title_size, plot_subtitle, plot_subtitle_size, x_axis_label, y_axis_label, axis_label_size, extra_left_margin, font_family, print_risk_table, print_group_lines, print_group_names, risk_table_title, group_names, group_order, legend, legend_x_loc, legend_y_loc)
}

\arguments{
  \item{fit}{
  Fitted model object from \code{\link{survfit}}. Required.
  }
  \item{type}{
  Kaplan-Meier plot ("km") or complement ("1-km"). Optional; defaults to "km".
  }
  \item{xlim_major}{
  Vector of x-axis locations for labels and major gridlines. Optional.
  }
  \item{xlim_minor}{
  Vector of x-axis locations for minor gridlines. Optional.
  }
  \item{ylim_major}{
  Vector of y-axis locations for labels and major gridlines. Optional.
  }
  \item{ylim_minor}{
  Vector of y-axis locations for minor gridlines. Optional.
  }
  \item{grid}{
  Plot gridlines? Optional logical; defaults to TRUE.
  }
  \item{col_grid_major}{
  Color of major gridlines. Optional.
  }
  \item{col_grid_minor}{
  Color of minor gridlines. Optional.
  }
  \item{lwd_grid_major}{
  Line width of major gridlines. Optional.
  }
  \item{lwd_grid_minor}{
  Line width of minor gridlines. Optional.
  }
  \item{conf_int}{
  Plot confidence intervals? Optional logical; defaults to FALSE.
  }
  \item{mark_time}{
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
  \item{plot_title}{
  Main plot title. Optional.
  }
  \item{plot_title_size}{
  Text size for plot title. Optional.
  }
  \item{plot_subtitle}{
  Subtitle for plot. Optional.
  }
  \item{plot_subtitle_size}{
  Text size for plot subtitle. Optional.
  }
  \item{x_axis_label}{
  Label for the x-axis. Optional; defaults to "Time".
  }
  \item{y_axis_label}{
  Label for the y-axis. Optional; defaults to "Probability".
  }
  \item{axis_label_size}{
  Text size for axis labels. Optional.
  }
  \item{extra_left_margin}{
  Increase numeric value for added left margin padding to accomodate long labels. Optional.
  }
  \item{font_family}{
  Choice of "sans", "serif", "mono", or a specified font set up to work on the user's system. Optional; defaults to "serif".
  }
  \item{print_risk_table}{
  Print risk table beneath plot? Optional logical; defaults to TRUE.
  }
  \item{print_group_lines}{
  Print legend lines next to risk table? Optional logical; defaults to TRUE.
  }
  \item{print_group_names}{
  Print group/strata names next to risk table? Optional logical; defaults to TRUE.
  }
  \item{risk_table_title}{
  Header for risk table. Optional; defaults to "Total at risk".
  }
  \item{group_names}{
  Vector of group names to print in risk table. Optional.
  }
  \item{group_order}{
  Numeric vector giving order of groups to be printed in risk table. Optional.
  }
  \item{legend}{
  Logical indicator for printing a legend on the plot. Optional; defaults to FALSE since the information is found by default in the risk table.
  }
  \item{legend_x_loc}{
  X-axis coordinate or position for the legend location. Optional; defaults to "bottomleft".
  }
  \item{legend_y_loc}{
  Y-axis coordinate for the legend location. Optional; defaults to NULL.
  }
}

\value{Returns a Kaplan-Meier plot with the requested attributes.}

\author{Erica Wozniak}

\examples{
library(survival)

# Set up data as a survival object
# KM plot for time to death
# Censor those who receive a transplant or are indicated by the dataset as censored
pbc_km <- na.omit(pbc[, c("time", "status", "trt")])
pbc_km$status[pbc_km$status == 1] <- 0
pbc_km$status[pbc_km$status == 2] <- 1
pbc_km$SurvObj <- with(pbc_km, 
                       Surv(time  = time, 
                            event = status)
                            )
fit <- survfit(SurvObj ~ trt, 
               data = pbc_km, 
               conf.type = "log-log")

# If gridlines are extending outside the plot border, run
# "dev.off()" and then run the plotting function again.
km_plot(fit  = fit,
        type = "km",
        col  = c("orange3", "olivedrab"),
        # Add an extra major gridline and risk table column at time 4500
        xlim_major    = c(0, 1000, 2000, 3000, 4000, 4500, 5000),
        plot_title    = "Primary biliary cirrhosis survival",
        plot_subtitle = "By randomized treatment status",
        y_axis_label  = "Survival",
        x_axis_label  = "Time since registration (days)",
        group_names   = c("D-penacillamine", "Placebo"),
        risk_table_title = bquote(italic("Patients at risk:")))
}
