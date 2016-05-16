##########################################
# KAPLAN-MEIER PLOTS FOR CLASS 'SURVFIT' #
##########################################

#fit must be a survfit object for KM plots
km_plot <- function(fit  = fit,
                    type = "km",
                    
                    # Axis measures
                    xlim_major = pretty(fit$time),
                    xlim_minor = ((pretty(fit$time) + 
                                   c(NA, pretty(fit$time)[-length(pretty(fit$time))])) / 
                                   2)[-1],
                    ylim_major = pretty(c(0, 1), 5),
                    ylim_minor = ((pretty(c(0, 1), 5) + 
                                   c(NA, pretty(c(0, 1), 5)[-length(pretty(c(0, 1), 5))])) /
                                   2)[-1],
                    
                    # Gridlines
                    grid = TRUE,
                    col_grid_major = "gray90",
                    col_grid_minor = "gray97",
                    lwd_grid_major = 0.5,
                    lwd_grid_minor = 0.2,
                    
                    # Plotting options
                    conf_int  = FALSE,
                    mark_time = FALSE,
                    col = rainbow(length(fit$strata)),
                    lty = rep(1, length(fit$strata)),
                    lwd = rep(1, length(fit$strata)),
                    
                    # Headers and axis labels
                    plot_title         = "",
                    plot_title_size    = 1.25,
                    plot_subtitle      = "",
                    plot_subtitle_size = 1,
                    x_axis_label       = "Time",
                    y_axis_label       = "Probability",
                    axis_label_size    = 0.9,
                    extra_left_margin  = 3,
                    font_family        = "serif",
                    
                    # Risk table options
                    print_risk_table  = TRUE,
                    print_group_lines = TRUE,
                    print_group_names = TRUE,
                    risk_table_title  = "Total at risk",
                    
                    # Legend options
                    group_names  = names(fit$strata),
                    group_order  = seq(length(fit$n)),
                    legend = FALSE,
                    legend_x_loc = "bottomleft",
                    legend_y_loc = NULL
                    ) {
  
  # Copy current/default plotting parameters to reset after building plot
  op <- par(no.readonly = TRUE)
  
  # Tick labels closer to axis, rotated text, wider left margin
  par(oma = c(1, 1, 1, 1), 
      mar = c(4 + length(fit$strata), 4 + extra_left_margin, 4, 2) + 0.1, 
      mgp = c(3, 0.5, 0), 
      las = 1, 
      family = font_family)
  
  # Establish plotting window
  plot.new()
  plot.window(c(min(xlim_major), max(xlim_major)), c(min(ylim_major), max(ylim_major)))
  
  # Establish gridlines 
  if (grid == TRUE) {
    abline(v = xlim_minor, lwd = lwd_grid_minor, col = col_grid_minor)
    abline(h = ylim_minor, lwd = lwd_grid_minor, col = col_grid_minor)
    abline(v = xlim_major, lwd = lwd_grid_major, col = col_grid_major)
    abline(h = ylim_major, lwd = lwd_grid_major, col = col_grid_major)
  }
  
  # Put a box around the plot
  box(which = "plot", lty = "solid")
  
  # Include axes and labels
  axis(1, at = xlim_major, tck = -0.018, cex.axis = axis_label_size)
  axis(1, at = xlim_minor, tck = -0.01, labels = FALSE)
  axis(2, at = ylim_major, tck = -0.018, cex.axis = axis_label_size)
  axis(2, at = ylim_minor, tck = -0.01, labels = FALSE)
  title(xlab = x_axis_label, line = 1.75)
  title(ylab = y_axis_label, line = 2.25)
  
  # Include title and subtitle
  title(plot_title, cex.main = plot_title_size, font.main = 1)
  mtext(plot_subtitle, line = 0.25, cex = plot_subtitle_size)
  
  # Print a risk table beneath the plot 
  if (print_risk_table == TRUE) {
    
    # Print group names in bottom plotting margin
    group_pos <- (par()$usr[2] - par()$usr[1]) / (-8)  
    pad       <- abs(group_pos / 8)
    line_pos  <- (1:length(group_names))[order(group_order)] + 2
    
    if (print_group_names == TRUE) {
      mtext(group_names, 
            side = 1, 
            line = line_pos, 
            at   = group_pos, 
            adj  = 1, 
            col  = 1, 
            las  = 1, 
            cex  = axis_label_size )
    }
    
    # Print lines matching plot in bottom plotting margin  
    if (print_group_lines == TRUE) {
      par('xpd' = TRUE)
      for (i in 1:length(group_names)) {
        axis(side      = 1, 
             at        = c(group_pos + pad, 0 - 2*pad), 
             labels    = FALSE, 
             line      = line_pos[i] + 0.6, 
             lwd.ticks = 0,
             col       = col[i], 
             lty       = lty[i], 
             lwd       = lwd[i]) 
        }
      }
    
    # Extract risk groups
    rsk <- summary(fit, times = xlim_major)  
    
    #if(length(kms$strata)==1) kms$strata <- rep(1,length(kms$time) )
    rskdat <- data.frame(time   = rsk$time, 
                         n_risk = rsk$n.risk, 
                         strata = c(rsk$strata))
    datsplit <- split(rskdat, f = rskdat$strata)
    
    # Print right-justified risk table values beneath plot
    ndigits <- lapply(datsplit, function(x) nchar(x[, 2]))
    mat <- do.call('rbind', 
                   lapply(ndigits, 
                          function(z) { 
                            length(z) <- max(sapply(ndigits, length))
                            z
                            } 
                          ) 
                        )
    matapply <- apply(mat, 2, max, na.rm = T)
    for (i in seq(length(fit$strata))) {
      iter <- datsplit[[i]] 
      w.adj <- strwidth('0', cex = axis_label_size, font = par('font')) / 
                  2*matapply[1:nrow(iter)]
      mtext(side = 1,
              at = iter$time + w.adj, 
            text = iter$n_risk, 
            line = line_pos[i], 
             cex = axis_label_size,
             adj = 1, 
             col = 1, 
             las = 1)
    }
    
    #Header for risk table
    mtext(side = 1, 
          text = risk_table_title, 
            at = group_pos, 
          line = 1.5, 
           adj = 1, 
           col = 1, 
           las = 1, 
           cex = axis_label_size)
  }
  
  # Draw the KM plot
  if (type == "km") {
    lines(fit, 
          conf.int  = conf_int,
          mark.time = mark_time,
          col       = col,
          lty       = lty,
          lwd       = lwd,
          xmax      = max(xlim_major),
          ymin      = min(ylim_major))
  }
  
  # Draw the KM complement plot
  else if (type == "1-km") {
    lines(fit, 
          fun       = function(x) 1-x,
          conf.int  = conf_int,
          mark.time = mark_time,
          col       = col,
          lty       = lty,
          lwd       = lwd,
          xmax      = max(xlim_major),
          ymin      = min(ylim_major))
  }
  
  # Legend options
  if (legend == TRUE) {
    legend(x      = legend_x_loc, 
           y      = legend_y_loc, 
           legend = group_names[group_order], 
           col    = col[group_order],
           lty    = lty[group_order],
           lwd    = lwd[group_order],
           cex    = axis_label_size,
           bty    = "o", 
           bg     = "white",
           inset  = 0.01)
  }  
  
  # Reset original plotting parameters
  par(op)
}

#####################################################
# CUMULATIVE INCIDENCE FUNCTIONS FOR CLASS 'CUMINC' #
#####################################################

# cmprsk.plot <- function(#Inputs needed to run cuminc function
#                         ftime,
#                         fstatus,
#                         group,
#                         cencode=0,
#                         subset.var,
#                         #subset.val needs to be a quoted value
#                         subset.val,
#                         
#                         #Plotting options
#                         font.family="serif",
#                         extra.left.margin=3,
#                         
#                         #Grid options
#                         grid=TRUE,
#                         xlim.major=pretty(ftime),
#                         xlim.minor=((pretty(ftime) + 
#                                        c(NA, pretty(ftime)[-length(pretty(ftime))]))/2)[-1],
#                         ylim.major=pretty(c(0, 1), 5),
#                         ylim.minor=((pretty(c(0, 1), 5) + 
#                                        c(NA, pretty(c(0, 1), 5)[-length(pretty(c(0, 1), 5))]))/2)[-1],
#                         col.grid.major="gray90",
#                         col.grid.minor="gray97",
#                         lwd.grid.major=0.5,
#                         lwd.grid.minor=0.2,
#                         
#                         #Axis options
#                         axis.label.size=0.9,
#                         x.axis.label="Time",
#                         y.axis.label="Probability",
#                         
#                         plot.title="",
#                         plot.title.size=1.25,
#                         plot.subtitle="",
#                         plot.subtitle.size=1,
#                         
#                         #CIF options
#                         col="black",
#                         lwd=1,
#                         lty=1,
#                         
#                         #Legend options
#                         group.names=names(fit$strata),
#                         group.order=seq(length(fit$n)),
#                         legend=FALSE,
#                         legend.x.loc="bottomleft",
#                         legend.y.loc=NULL
#                         )
# {
#   #Evaluate the cuminc functions
#   if (missing(subset.var) & missing(subset.val)) {
#     fit <- cuminc(ftime, fstatus, group, cencode=cencode)
#   }
#   
#   else if (!(missing(subset.var) & missing(subset.val))) {
#     fit <- cuminc(ftime, fstatus, group, cencode=cencode,
#                   subset=subset.var==subset.val)
#   }
#   
#   #Copy current/default plotting parameters to reset after building plot
#   op <- par(no.readonly=TRUE)
#   
#   #Tick labels closer to axis, rotated text, wider left margin
#   par(oma=c(1, 1, 1, 1), 
#       mar=c(4 + (length(unique(group)) + length(unique(fstatus))), 4 + extra.left.margin, 4, 2) + 0.1, 
#       mgp=c(3, 0.5, 0), 
#       las=1, 
#       family=font.family)
#   
#   #Establish plot window
#   plot.new()
#   plot.window(c(min(xlim.major), max(xlim.major)), c(min(ylim.major), max(ylim.major)))
#   
#   #Establish gridlines if grid==T
#   if (grid==TRUE) {
#     abline(v=xlim.minor, lwd=lwd.grid.minor, col=col.grid.minor)
#     abline(h=ylim.minor, lwd=lwd.grid.minor, col=col.grid.minor)
#     abline(v=xlim.major, lwd=lwd.grid.major, col=col.grid.major)
#     abline(h=ylim.major, lwd=lwd.grid.major, col=col.grid.major)
#   }
#   
#   #Put a box around the plot
#   box(which="plot", lty="solid")
#   
#   #Include axes and labels
#   axis(1, at=xlim.major, tck=-0.018, cex.axis=axis.label.size)
#   axis(1, at=xlim.minor, tck=-0.01, labels=FALSE)
#   axis(2, at=ylim.major, tck=-0.018, cex.axis=axis.label.size)
#   axis(2, at=ylim.minor, tck=-0.01, labels=FALSE)
#   title(xlab=x.axis.label, line=1.75)
#   title(ylab=y.axis.label, line=2.25)
#   
#   #Give these multiple options with defaults
#   title(plot.title, cex.main=plot.title.size, font.main=1)
#   mtext(plot.subtitle, line=0.25, cex=plot.subtitle.size)
#   
#   #Overlay CIFs
#   par(new=TRUE)
#   
#   plot.cuminc(fit,
#               ylim=c(min(ylim.major), max(ylim.major)),
#               xlim=c(min(xlim.major), max(xlim.major)),
#               xlab="",
#               ylab="",
#               xaxt="n",
#               yaxt="n",
#               wh=c(0, 3),
#               col=col,
#               lty=lty,
#               lwd=lwd)
#   
#   #Legend options
#   if (legend==TRUE) {
#     legend(x=legend.x.loc, 
#            y=legend.y.loc, 
#            legend=group.names[group.order], 
#            col=col[group.order],
#            lty=lty[group.order],
#            lwd=lwd[group.order],
#            cex=axis.label.size,
#            bty="o", 
#            bg="white",
#            inset=0.01)
#   } 
#   
# }



#Input should include the various inputs to cuminc so we don't have to deal with
#extracting lists from the cuminc class output

#Competing risks plot
#Pretend transplant and death are competing risks
#pbc.cr <- na.omit(pbc[, c("time", "status", "trt")])
#test <- cuminc(ftime=pbc.cr$time, fstatus=pbc.cr$status, group=pbc.cr$trt, cencode=0)

#fit must be a cuminc object for CIFs from competing risks data

#After plotting grid, overlay CIFs
#par(new=T)

#If only one failure type should be plotted for a competing risks model, run the following:
# if (!is.null(failtype)) {
#   cs.cuminc <- function(x, cause=failtype){
#     if (!is.null(x$Tests)) 
#       x <- x[names(x) != "Tests"]
#     which.out <- which(unlist(strsplit(names(x), " "))[seq(2,length(names(x))*2,2)]!=cause)
#     x[which.out] <- NULL
#     class(x) <- "cuminc"
#     return(x)
#   }
# }
