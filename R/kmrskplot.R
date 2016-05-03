##########################################
# KAPLAN-MEIER PLOTS FOR CLASS 'SURVFIT' #
##########################################

#fit must be a survfit object for KM plots
km.plot <- function(fit=fit,
                    type="km",
                    xlim.major=pretty(fit$time),
                    xlim.minor=((pretty(fit$time) + 
                                   c(NA, pretty(fit$time)[-length(pretty(fit$time))]))/2)[-1],
                    ylim.major=pretty(c(0, 1), 5),
                    ylim.minor=((pretty(c(0, 1), 5) + 
                                   c(NA, pretty(c(0, 1), 5)[-length(pretty(c(0, 1), 5))]))/2)[-1],
                    grid=TRUE,
                    col.grid.major="gray90",
                    col.grid.minor="gray97",
                    lwd.grid.major=0.5,
                    lwd.grid.minor=0.2,
                    
                    #Info for plotting data
                    conf.int=FALSE,
                    mark.time=FALSE,
                    #These values should recycle
                    col=rep("black", length(fit$strata)),
                    lty=rep(1, length(fit$strata)),
                    lwd=rep(1, length(fit$strata)),
                    
                    #Labels
                    plot.title="",
                    plot.title.size=1.25,
                    plot.subtitle="",
                    plot.subtitle.size=1,
                    x.axis.label="Time",
                    y.axis.label="Probability",
                    
                    axis.label.size=0.9,
                    #Additional space to accomodate long labels
                    extra.left.margin=3,
                    font.family="serif",
                    
                    #Risk table options
                    print.risk.table=TRUE,
                    print.group.lines=TRUE,
                    print.group.names=TRUE,
                    risk.table.title="Total at risk",
                    
                    #Legend options
                    group.names=names(fit$strata),
                    group.order=seq(length(fit$n)),
                    legend=FALSE,
                    legend.x.loc="bottomleft",
                    legend.y.loc=NULL
                    
)

{
  #Copy current/default plotting parameters to reset after building plot
  op <- par(no.readonly=TRUE)
  
  #Tick labels closer to axis, rotated text, wider left margin
  par(oma=c(1, 1, 1, 1), 
      mar=c(4 + length(fit$strata), 4 + extra.left.margin, 4, 2) + 0.1, 
      mgp=c(3, 0.5, 0), 
      las=1, 
      family=font.family)
  
  #Establish plot window
  plot.new()
  plot.window(c(min(xlim.major), max(xlim.major)), c(min(ylim.major), max(ylim.major)))
  
  #Establish gridlines if grid==T
  if (grid==TRUE) {
    abline(v=xlim.minor, lwd=lwd.grid.minor, col=col.grid.minor)
    abline(h=ylim.minor, lwd=lwd.grid.minor, col=col.grid.minor)
    abline(v=xlim.major, lwd=lwd.grid.major, col=col.grid.major)
    abline(h=ylim.major, lwd=lwd.grid.major, col=col.grid.major)
  }
  
  #Put a box around the plot
  box(which="plot", lty="solid")
  
  #Include axes and labels
  axis(1, at=xlim.major, tck=-0.018, cex.axis=axis.label.size)
  axis(1, at=xlim.minor, tck=-0.01, labels=FALSE)
  axis(2, at=ylim.major, tck=-0.018, cex.axis=axis.label.size)
  axis(2, at=ylim.minor, tck=-0.01, labels=FALSE)
  title(xlab=x.axis.label, line=1.75)
  title(ylab=y.axis.label, line=2.25)
  
  #Give these multiple options with defaults
  title(plot.title, cex.main=plot.title.size, font.main=1)
  mtext(plot.subtitle, line=0.25, cex=plot.subtitle.size)
  
  #Print a risk table beneath the plot if requested
  if (print.risk.table==TRUE) {
    #Print group names in bottom plotting margin
    group.pos <- (par()$usr[2] - par()$usr[1])/(-8)  
    pad <- abs(group.pos/8)
    line.pos <- (1:length(group.names))[order(group.order)] + 2
    
    if (print.group.names==TRUE) {
      mtext(group.names, side=1, line=line.pos, at=group.pos, 
            adj=1, col=1, las=1, cex=axis.label.size )
    }
    
    #Print lines matching plot in bottom plotting margin  
    if (print.group.lines==TRUE) {
      par('xpd'=TRUE)
      for (i in 1:length(group.names)) {
        axis(side=1, at=c(group.pos + pad, 0 - 2*pad), 
             labels=FALSE, 
             line=line.pos[i] + 0.6, 
             lwd.ticks=0,
             col=col[i], lty=lty[i], lwd=lwd[i]) 
      }
    }
    
    #Extract risk groups
    rsk <- summary(fit, times=xlim.major)  
    #if(length(kms$strata)==1) kms$strata <- rep(1,length(kms$time) )
    rskdat <- data.frame(time=rsk$time, 
                         n.risk=rsk$n.risk, 
                         strata=c(rsk$strata))
    datsplit <- split(rskdat, f=rskdat$strata)
    
    #Print right-justified risk table values beneath plot
    ndigits <- lapply(datsplit, function(x) nchar(x[,2]))
    mat <- do.call('rbind', 
                   lapply(ndigits, 
                          function(z){ 
                            length(z) <-  max(sapply(ndigits, length)); z
                          } 
                   ) 
    )
    matapply <- apply(mat, 2, max, na.rm=T)
    for( i in seq(length(fit$strata)) ){
      iter <- datsplit[[i]] 
      w.adj <- strwidth('0', cex=axis.label.size, font=par('font')) / 
        2*matapply[1:nrow(iter)]
      mtext(side=1,
            at=iter$time + w.adj, 
            text=iter$n.risk, 
            line=line.pos[i], 
            cex=axis.label.size, adj=1, col=1, las=1)
    }
    
    #Header for risk table
    mtext(side=1, 
          text=risk.table.title, 
          at=group.pos, 
          line=1.5, adj=1, col=1, las=1, cex=axis.label.size)
  }
  
  #Add the designated type of plot below
  
  #Standard KM plot
  if (type=="km") {
    lines(fit, 
          conf.int=conf.int,
          mark.time=mark.time,
          col=col,
          lty=lty,
          lwd=lwd,
          xmax=max(xlim.major),
          ymin=min(ylim.major))
  }
  
  #KM complement plot
  else if (type=="1-km") {
    lines(fit, 
          fun=function(x) 1-x,
          conf.int=conf.int,
          mark.time=mark.time,
          col=col,
          lty=lty,
          lwd=lwd,
          xmax=max(xlim.major),
          ymin=min(ylim.major))
  }
  
  #Legend options
  if (legend==TRUE) {
    legend(x=legend.x.loc, 
           y=legend.y.loc, 
           legend=group.names[group.order], 
           col=col[group.order],
           lty=lty[group.order],
           lwd=lwd[group.order],
           cex=axis.label.size,
           bty="o", 
           bg="white",
           inset=0.01)
  }  
  
  #Reset original plotting parameters
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
