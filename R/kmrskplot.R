#############################
# Establish plotting region #
#############################

#fit must be a survfit object for KM plots
#or cuminc object for competing risks plots
kmrsk.plot <- function(fit=fit, #Required 
                       #Plot types: "km", "1-km", "cmprsk", "1-cmprsk"
                       type=NULL, #Required
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
                       
                       axis.label.size=0.9,
                       #Additional space to accomodate long labels
                       extra.left.margin=3,
                       
                       #Risk table options
                       print.risk.table=TRUE,
                       print.group.lines=TRUE,
                       print.group.names=TRUE,
                       risk.table.title="Total at risk",
                       
                       #Legend options
                       group.names=names(fit$strata),
                       group.order=seq(length(fit$n))
)

{
  #Copy current/default plotting parameters to reset after building plot
  op <- par(no.readonly=TRUE)
  
  #Tick labels closer to axis, rotated text, wider left margin
  par(oma=c(1, 1, 1, 1), 
      mar=c(4 + length(fit$strata), 4 + extra.left.margin, 4, 2) + 0.1, 
      mgp=c(3, 0.5, 0), 
      las=1, 
      family="serif")
  
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
  
  
  ###########################
  # Include axes and labels #
  ###########################
  
  axis(1, at=xlim.major, tck=-0.018, cex.axis=axis.label.size)
  axis(1, at=xlim.minor, tck=-0.01, labels=FALSE)
  axis(2, at=ylim.major, tck=-0.018, cex.axis=axis.label.size)
  axis(2, at=ylim.minor, tck=-0.01, labels=FALSE)
  
  #Give these multiple options with defaults
  title(plot.title, cex.main=plot.title.size, font.main=1)
  mtext(plot.subtitle, line=0.25, cex=plot.subtitle.size)
  
  #################################
  # Print risk table beneath plot #
  #################################
  
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
  
  ###########
  # KM plot #
  ###########
  
  if (type=="km") {
    lines(fit, 
          conf.int=conf.int,
          mark.time=mark.time,
          col=col,
          lty=lty,
          lwd=lwd,
          xmax=max(xlim.major),
          ylim=min(ylim.major))
  }
  
  
  ######################
  # KM complement plot #
  ######################
  
  else if (type=="1-km") {
    lines(fit, 
          fun=function(x) 1-x,
          conf.int=conf.int,
          mark.time=mark.time,
          col=col,
          lty=lty,
          lwd=lwd,
          xmax=max(xlim.major),
          ylim=min(ylim.major))
  }
  
  #######################
  # Competing risk plot #
  #######################
  
  ##################################
  # Competing risk complement plot #
  ##################################
  
  par(op)
}
