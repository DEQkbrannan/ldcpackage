scatter.bacteria.flow.ts.plot.with.shading <- function(ldc=NULL,
                                                       ldc.crit=NULL,
                                                       est.flow=NULL,
                                                       plot.fn=NULL,
                                                       x.lim.scatter=NULL,
                                                       y.lim.scatter.bacteria=NULL,
                                                       y.lim.flow=NULL) {

  ## check for correct input
  if(is.null(ldc) | is.null(ldc.crit) | is.null(est.flow)) {
    print("Check input! Functions requires ldc. ldc.crit, and est.flow")
    return()
  }

  ## set output for plot
  ## if no filename, output sent to current device
  if(is.null(plot.fn ) != TRUE) {
    pdf(file=plot.fn,
        width=11,heigh=8.5,onefile=FALSE,title="",
        paper="special", bg="white")
  }
  ## set plot margins to accomodate the secondary
  ## y-axis
  par(mar=c(5, 4, 4, 4) + 0.1)
  ## get x limts from dates of ldc and flow-ts
  if( is.null(x.lim.scatter)) {
    x.lim.scatter <- as.POSIXlt(
      c(min(c(range(ldc.crit$ldc.max$Date),
              range(ldc$points$Date),
              range(est.flow$date))),
        max(c(range(ldc.crit$ldc.max$Date),
              range(ldc$points$Date),
              range(est.flow$date)))
      )
    )
    x.lim.scatter[1]$year <- x.lim.scatter[1]$year - 2
    x.lim.scatter[2]$year <- x.lim.scatter[2]$year + 1
    x.lim.scatter <- as.Date(x.lim.scatter)
  }
  ## get limits for the bacteria axis
  if(is.null(y.lim.scatter.bacteria)) {
    y.lim.scatter.bacteria <- c(100,10)
    if(y.lim.scatter.bacteria[1] > 10^floor(log10(min(ldc$points$ecoli,na.rm=TRUE)))) {
      y.lim.scatter.bacteria[1] = 10^floor(log10(min(ldc$points$ecoli,na.rm=TRUE)))
    }
    if(y.lim.scatter.bacteria[2] < 10^ceiling(log10(max(ldc$points$ecoli,na.rm=TRUE)))) {
      y.lim.scatter.bacteria[2] = 10^ceiling(log10(max(ldc$points$ecoli,na.rm=TRUE)))
    }
  }
  ## get flow subsets based in the date range of ldc$points
  ## get flow subsets based in the x-axis range
  flow.lim.subset <- as.POSIXlt(x.lim.scatter)
  flow.lim.subset[1]$year <- flow.lim.subset[1]$year + 2
  flow.lim.subset[2]$year <- flow.lim.subset[2]$year - 1
  flow.lim.subset <- as.Date(flow.lim.subset)

  subset.ldc.crit.flow <- subset(ldc.crit$ldc.max, Date >= flow.lim.subset[1] & Date <= flow.lim.subset[2])
  row.names(subset.ldc.crit.flow) <- 1:length(subset.ldc.crit.flow$Date)
  subset.est.flow <- subset(est.flow, date >= flow.lim.subset[1]  & date <= flow.lim.subset[2])
  row.names(subset.est.flow) <- 1:length(subset.est.flow$date)

  ## create temporary data.frame for flow
  tmp.flow <- data.frame(Date=subset.ldc.crit.flow$Date,
                         flow=subset.ldc.crit.flow$flow,
                         stringsAsFactors=FALSE)
  tmp.flow <- tmp.flow[order(tmp.flow$Date),]
  if(is.null(y.lim.flow)) {
    ## get limits for flow axis
    y.lim.flow <- c(100,10)
    if(y.lim.flow[1] > 10^floor(log10(min(tmp.flow$flow,na.rm=TRUE)))) {
      y.lim.flow[1] = 10^floor(log10(min(tmp.flow$flow,na.rm=TRUE)))
    }
    if(y.lim.flow[2] < 10^ceiling(log10(max(tmp.flow$flow,na.rm=TRUE)))) {
      y.lim.flow[2] = 10^ceiling(log10(max(tmp.flow$flow,na.rm=TRUE)))
    }
  }
  ## get flow zones
  flow.zone.exceed <- c(10,40,60,90,100)
  flow.zone.number <- c(1,2,3,4,5)
  flow.zone.names <- c("High Flows", "Transitional Flows",
                       "Typical Flows","Low Flows","Dry Flows")
  flow.zone.ts <- data.frame(date=subset.est.flow$date,
                             flow=subset.est.flow$value,
                             exceed=subset.est.flow$flow.exceed,
                             flow.zone.exceed=-1,
                             flow.zone.number=-1,
                             flow.zone.name="junk",
                             stringsAsFactors=FALSE)

  for(ii in 1:length(flow.zone.names)) {
    if(flow.zone.names[ii] == "High Flows") {
      cur.indexes <- as.numeric(row.names(
        subset.est.flow[subset.est.flow
                        $flow.exceed <= flow.zone.exceed[ii],]))
    }
    if(flow.zone.names[ii] == "Transitional Flows" |
       flow.zone.names[ii] == "Typical Flows" |
       flow.zone.names[ii] == "Low Flows" |
       flow.zone.names[ii] == "Dry Flows") {
      cur.indexes <- as.numeric(row.names(
        subset.est.flow[subset.est.flow$flow.exceed > flow.zone.exceed[ii-1] &
                          subset.est.flow$flow.exceed <= flow.zone.exceed[ii],]))
    }
    flow.zone.ts$flow.zone.exceed[cur.indexes] <- flow.zone.exceed[ii]
    flow.zone.ts$flow.zone.number[cur.indexes] <- flow.zone.number[ii]
    flow.zone.ts$flow.zone.name[cur.indexes]   <- flow.zone.names[ii]
    rm(cur.indexes)
  }
  ## set up plot plot using bacteria observations
  ##x.lim.scatter[1] <- x.lim.scatter[1] + 3*365
  plot(ldc$points$Date,ldc$points$ecoli,
       xlim=x.lim.scatter,ylim=y.lim.scatter.bacteria,
       ylab="E.coli (org/100 ml)", log="y",xlab="Date",cex=0)
  ## plot flow zone as plot bacground
  x.left <- flow.zone.ts$date[1:length(flow.zone.ts$date)-1]
  x.right <- flow.zone.ts$date[2:length(flow.zone.ts$date)]
  y.bottom <- par("yaxp")[1]
  y.top <- par("yaxp")[2]
  z.col <- 1 - flow.zone.ts$flow.zone.number/max(flow.zone.number)
  rect(xleft=x.left,ybottom=y.bottom,
       xright=x.right,ytop=y.top,col=hcl(h=240*z.col,alpha=0.55),
       border=NULL,lty="blank")

  ## plot flow time-series on secondary y-axis
  par(new=TRUE)
  plot(tmp.flow$Date,tmp.flow$flow,xlim=x.lim.scatter,
       type="l",xaxt="n",yaxt="n",xlab="",ylab="",ylim=y.lim.flow,
       log="y",col="blue")
  mtext("Stream Flow (cfs)",side=4,line=3,col="blue")
  axis(4, col.axis="blue",col.lab="blue", col="blue")

  ## plot bacteria on primary y-axis
  par(new=T)
  ## plot digressions in red and bigger point
  ldc.yes.D <- subset(ldc$points,ldc$points$digression == 1)
  plot(ldc.yes.D$Date,ldc.yes.D$ecoli,
       xlim=x.lim.scatter,ylim=y.lim.scatter.bacteria,
       ylab="", log="y",xlab="",type="p",
       axes=F,pch=21,bg="red",cex=1.5)
  ## plot no digression in green and smaller point
  ldc.no.D <- subset(ldc$points,ldc$points$digression == 0)
  points(ldc.no.D$Date,ldc.no.D$ecoli,
         xlim=x.lim.scatter,ylim=y.lim.scatter.bacteria,
         pch=21,bg="green",cex=0.9)
  ## add criteria lines and text
  ##x.text <- par("xaxp")[1] - (par("xaxp")[2]-par("xaxp")[1])/(1.2*par("xaxp")[3])
  ##x.text<-x.lim.scatter[1]-250
  x.text<-x.lim.scatter[1]
  text(x.text,500,"Max\n406 org/100ml",
       cex=0.9, col="red",adj=c(0,0))
  abline(h=406, col="red",lwd=2, lty="dashed")
  text(x.text,150,"GeoMean\n126 org/100ml",
       cex=0.9,col="red",adj=c(0,0))
  abline(h=126, col="red",lwd=2, lty="dashed")
  ## if filename, close pdf device
  if(is.null(plot.fn ) != TRUE) {
    dev.off()
  }
}

flz.legend <-function(legend.fn=NULL, ldc=NULL,
                      x.lim.scatter = NULL,
                      y.lim.scatter = NULL
) {

  jpeg(filename=legend.fn, quality=100,
       pointsize=1/288)
  plot(ldc$points$Date,ldc$points$ecoli,
       xlim=x.lim.scatter,ylim=y.lim.scatter.bacteria,
       xlab="",ylab="",
       log="y",type="n", axes=FALSE)
  col.seq <- seq(from=range(x.lim.scatter)[1],
                 to=range(x.lim.scatter)[2],
                 by=(range(x.lim.scatter)[2]-range(x.lim.scatter)[1])/5)

  x.left.rec  <- col.seq[1:length(col.seq)-1]
  x.right.rec <- col.seq[2:length(col.seq)]
  y.bottom.rec <- y.lim.scatter[1]
  y.top.rec <- y.lim.scatter[2]


  rect(xleft=x.left.rec,ybottom=y.bottom.rec,
       xright=x.right.rec,ytop=y.top.rec,
       col=hcl(h=240*(1 - seq(1:5)/max(5)),alpha=0.55),
       border=NULL,lty="solid")
  col.labels <- c("High Flows","Transitional Flows",
                  "Typical Flows", "Low Flows", 'Dry Flows')
  for(ii in 1:5) {
    x.center <- x.left.rec[ii] + (x.right.rec[ii] - x.left.rec[ii])/2
    y.center <- y.top.rec - (y.top.rec - y.bottom.rec)/2
    text(x=x.center,y=y.center,labels=col.labels[ii], srt=-90,
         adj=c(0,0.5), cex=2.75)
  }
  dev.off()

}
