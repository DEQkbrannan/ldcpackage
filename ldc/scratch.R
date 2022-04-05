##
## station list
stns <- c(11476, 12301, 33112, 34454, 34455, 34456)
for(ii in 1:6) {
  stn <- stns[ii]

  rm(list = ls(pattern='fdc[.]ss[.]st*|ldc[.]st*|ldc[.]crit*|flow[.]est*|fileG*'))

  ## path to ldc estimates
  pathLDC <- 'C:/Users/kbranna/OneDrive - Oregon/Documents/Upper Yaquina River/Results/UpperYaquinaRiver/estimates'
  ##
  ## ldc filename
  fileLDC <- paste0('ldc_st', stns[ii], '.RData')
  ##
  ## load data
  load(file = paste0(pathLDC, '/', fileLDC))
  ##
  ## set temporary data
  eval(parse(text = paste0('tmp.flow.est = flow.est.st',stn)))
  eval(parse(text = paste0('tmp.fdc.ss = fdc.ss.st',stn)))
  eval(parse(text = paste0('tmp.ldc.crit = ldc.crit.st',stn)))
  eval(parse(text = paste0('tmp.ldc.st = ldc.st',stn)))
  ##
  ## get station name
  ##strStat <- strsplit(strsplit(fileLDC, "[.]")[[1]][1],'[_]')[[1]][2]
  ##
  ## path for plot files
  pathPlot <- 'C:/Users/kbranna/OneDrive - Oregon/Documents/Upper Yaquina River/Results/UpperYaquinaRiver/graphs/new'
  ##
  ## plot file names
  fileGFDC <- paste0("plot_fdc_", stns[ii], ".png")
  fileGLDC <- paste0("plot_ldc_", stns[ii], ".png")
  fileGSCT <- paste0("plot_sct_", stns[ii], ".png")
  ##
  ## plot FDC
  fdc.ss.est.flow.plot(flow.exceed = tmp.flow.est$flow.exceed,
                       flow.est = tmp.flow.est$value,
                       ss.est = tmp.fdc.ss,
                       y.lims = NULL,
                       plot.fn = paste0(pathPlot, '/', fileGFDC))
  ##
  ## plot LDC
  ldc.plot(ldc.stn = tmp.ldc.st,
           ldc.crit = tmp.ldc.crit,
           y.lims = NULL,
           plot.fn = paste0(pathPlot, '/', fileGLDC))
  ##
  ## scatter plot
  scatter.bacteria.flow.ts.plot.with.shading(ldc =tmp.ldc.st,
                                             ldc.crit = tmp.ldc.crit,
                                             est.flow = tmp.flow.est,
                                             plot.fn = paste0(pathPlot, '/', fileGSCT))


}
