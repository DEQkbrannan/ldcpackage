##
## station list
stns <- c(11476, 12301, 33112, 34454, 34455, 34456)

## path to ldc estimates
pathLDC <- 'C:/Users/kbranna/OneDrive - Oregon/Documents/Upper Yaquina River/Results/UpperYaquinaRiver/estimates'
##
## ldc filename
fileLDC <- 'ldc_st11476.RData'
paste0(pathLDC, '/', fileLDC)
##
## load data
load(file = paste0(pathLDC, '/', fileLDC))
##
## get station name
strStat <- strsplit(strsplit(fileLDC, "[.]")[[1]][1],'[_]')[[1]][2]
##
## path for plot files
pathPlot <- 'C:/Users/kbranna/OneDrive - Oregon/Documents/Upper Yaquina River/Results/UpperYaquinaRiver/graphs/new'
##
## plot file names
fileGFDC <- paste0("plot_fdc_", strStat, ".png")
fileGLDC <- paste0("plot_ldc_", strStat, ".pdf")
##
## plot FDC
fdc.ss.est.flow.plot(flow.exceed = flow.est.st11476$flow.exceed,
                     flow.est = flow.est.st11476$value,
                     ss.est = fdc.ss.st11476,
                     y.lims = NULL,
                     plot.fn = paste0(pathPlot, '/', fileGFDC))
##
## plot LDC
ldc.plot(ldc.stn = ldc.st11476,
         ldc.crit = ldc.crit.st11476,
         y.lims = NULL,
         plot.fn = paste0(pathPlot, '/', fileGLDC))

