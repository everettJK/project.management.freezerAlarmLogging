library(XML)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(grDevices)

maxDataPoints <- 500
sourcePath    <- '/home/everett/software/freezerSensorTracker'
credFile      <- '/home/everett/.freezerAlarmCreds'  # chmod 600
outputDir     <- '/media/lorax/data/export/freezerMonitor'

cred <- scan(what = 'character', file = credFile)
file.remove(file.path(sourcePath, 'tracker.tmp'))
Sys.sleep(5)
system(paste0('wget --read-timeout 10 -O ', file.path(sourcePath, 'tracker.tmp'), ' --user ', cred[1], ' --password ', cred[2], ' http://microb146.med.upenn.edu/y.xml'), intern = FALSE)
x  <- xmlToList(xmlParse(file.path(sourcePath, 'tracker.tmp')))
d  <- system('date +%s', intern = TRUE)

# Parse data from relevant sensors.
o <- lapply(x$channels, function(x){
  if(x$general$name %in% c('Power', 'Battery')) return(NULL)
  if(x$general$name != 'Unconfigured') return(paste0(d, '\t', x$general$name, '\t', gsub('[CF]', '', x$runtime$info$val)))
  return(NULL)
})


# Write out data.
write(unlist(o[! sapply(o, is.null)]), file = file.path(sourcePath, 'data', d))


# Remove older data points.
files <- gtools::mixedsort(list.files(file.path(sourcePath, 'data')))
if(length(files) > maxDataPoints){
  file.remove(file.path(sourcePath, 'data', files[1:(length(files) - maxDataPoints)]))
}


# Parse log files, parse epoch time to human readable and sort on reading date.
files <- gtools::mixedsort(list.files(file.path(sourcePath, 'data')))
files <- file.path(sourcePath, 'data', files)
data <- bind_rows(sapply(files, function(x) read.table(x, sep = '\t', comment.char = ''), simplify = FALSE))
data$date <- sapply(data$V1, function(x) as.character(as.POSIXct(x, origin="1970-01-01")))
data <- arrange(data, V1)


# Define axis labels.
axisBreaks = unique(data$V1)
axisLabels <- data[match(axisBreaks, data$V1),]$date
i <- seq(1, length(axisBreaks), 20)
axisBreaks <- axisBreaks[i]
axisLabels <- axisLabels[i]


# Create and export plot.
p <- ggplot(data, aes(x = V1, y = V3, color = V2, group = V2)) +
     scale_color_manual(name = 'Unit', values = colorRampPalette(brewer.pal(12, "Paired"))(n_distinct(data$V2))) +
     theme_bw() +
     geom_point() +
     geom_line() +
     scale_x_continuous(breaks = axisBreaks, labels = axisLabels) + 
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
     labs(x = '', y = 'Temperature (C)') +
     ggtitle(paste0('Last update: ', date()))

ggsave(file.path(outputDir, 'freezerMonitorPlot.png'), p, width = 8, height = 8, units = c("in"))
