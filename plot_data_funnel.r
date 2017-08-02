# generates a plot representing attritionof servers for stricter criteria

### initialize globals
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
pathStaid <- '/Users/sfrey/projecto_staid/minecraft/'
source(paste0(pathLocal,"local_settings.R"))
source(paste0(pathLocal,"lib_step6_analysis.r"))
###  libraries
library(magrittr)
library(ggforce)

# COLLECT DATA
counts <- rep(0,5)
#FIRST Total collected: 376576
#wc -l /Users/sfrey/projecto_staid/minecraft/20161114/master_ip_list.txt
ttl_command <- paste0("wc -l ", pathStaid, "20161114/master_ip_list.txt  | awk '{print $1}'")
counts[1] <- system(ttl_command, intern=TRUE) %>% as.numeric()
#SECOND pingable: 147815
spings <- readRDS(paste0(pathData, "step5_serversweeks.rds"))
counts[2] <- spings[,length(unique(srv_addr))]
# THIRD up at least a month 85000
sfeat <- buildPickDependent(spings, dependent='ncomm4visits')
counts[3] <- sfeat[,length(unique(srv_addr))]
#FOURTH gave plugins: 9709
spings <- readRDS(paste0(pathData, "step5_serversweeks.rds"))
splugins <- readRDS(paste0(pathData, "step5_serversweeksplugins.rds"))
pluginstats <- as.data.table(read.csv(paste0(pathData, 'step45_curse_plugins_metadata_full.csv')))
sfeat <- buildPickDependent(spings, dependent='ncomm4visits')
sfeat <- buildFeatureTable(sfeat, splugins, pluginstats)
mc <- sfeat
n_servers <- mc[,length(unique(srv_addr))]
featureCountMin=max(2, as.integer(n_servers/1000))
mc <- filterDataSetDownDataProvision(mc, featureCountMin=featureCountMin, keepFeatTypes=c('plugin', 'property'), keepDataSource=c('reddit', 'omni', 'mcs_org'))
counts[4] <- mc[,length(unique(srv_addr))]
#FIFTH basic functioning: 5280
mc <- sfeat
mc <- filterDataSetDown(mc, cutUnrealistic=TRUE, cutNonVanilla=TRUE, cutNonPositiveDependent=FALSE, featureCountMin=featureCountMin, keepFeatTypes=c('plugin', 'property'), keepDataSource=c('reddit', 'omni', 'mcs_org'))
counts[5] <- mc[,length(unique(srv_addr))]
counts 
labels <- c( "Total collected", "Pingable", "\u22651 month", "Governance info", "Basic play")
fnl <- data.frame(cbind(count=counts, label=labels ))

### FAKE DATA
if(0){
	#376576  # total collected
	#147815  # responded to pings at least once between dates  
	#105292  # made governance data available through API
	#58208   # were up for at least one month
	#5280    # amateur run
	counts <- c(376576, 147815, 105292, 58208, 5280)
	fnl <- data.frame(cbind(count=counts, label=labels ))
}

### GENEREATE PLOT
# https://github.com/wch/ggplot2/wiki/New-theme-system
theme_empty <- theme_bw()
theme_empty$line <- element_blank()
theme_empty$rect <- element_blank()
theme_empty$strip.text <- element_blank()
theme_empty$axis.text <- element_blank()
theme_empty$plot.title <- element_blank()
theme_empty$axis.title <- element_blank()
theme_empty$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, class = "unit")
theme_empty$legend.position <- "none"

#https://rdrr.io/cran/ggforce/man/geom_circle.html
y0 = seq(from=5, to=1)
circles <- data.frame(
  x0 = rep(0,5),
  y0 = y0,
  r = seq(5, 1, length.out = 5)
)
# Use coord_fixed to ensure true circularity
cp <- ggplot() + geom_circle(aes(x0=x0, y0=y0, r=r, alpha=100/y0), fill="light blue", size=0, data=circles) +
  coord_fixed() + theme_empty
cp <- cp + geom_text(data=fnl, aes(label=label, x=0, y=y0*2), vjust=3.0, size=3.5 ) + geom_text(data=fnl, aes(label=count, x=0, y=y0*2), vjust=4, size=5 )
ggsave(cp, file=paste0(pathImages, "plot_data_funnel.png"), units='cm', width=2.5, height=2.5, scale=4)
