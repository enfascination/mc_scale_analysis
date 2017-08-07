library(boot)
library(scales)
library(ggthemes)
library(cowplot)

### plotting function
make_plot_size_by_success <- function(mwdata, fillvarscols, fillvarsfn, ggmore=geom_blank(), ggguide = guide_legend(reverse=TRUE), reps=0, return_plot=T, facetting=c(), unscaledyvar=TRUE, xvar="pop_size_factor", yvar="perf_factor", ggtext=TRUE, ggrug=TRUE, ...) {
    ###  this function makes the main plot of the paper in a few ways.
    ### data_prep_fn has to be bootstrap compliant, meaning it has two arguments, data and (possibly complete) indices for the data. data.table is amazing and can take null as the first argument, meaning you only have to pass one of the two arguments when you aren't bootstrapping
    ### ggmore lets you pass some aes-free ggplot elements as arguments for max  ease and customizability
    ### turn bootstrapping of stats off by setting reps==0
    if (unscaledyvar) plot_groups=c(yvar, xvar, facetting)
    else              plot_groups=c(yvar, xvar, facetting)
    if (reps>0) {
        mwd1 <- mwdata[,{
                        ttt <- boot(.SD[,c(fillvarscols), with=F], fillvarsfn,  R=reps, parallel = "multicore", ncpus = 8, ...); 
                        tttq <- unlist(quantile(ttt$t, c(0.995, 0.50, 0.005), names=FALSE, na.rm=TRUE));
                        #print(tttq)
                        list(pop_var=tttq[2], pop_var_low=tttq[3], pop_var_high=tttq[1])
                        #tttq[2]
        },by=plot_groups]
    } 
    else {
        mwd1 <- mwdata[,.(pop_var=fillvarsfn(.SD[,c(fillvarscols), with=F], ...)),by=plot_groups]
    }
    if (return_plot) {
        mwp1 <- ggplot(mwd1, aes_string(x=xvar, y=yvar))
        if (unscaledyvar) {
            mwp1 <- mwp1 + scale_y_discrete("Core members", expand = c(0.035,0))#, labels=c("0", "", "", "10", "", "", "100"))
        } else {
            mwp1 <- mwp1 + scale_y_discrete("Core members", expand = c(0.035,0))
        }
        mwp1 <- mwp1 + geom_bin2d(aes(fill=pop_var)) + theme_cowplot() + theme(panel.grid.major=element_line(0), axis.text.y = element_text(angle = 45), plot.title = element_text(hjust = 0.5)) + coord_fixed(ratio=6/7) + scale_x_discrete("Server size", expand = c(0.035,0) ) + guides(fill=ggguide) + ggmore
        if (ggtext != FALSE) {
			if (ggtext=="%") {
				mwp1 <- mwp1 + geom_text(aes(label=paste("",round(100*pop_var), '%', sep='')), color="dark grey")
			} else {
				mwp1 <- mwp1 + geom_text(aes(label=round(signif(pop_var, 2),2)), color="dark grey")
			}
        }
        if (ggrug) {
            #mwp1 <- mwp1 + geom_rug(data=mw_train, mapping=aes(x=log2(srv_max+1)/2-0.2, y=log2(y+1)/srv_max_log*0.83+1.1+rnorm(length(y),sd=0.05)), col=rgb(0.7,0.7,0.7,alpha=0.2),sides="tl") 
            mwp1 <- mwp1 + geom_rug(data=mw_train, mapping=aes(x=log2(srv_max+1)/2-0.2, y=log2(y+1)/srv_max_log*1.0+0.99+rnorm(length(y),sd=0.05)), col=rgb(0.7,0.7,0.7,alpha=0.2),sides="tl") 
        }
        return(mwp1)
    }
    else {
        return(mwd1)
    }
}

plot_visitortype <- function(mw, plot_type=c('vertical', 'horizontal', 'horizontal2', 'circular')) {
    ### proportion of users in small servers
    ### from http://stackoverflow.com/questions/13615562/ggplot-donut-chart#13636037
    ###  and http://mathematicalcoffee.blogspot.com/2014/06/ggpie-pie-graphs-in-ggplot2.html
    # Create test data.
    dat <- rbind(  mw[,.(vtype=0, y=sum(nuvisits12)), by=pop_size_factor]
                 , mw[,.(vtype=1, y=sum(y)), by=pop_size_factor]
                 )
    dat[, vtype:=factor(vtype, levels=c(0,1), labels=c("Unique visitors", "Core visitors"), ordered=TRUE)]
    setnames(dat, c("y","pop_size_factor"), c("count", "cat"))
    dat <- dat[order(vtype,cat)]
    dat[,fraction:=count/sum(count),by=vtype]
    dat[,ymax:=cumsum(fraction),by=vtype]
    dat[,ymin:=c(0, head(ymax, n=-1)),by=vtype]
    #dat[,cat:=factor(cat, labels=c("\u22645","\u226410","\u226450","\u2264100", "\u2264500", ">500"))]
    #mw[,pop_size_factor:=cut(log2(srv_max+1), breaks=c(0,2,4,6,12), labels=c("\u22644", "4 to 16", "16 to 64", "64 to 1024"), ordered_result=TRUE, right=TRUE)]

	#pct_labels <- paste(round(100*dat$ymax,0), '%', sep='')
	#pct_labels <- paste("—",signif(100*dat$ymax,2), '%', sep='')
	pct_labels <- paste("",signif(100*dat$fraction,2), '%', sep='')
	pct_labels <- ifelse(100*dat$fraction < 1, '<1%', pct_labels)
	#pct_labels[c(length(pct_labels)/2,length(pct_labels))] <- ''
	if(plot_type == "circular") {
		## polar plot
		plot_population_distribution <- ggplot(dat, aes(fill=cat, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
		 geom_rect() +
		 coord_polar(theta="y", start=-pi/2) +
		 xlim(c(0, 4)) +
		 ylim(c(0, 1.02)) +
		 geom_text(aes(label=pct_labels, x=3.5, y=ymax-fraction/2), size=3.5) +
		 geom_text(aes(label=cat, x=2.4, y=ymax-fraction/2), size=5) +
		 labs(title="") +
		 xlab("") + ylab("") +
		 facet_wrap( ~ vtype) +
		 theme_cowplot() +
		 theme(axis.ticks=element_blank()) +
		 theme(axis.text.y=element_blank()) +
		 theme(axis.text.x=element_blank()) +
		 theme(panel.grid=element_blank()) +
		 theme(panel.spacing=unit(0, "cm")) +
		 theme(plot.margin=unit(c(0,0,0,0), "cm")) +
		 theme(strip.text=element_text(size=18, family="sans")) +
		 guides(fill="none") +
		 scale_fill_brewer(direction=1, type='seq', palette="Oranges")
	} else if(plot_type == "vertical") {
        ### rectangle plot
        plot_population_distribution <- ggplot(dat, aes(fill=cat, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
            geom_rect() +
            #coord_polar(theta="y", start=-pi/2) +
            xlim(c(2.7, 4.23)) +
            ylim(c(0, 1.02)) +
            geom_text(aes(label=cat, x=3.35, y=ymax-fraction/2), size=4.0) +
            geom_text(aes(label=paste0("(",pct_labels,")"), x=3.86, y=ymax-fraction/2), size=3) +
			annotate("text", label="0%", x=4.15, y=0+0.003, size=3.5) +
			annotate("text", label="100%", x=4.15, y=1+0.003, size=3.5) +
            labs(title="") +
            xlab("") + ylab("") +
            facet_wrap( ~ vtype) +
            theme_cowplot() +
            theme(axis.ticks=element_blank()) +
            theme(axis.text.y=element_blank()) +
            theme(axis.text.x=element_blank()) +
            theme(panel.grid=element_blank()) +
            theme(panel.spacing=unit(0, "cm")) +
            theme(plot.margin=unit(c(0,0,0,0), "cm")) +
            theme(strip.text=element_text(size=18, family="sans")) +
            guides(fill="none") +
            scale_fill_brewer(direction=1, type='seq', palette="Oranges") 
    } else if(plot_type == "horizontal") {
    ### sideways
    plot_population_distribution <- ggplot(dat, aes(fill=cat, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
     geom_rect() +
     #coord_polar(theta="y", start=-pi/2) +
     xlim(c(2.7, 4.43)) +
     ylim(c(0, 1.02)) +
     geom_text(aes(label=cat, x=3.5, y=ymax-fraction/2), size=5) +
     geom_text(aes(label=pct_labels, x=3.5-0.25, y=ymax-fraction/2), size=3.5) +
     annotate("text", label="0%", x=4.15, y=0+0.003, size=3.5) +
     annotate("text", label="100%", x=4.15, y=1+0.003, size=3.5) +
     labs(title="") +
     xlab("") + ylab("") +
     facet_wrap( ~ vtype, ncol=1) +
     theme_cowplot() +
     theme(axis.ticks=element_blank()) +
     theme(axis.text.y=element_blank()) +
     theme(axis.text.x=element_blank()) +
     theme(panel.grid=element_blank()) +
     theme(panel.spacing=unit(0, "cm")) +
     theme(plot.margin=unit(c(0,0,0,0), "cm")) +
     theme(strip.text=element_text(size=18, family="sans")) +
     guides(fill="none") +
     scale_fill_brewer(direction=1, type='seq', palette="Oranges") + 
	 coord_flip()
    }
 return(plot_population_distribution )
}

### handle NAs thusly: https://stackoverflow.com/questions/17398044/how-can-i-vectorize-the-entropy-calculation
entropy_calc <- function(x) {entropy(x, method="ML")}
entropy_calc <- function(x) {sum(log(x^-x))}

### aggregation functions
gov_median <- function(x,i) median(as.double(asdf(x[i])[,1]))
gov_median_narm <- function(x,i) median(as.double(asdf(x[i])[,1]), na.rm=TRUE)
gov_mean <- function(x,i) mean(as.double(asdf(x[i])[,1]))
gov_mean_narm <- function(x,i) mean(as.double(asdf(x[i])[,1]), na.rm=TRUE)
gov_median_proportion_1 <- function(x,i) median(as.double(asdf(x[i])[,1]/asdf(x[i])[,2]))
gov_median_proportion_1_narm <- function(x,i) median(as.double(asdf(x[i])[,1]/asdf(x[i])[,2]), na.rm=TRUE)
gov_mean_proportion_1 <- function(x,i) mean(as.double(asdf(x[i])[,1]/asdf(x[i])[,2]))
gov_median_proportion_2 <- function(x,i, focal) {
    median( asdf(x[i,focal,with=F])[,1]/rowSums(x[i]) , na.rm=TRUE)
}
gov_mean_proportion_2 <- function(x,i, focal) {
    mean( asdf(x[i,focal,with=F])[,1]/rowSums(x[i]) , na.rm=TRUE)
}
gov_max <- function(x,i) max(as.double(asdf(x[i])[,1]))
gov_sum <- function(x,i) sum(as.double(asdf(x[i])[,1]))
gov_var <- function(x,i) var(as.double(asdf(x[i])[,1]))
gov_var_controlled <- function(x,i) var(rescale(as.double(asdf(x[i])[,1])))
gov_var_diversity <- function(data, i_samp) {
    samp_size <- ncol(data) ### this is a bit les unproper, but 
    samp_size <- 3  ### this lets me keep at least one bin in the upper-right bin of the 2D histogram
    if(nrow(data[i_samp]) < samp_size) {
        diversity <- numeric()
    }
    else {
        ### I'm subsampling within boot to take small samples and thereby 
        ###  control slightly less badly for the different sizes in each bin
        gg <- data[sample(i_samp, 1000, replace=TRUE),] 
        gg <- t(apply(gg, 1, function(x) x/sum(x)))  ### turn counts by type into a distribution
        inst_vars<-apply(gg, 2, var)  
        diversity <- mean(inst_vars)
    }
    return(diversity )
}
gov_entropy_diversity <- function(data, i_samp) {
    samp_size <- ncol(data) ### this is a bit les unproper, but 
    samp_size <- 3  ### this lets me keep at least one bin in the upper-right bin of the 2D histogram
    #entropy_calc <- function(x) {sum(-x*log(x))}
    if(nrow(data[i_samp]) < samp_size) {
        diversity <- numeric()
    }
    else {
        ### I'm subsampling within boot to take small samples and thereby 
        ###  control slightly less badly for the different sizes in each bin
        gg <- data[sample(i_samp, 1000, replace=TRUE),] 
        gg <- apply(gg, 1, function(x) x/sum(x))  ### turn counts by type into a distribution
        inst_vars<-apply(gg, 2, entropy_calc)  
        diversity <- mean(inst_vars, na.rm=TRUE)
    }
    return(diversity )
}
#library(devtools)
#devtools::install_github("tillbe/jsd")
library(jsd)
library('proxy')
gov_dist <- function(data, i_samp) {
    samp_size <- ncol(data) ### this is a bit les unproper, but 
    samp_size <- 3  ### this lets me keep at least one bin in the upper-right bin of the 2D histogram
    n <- 10
    if(nrow(data[i_samp]) < samp_size) {
        diversity <- numeric()
    }
    else {
        dists <- rep(0, n)
        for (i in 1:n) {
            idxs <- sample(i_samp, 2) #i_sampl should come shuffled, but just in case ...
            #dists[i] <- hamming( data[ idxs[1], ] , data[ idxs[2], ] )
            #dists[i] <- dist( data[ idxs, ] , method="cosine")
            #dists[i] <- dist( apply(data[ idxs, ] + 1, 1, prop.table) , method="Kullback")
            #dists[i] <- dist( data[ idxs, ] , method="Manhattan")
            dists[i] <- dist( data[ idxs, ] , method="simple matching")
            #dists[i] <-  data[idxs,] %>% apply(2, diff) %>% abs() %>% sum() ## "taxicab" distance or L1
            #dists[i] <- ( data[ idxs[1], ] - data[ idxs[2], ] ) %>% abs() %>% log1p() %>% sum()
            #dists[i] <- ( data[ idxs[1], ] - data[ idxs[2], ] ) %>% abs() %>% (function(x){ifelse(x==0,0,1)})() %>% sum()
        }
        #diversity <- mean(dists) 
        diversity <- median(dists) 
    }
    return(diversity )
}
L1 <- function(d1, d2) {( d1 - d2 ) %>% abs() %>% log1p() %>% sum() %>% return() }
hamming <- function(d1, d2) {(d1 - d2) %>% abs() %>% (function(x){ifelse(x==0,0,1)})() %>% sum() %>% return() }
logL1 <- function(d1, d2) {( d1 - d2 ) %>% abs() %>% log1p() %>% sum() %>% return() }
SJD <- function(d1, d2) {JSD( d1, d2 )^0.5 %>% return() }
cosine_dist <- function(d1, d2) {
    d1l <- (d1*d1) %>% sum() %>% sqrt()
    d2l <- (d2*d2) %>% sum() %>% sqrt()
    return( sum( d1 * d2 )/(d1l * d2l ) ) 
}
euclidian <- function(d1, d2) {sum((d1 - d2)^2)^0.5 %>% return() }
minkowski <- function(d1, d2, p) {sum(abs(d1 - d2)^p)^(1/p) %>% return() }
