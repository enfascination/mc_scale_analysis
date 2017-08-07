#http://scc.stat.ucla.edu/page_attachments/0000/0140/reg_2.pdf
#Prior to any analysis, the data should always be inspected for:
#Data-entry errors
#Missing values
#Outliers
#Unusual (e.g. asymmetric) distributions
#Changes in variability of data (output over input)
#Clustering
#Non-linear bivariate relatioships
#Unexpected patterns
## during/ after:
# distribution of residualsk
https://stats.stackexchange.com/questions/75054/how-do-i-perform-a-regression-on-non-normal-data-which-remain-non-normal-when-tr
http://www.isixsigma.com/tools-templates/normality/dealing-non-normal-data-strategies-and-tools/
	http://people.duke.edu/~rnau/testing.htm
http://www.basic.northwestern.edu/statguidefiles/linreg_ass_viol.html
http://pareonline.net/getvn.asp?n=2&v=8

### google roy bargman stedown
### constrast coding of manova

### variables
### admin choices
serv_max: size aka pressure?
entropy of trinomial dist: resource complexity aka resource regime variety aka target variety governance scope
total_res : governance instensity
median(feat_count) : specialization
entropy(style) : rule diversity
%admin focused: consolidation
### results
core : community size
success : success (core/size)
uniques
longevity
### covariates and onctorls:
###     srv_details
###     srv_details
weeks_up_todate  ### in case plugin count
week_up_total
dataset_reddit + dataset_mcs_org
date_ping_int
"date_ping_1st"              "date_ping_lst" 
srv_retired

### thought models:
success ~ dataset_reddit + dataset_mcs_org + data_ping_int + date_point_1st + srv_retired + weeks_up_todate + weeks_up_total + size:governance_intensity + size:specialization + size:rule_diversity + size:consolidation + size:resource_complexity + governance_intensity + specialization + rule_diversity + consolidation + resource_complexity 
results:
these things are associated with community success, and their connection to success increases as communities get larger


#http://dwoll.de/rexrepos/posts/multRegression.html
(fit <- lm(cbind(Y1, Y2) ~ X1 + X2 + X3, data=dfRegr))
summary(manova(fit), test="Hotelling-Lawley")
summary(manova(fit), test="Wilks")
summary(manova(fit), test="Roy")
summary(manova(fit), test="Pillai")
