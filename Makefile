### from http://zmjones.com/make/
### and  http://blog.kaggle.com/2012/10/15/make-for-data-scientists/

RCMD := R --interactive

#all: data model paper
#model: model.Rout
#paper: plot.Rout paper.pdf
all: prep
prep: prepscrapes prepposts prepmerge 
dev: prepmerge analysis


prepscrapes:
	#echo STEP: Get raw data into nice csv via python
	time python step0_preprocess_server_scrape_data.py
	#echo STEP: Start fixing up r data
	time R -f step1_prep_scrape_data.r
	#echo STEP: Start fixing up r data
	time R -f step2_reduce_scrape_data.r

prepposts:
	time python step25_mcs_org_scrape.py
	time python step3_integrate_data.py

prepmerge:
	time python step4_prep_topic_analysis.py
	time R -f step5_merge_scrapes.r

analysis:
	time R -f step6_plotting.r

test:
	time R -f tests/testthat.r

clean:
	#echo STEP: Reset postgres db mc
	psql -f step05_prep_postgres.sql

model.Rout: model.R
	R CMD BATCH model.R

plot.Rout: plot.R model.Rout
	R CMD BATCH plot.R

paper.pdf: paper.tex
	$(RCMD) $<
	$(RCMD) $<
	bibtex *.aux

