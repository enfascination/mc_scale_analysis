library(testthat)
pathLocal <- '/Users/sfrey/projecto/research_projects/minecraft/redditcommunity/'
source(paste0(pathLocal,"header_db_redditscrape.r"))

context("server data set processing")

servers <- readRDS(file.path(pathData, "step2_servers_fortesting.rds"))
test_that("no duplicate rows", { 
  #expect_equal(nrow(serversweeks), nrow(unique(serverweekss, by=c("server", "year", "week"))))
  expect_equal(nrow(servers)/2, nrow(unique(servers, by=c("server"))))
})

test_that("sanity of basic construct", { 
  expect_true(all(servers[!is.na(genivisits),genivisits >= 0]))
  expect_equal(servers[!is.na(genivisits),max(genivisits)], 1)
  expect_equal(servers[!is.na(pctmaxpop),min(pctmaxpop)], 0)
  expect_true(servers[,!all(is.na(genivisits))])
  expect_true(servers[,!any(is.na(server))])
  expect_true(servers[,!any(is.na(year))])
  expect_true(servers[,!any(is.na(week))])
})

test_that("the data violates some basic asusmptions and has basic flaws", { 
  expect_true(!all(servers[,testnmaxquota == testnminquota]))
  expect_true(!all(servers[,nmaxpop < testnmaxquota]))
  expect_true(!all(servers[,nvisitsobs < nvisits]))
  expect_true(!all(servers[,pctmaxpop<=1]))
})


context("server/week data set processing")
serversweeks <- data.table(dbGetQuery(con, "SELECT * FROM serversweeks"))
test_that("the data violates some basic asusmptions and has basic flaws", { 
  expect_true(!all(serversweeks[,testnmaxquota == testnminquota]))
  expect_true(!all(serversweeks[,nmaxpop < testnmaxquota]))
  expect_true(!all(serversweeks[,nvisitsobs <= nvisits]))
  expect_true(!all(serversweeks[,pctmaxpop<=1]))
})
  expect_true(all(serversweeks[,nvisitsobs >= nuvisits]))
