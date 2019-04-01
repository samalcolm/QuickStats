# if you *insist* on using the molasses-time API:
# API function needs to be broken up in chunks that query < 50,000 records. By state should do it.
state.alpha <- c("AL", "GA", "AZ", "IL", "AR", "IN", "IA", "CA", "CO", "DE", "FL", "KS", 
                 "KY", "MD", "MI", "MN", "MS", "MO", "LA", "NE", "ID", "NC", "NJ", "NM", 
                 "MT", "NY", "ND", "OH", "PA", "SC", "OK", "SD", "TN", "TX", "OR", "UT",
                 "VA", "WA", "WV", "WI", "WY")

area.harvested.api <- lapply(state.alpha, function(x) readquickstats.API(source_desc='SURVEY',
                                                                         agg_level_desc='COUNTY',
                                                                         state_alpha=x,
                                                                         domain_desc='TOTAL',
                                                                         commodity_desc='CORN',
                                                                         statisticcat_desc='AREA HARVESTED'))

area.harvested.api2 <- as.data.frame(abind::abind(area.harvested.api, along=1))
area.harvested.api2 <- area.harvested.api2[area.harvested.api2$UTIL_PRACTICE_DESC!="FORAGE",]




# some tests

# test API version using foreach
# 232.32 seconds
tictoc::tic()
area.harvested.api.lapply <- foreach::foreach(i=1:length(state.alpha), .combine=rbind) %do%
  readquickstats.API(source_desc='SURVEY',
                     agg_level_desc='COUNTY',
                     state_alpha=state.alpha[i],
                     domain_desc='TOTAL',
                     commodity_desc='CORN',
                     statisticcat_desc='AREA HARVESTED')
tictoc::toc()

# test API version using lapply
# 230.48
tictoc::tic()
area.harvested.api.foreach <- lapply(state.alpha, 
                                     function(x) readquickstats.API(source_desc='SURVEY',
                                                                    agg_level_desc='COUNTY',
                                                                    state_alpha=x,
                                                                    domain_desc='TOTAL',
                                                                    commodity_desc='CORN',
                                                                    statisticcat_desc='AREA HARVESTED'))
tictoc::toc()

# test ERS SQL server version
# 18.1 seconds
tictoc::tic()
area.harvested.ers <- readquickstats.ERS(source_desc='SURVEY',
                                         agg_level_desc='COUNTY',
                                         domain_desc='TOTAL',
                                         commodity_desc='CORN',
                                         statisticcat_desc='AREA HARVESTED')
tictoc::toc()
