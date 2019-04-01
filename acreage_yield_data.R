library(plyr)
library(reshape2)
library(abind)

# Read each category separately for ease of reading/specifying query
area.harvested <- readquickstats.ERS(source_desc='SURVEY',
                                     agg_level_desc='COUNTY',
                                     domain_desc='TOTAL',
                                     commodity_desc='CORN',
                                     statisticcat_desc='AREA HARVESTED')

area.harvested <-area.harvested[area.harvested$UTIL_PRACTICE_DESC!="FORAGE",]

area.planted <- readquickstats.ERS(source_desc='SURVEY',
                                   agg_level_desc='COUNTY',
                                   domain_desc='TOTAL',
                                   commodity_desc='CORN',
                                   statisticcat_desc='AREA PLANTED')

crop.yield <- readquickstats.ERS(source_desc='SURVEY',
                                 agg_level_desc='COUNTY',
                                 domain_desc='TOTAL',
                                 commodity_desc='CORN',
                                 statisticcat_desc='YIELD')

# aggregate and clean data
qs.data <- rbind(area.harvested,area.planted,crop.yield)
qs.data <- qs.data[!is.na(qs.data$COUNTY_ANSI),]
qs.data$VALUE <- gsub(",","",qs.data$VALUE)
qs.data$VALUE <- as.numeric(qs.data$VALUE)
qs.data <- qs.data[!is.na(qs.data$VALUE),]
qs.data$FIPS <- as.integer(paste(qs.data$STATE_FIPS_CODE,qs.data$COUNTY_CODE, sep=""))
qs.data$YEAR <- as.integer(qs.data$YEAR)

# Andrew's column headers
labels.new <- c("area_harvested","area_harvested_irrigated",	"area_harvested_nonirrigated",	
                "area_harvested_silage",	"area_harvested_silage_irrigated","area_harvested_silage_nonirrigated",	
                "area_planted",	"area_planted_irrigated",	"area_planted_nonirrigated",
                "yield",	"yield_irrigated",	"yield_irrigated_net",	
                "yield_net",	"yield_nonirrigated",	"yield_nonirrigated_net",	
                "yield_silage",	"yield_silage_irrigated",	"yield_silage_nonirrigated"
)

# corresponding QuickStats SHORT_DESCriptions
labels.QS <-  c("CORN, GRAIN - ACRES HARVESTED",
                "CORN, GRAIN, IRRIGATED - ACRES HARVESTED",
                "CORN, GRAIN, NON-IRRIGATED - ACRES HARVESTED" ,
                "CORN, SILAGE - ACRES HARVESTED","CORN, SILAGE, IRRIGATED - ACRES HARVESTED", 
                "CORN, SILAGE, NON-IRRIGATED - ACRES HARVESTED",
                "CORN - ACRES PLANTED" , 
                "CORN, IRRIGATED - ACRES PLANTED", 
                "CORN, NON-IRRIGATED - ACRES PLANTED",
                "CORN, GRAIN - YIELD, MEASURED IN BU / ACRE", 
                "CORN, GRAIN, IRRIGATED - YIELD, MEASURED IN BU / ACRE",                
                "CORN, GRAIN, IRRIGATED - YIELD, MEASURED IN BU / NET PLANTED ACRE",    
                "CORN, GRAIN - YIELD, MEASURED IN BU / NET PLANTED ACRE",               
                "CORN, GRAIN, NON-IRRIGATED - YIELD, MEASURED IN BU / ACRE",            
                "CORN, GRAIN, NON-IRRIGATED - YIELD, MEASURED IN BU / NET PLANTED ACRE",
                "CORN, SILAGE - YIELD, MEASURED IN TONS / ACRE",
                "CORN, SILAGE, IRRIGATED - YIELD, MEASURED IN TONS / ACRE",             
                "CORN, SILAGE, NON-IRRIGATED - YIELD, MEASURED IN TONS / ACRE"         
                 )

qs.data$LABELS <- plyr::mapvalues(qs.data$SHORT_DESC,labels.QS,labels.new)

acres.yields.wide <- reshape2::dcast(qs.data, FIPS + YEAR ~ LABELS, value.var="VALUE",sum)
write.csv("put your local file name here")

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
