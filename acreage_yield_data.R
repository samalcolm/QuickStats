
acreage_yield_data <- function(output.file=NULL){

  require(plyr)
  require(reshape2)
  require(abind)

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
  acres.yields.wide[acres.yields.wide==0] <- NA
  if (!is.null(output.file)) write.csv(acres.yields.wide,output.file, row.names = F,na="")
  return(acres.yields.wide)
}

