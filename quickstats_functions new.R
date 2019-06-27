
read_acreage_yield <- function(crop, state, output.file=NULL, dsource="API"){
  require(plyr)
  require(reshape2)
  require(abind)
  
  # Read each category separately for ease of reading/specifying query
  area.harvested <- readquickstats(source_desc='SURVEY',
                                   agg_level_desc='COUNTY',
                                   domain_desc='TOTAL',
                                   commodity_desc=crop,
                                   state_alpha=state,
                                   statisticcat_desc='AREA HARVESTED',source=dsource)
  
  area.planted <- readquickstats(source_desc='SURVEY',
                                 agg_level_desc='COUNTY',
                                 domain_desc='TOTAL',
                                 commodity_desc=crop,
                                 state_alpha=state,
                                 statisticcat_desc='AREA PLANTED',source=dsource)
  
  crop.yield <- readquickstats(source_desc='SURVEY',
                               agg_level_desc='COUNTY',
                               domain_desc='TOTAL',
                               commodity_desc=crop,
                               state_alpha=state,
                               statisticcat_desc='YIELD',source=dsource)
  
  qs.data <- rbind.fill(area.harvested,area.planted,crop.yield)
  
  return.null <- F
  if (dsource == "API" & is.null(qs.data)) {return.null <- T
  } else if (dsource == "ERS" & nrow(qs.data) == 0) {
    return.null <- T
  }
  
  if(!return.null) {
    print(paste(crop,state,nrow(qs.data),"rows"))
    names(qs.data) <- toupper(names(qs.data))
    qs.data <- qs.data[!(qs.data$UTIL_PRACTICE_DESC=="FORAGE"),]
    
    # aggregate and clean data
    qs.data <- qs.data[!is.na(qs.data$COUNTY_ANSI),]
    qs.data$VALUE <- gsub(",","",qs.data$VALUE)
    qs.data$VALUE <- as.numeric(qs.data$VALUE)
    #  qs.data <- qs.data[!is.na(qs.data$VALUE),]
    qs.data$FIPS <- as.integer(paste(qs.data$STATE_FIPS_CODE,qs.data$COUNTY_CODE, sep=""))
    qs.data$YEAR <- as.integer(qs.data$YEAR)
    
    # go from wide to less wide
    
#    acres.yields.wide <- reshape2::dcast(qs.data[,c("FIPS","YEAR","SHORT_DESC","VALUE")], FIPS + YEAR ~ SHORT_DESC, value.var="VALUE",sum)
    acres.yields.wide <- reshape2::dcast(qs.data[,c("FIPS","YEAR","COMMODITY_DESC","STATISTICAT_DESC","CLASS_DESC","UTIL_PRACTICE_DESC","PRODN_PRACTICE_DESC","VALUE")], FIPS + YEAR + COMMODITY_DESC+STATISTICAT_DESC+CLASS_DESC+UTIL_PRACTICE_DESC+PRODN_PRACTICE_DESC , value.var="VALUE",sum)
    acres.yields.wide[acres.yields.wide==0] <- NA
    return(acres.yields.wide)
  } else {
    return(NULL)
  }
  #  if (!is.null(output.file)) write.csv(acres.yields.wide,output.file, row.names = F,na="")
}

readquickstats <- function(..., source="API") {
  
  require(odbc)
  require(DBI)
  require(httr)
  require(jsonlite)
  
  arg_string <- list(...)
  if (source == "ERS"){ 
    return(qsERS(...))
   } else if (source == "API"){
    return(qsAPI(...))
   } else {
    print("Must specify source: ERS or API")
  }
}
  
qsERS <- function(...){  
  # keep reporting clean. Major errors still get through
  options(error = NULL)
  
  arg_string <- list(...)
  invalid.cols <- integer()
  db <- "NASS_QuickStats_Survey_Crops"
  
  # check that arguments are valid QuickStats column names
  # column definitions are case-insensitive
  names(arg_string) <- tolower(names(arg_string))
  valid.cols <- lapply(names(arg_string), function(x) x %in% c(quickstats.fields, "format"))
  invalid.cols <- which(valid.cols != T)
  
  if (length(invalid.cols) == 0) {
    # specific to ERS SQL server
    con1 <- odbc::dbConnect(odbc::odbc(), Driver ="SQL Server", Server="SQLprod01",
                            Database="NASSQUICKSTATS", Trusted_Connection="Yes")
    # allow query operators to be specified in arguments. Add 'IN'
    operator = "="
    b <- lapply(names(arg_string), function(x){paste(x, operator, "'", arg_string[[x]][1], "'", sep="")})
    bq <- paste(b, collapse=" AND ")
    query_string <- paste("SELECT * FROM NASSQUICKSTATS.QSData.", db, " WHERE ", bq, sep="")
    q <- DBI::dbGetQuery(con1, query_string)
  } else { 
    # Invalid column names detected
    badcols <- names(arg_string)[invalid.cols]
    b <- paste(badcols, collapse = ", ")
    stop(paste(b, ": Invalid QuickStats column name(s). Use printQSfields() for the list of valid column names."))
  } 
  
}

qsAPI <- function(...) {
  # keep reporting clean. Major errors still get through
  options(error = NULL)
  
  arg_string <- list(...)
  invalid.cols <- integer()
  arg_string$format <- "JSON"
  
  # check that arguments are valid QuickStats column names
  # column definitions are case-insensitive
  names(arg_string) <- tolower(names(arg_string))
  valid.cols <- lapply(names(arg_string), function(x) x %in% c(quickstats.fields, "format"))
  invalid.cols <- which(valid.cols != T)
  
  # use your own key, leecher!!!
  arg_string$key="5AF61548-C01D-3836-8521-759B545EB2FC"
  
  if (length(invalid.cols) == 0) {
    # Query columns look good - go for it!
      qs.url <- httr::parse_url("https://quickstats.nass.usda.gov/api/api_GET/")
      qs.url$scheme <- "https"
      qs.url$query <- arg_string
      url <- httr::build_url(qs.url)
      a <- httr::GET(url)
      if (a$status_code == 200) q <- jsonlite::fromJSON(httr::content(a,as="text", encoding="UTF-8"))[[1]]
      else q <- NULL
      return(q)
  } else {
    # Invalid column names detected
    badcols <- names(arg_string)[invalid.cols]
    b <- paste(badcols, collapse = ", ")
    stop(paste(b, ": Invalid QuickStats column name(s). Use printQSfields() for valid column names."))
  } 
}

printQSfields <- function() {
  print(quickstats.fields)
}

quickstats.fields <- c(
  "source_desc",
  "sector_desc",
  "group_desc",
  "commodity_desc",
  "class_desc",
  "prodn_practice_desc",
  "util_practice_desc",
  "statisticcat_desc",
  "unit_desc",
  "short_desc",
  "domain_desc",
  "domaincat_desc",
  "agg_level_desc",
  "state_ansi",
  "state_fips_code",
  "state_alpha",
  "state_name",
  "asd_code",
  "asd_desc",
  "county_ansi",
  "county_code",
  "county_name",
  "region_desc",
  "zip_5",
  "watershed_code",
  "watershed_desc",
  "congr_district_code",
  "country_code",
  "country_name",
  "location_desc", 
  "year" ,
  "freq_desc",
  "begin_code",
  "end_code",
  "reference_period_desc",
  "week_ending",
  "load_time"
)


