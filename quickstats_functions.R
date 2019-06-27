


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
  arg_string$key=apikey
  
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


