# QuickStats
functions to read NASS data from the ERS SQL server and/or via web API

Sourcing quickstats_functions.R loads functions needed to run acreage_yield_data function

Example:

corn.area.harvested.API <- readquickstats(source_desc='SURVEY',
                                     agg_level_desc='COUNTY',
                                     domain_desc='TOTAL',
                                     commodity_desc='CORN',
                                     state_alpha='IA',
                                     year=2000,
                                     statisticcat_desc='AREA HARVESTED')
