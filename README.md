# QuickStats
functions to read NASS data from the ERS SQL server and/or via web API

'source' argument defaults to API. Otherwise, set source='ERS' AFTER list of QuickStats fields.

Example:

```
corn.area.harvested.API <- readquickstats(source_desc='SURVEY',
                                     agg_level_desc='COUNTY',
                                     domain_desc='TOTAL',
                                     commodity_desc='CORN',
                                     state_alpha='IA',
                                     year=2000,
                                     statisticcat_desc='AREA HARVESTED')
                                     
corn.area.harvested.ERS <- readquickstats(source_desc='SURVEY',
                                     agg_level_desc='COUNTY',
                                     domain_desc='TOTAL',
                                     commodity_desc='CORN',
                                     state_alpha='IA',
                                     year=2000,
                                     statisticcat_desc='AREA HARVESTED',
                                     source='ERS')
```
