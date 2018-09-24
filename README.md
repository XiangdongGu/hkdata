# R interface to retrieve data from Hong Kong Open Data

### Introduction
This package provides easy to use R functions to work with [Hong Kong Open Data](https://github.com/XiangdongGu).

### Installation
```
devtools::install_github("xiangdonggu/hkdata")
```
### Quickstart
```
library(hkdata)
library(dplyr)
```

#### List and search available data set
Datasets with school
```
list_hist_file(search = "university", start = Sys.Date() - 2, end = Sys.Date() - 2)
```

Datasets provided by Financial service and Treasury Bureau
```
list_hist_file(provider = "hk-fstb", start = Sys.Date() - 2, end = Sys.Date() - 2)
```

Historical version for traffice speed map
```
hist_file_versions("http://resource.data.one.gov.hk/td/speedmap.xml", Sys.Date() - 3, Sys.Date() - 2)
```
then obtain the url for this historical version file
```
hist_file_url("http://resource.data.one.gov.hk/td/speedmap.xml", "20180922-0100")
```

#### Some wrapped up functions
Get current weather
```
weather_current()
```

9-day weather forecast
```
weather_forecast_9day()
```


