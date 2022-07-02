library(quantmod)
library(lubridate)
getSymbols("AAPL", from = "2020-01-01", to = today("UTC"), auto.assign = F)
