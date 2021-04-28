# Read the data set for the cases in all countries
library(readr)
time_series_covid19_confirmed_global <- read_csv("time_series_covid19_confirmed_global.csv")
time_series_covid19_confirmed_global$t<-as.Date(time_series_covid19_confirmed_global$t,format = "%m/%d/%Y")

# Read the index from Brazil
brazil_january_october_2020 <- read_csv("brazil_--january-october-2020.csv")
names(brazil_january_october_2020)=c("t","Brazil_Index")
brazil_january_october_2020$t=as.Date(brazil_january_october_2020$t,format = "%m/%d/%Y")

# Read the index from Argentina
argentina_merval_stock_market_index_2020 <- read_csv("argentina_-merval-stock-market-index-2020.csv")
names(argentina_merval_stock_market_index_2020)=c("t","Argentina_Index")
argentina_merval_stock_market_index_2020$t=as.Date(argentina_merval_stock_market_index_2020$t,format = "%m/%d/%Y")

# Read the index from Mexico
mexico_ipc_stock_market_index_january_october_2020 <- read_csv("mexico_-ipc-stock-market-index-january-october-2020.csv")
names(mexico_ipc_stock_market_index_january_october_2020)=c("t","Mexico_Index")
mexico_ipc_stock_market_index_january_october_2020$t=as.Date(mexico_ipc_stock_market_index_january_october_2020$t,format = "%m/%d/%Y")

# Read the weekly index from US
weekly_dow_jones_industrial_average_index_performance_2020_2021 <- read_csv("weekly-dow-jones-industrial-average-index-performance-2020-2021.csv")
names(weekly_dow_jones_industrial_average_index_performance_2020_2021)=c("t","US_Index")
weekly_dow_jones_industrial_average_index_performance_2020_2021$t=as.Date(weekly_dow_jones_industrial_average_index_performance_2020_2021$t,format = "%m/%d/%Y")

# Join the indexes with the data 
library(dplyr)
ALL_data=left_join(time_series_covid19_confirmed_global, brazil_january_october_2020,by="t")
ALL_data=left_join(ALL_data, argentina_merval_stock_market_index_2020,by="t")
ALL_data=left_join(ALL_data, mexico_ipc_stock_market_index_january_october_2020,by="t")
ALL_data=left_join(ALL_data, weekly_dow_jones_industrial_average_index_performance_2020_2021,by="t")


###############################################################################################################
# Data analysis for Brazil

library(plotly)
fig <- plot_ly(data=ALL_data,x = ~t, y = ~Brazil_C, mode = 'lines')
fig

fig <- plot_ly(data=ALL_data,x = ~t, y = ~Brazil_Index, mode = 'lines')
fig

fig <- plot_ly(data=ALL_data,x = ~Brazil_Index, type = "histogram")
fig

fig <- plot_ly(data=ALL_data,x = ~Brazil_C, type = "histogram")
fig

fig <- plot_ly(data=ALL_data[ALL_data$Brazil_C>0,],x = ~log(Brazil_C), y = ~Brazil_Index)
fig

cor(log(ALL_data$Brazil_C[ALL_data$Brazil_C>0]),ALL_data$Brazil_Index[ALL_data$Brazil_C>0],use="complete.obs")

mod=lm(data = ALL_data[ALL_data$Brazil_C>0,], formula = Brazil_Index~log(Brazil_C),na.action=na.omit)
summary(mod)

plot(mod)

##################################################################################################################
# Data analysis for Argentina

library(plotly)
fig <- plot_ly(data=ALL_data,x = ~t, y = ~Argentina_C, mode = 'lines')
fig

fig <- plot_ly(data=ALL_data,x = ~t, y = ~Argentina_Index, mode = 'lines')
fig

fig <- plot_ly(data=ALL_data,x = ~Argentina_Index, type = "histogram")
fig

fig <- plot_ly(data=ALL_data,x = ~Argentina_C, type = "histogram")
fig

fig <- plot_ly(data=ALL_data[ALL_data$Argentina_C>0,],x = ~log(Argentina_C), y = ~Argentina_Index)
fig

cor(log(ALL_data$Argentina_C[ALL_data$Argentina_C>0]),ALL_data$Argentina_Index[ALL_data$Argentina_C>0],use="complete.obs")

mod=lm(data = ALL_data[ALL_data$Argentina_C>0,], formula = Argentina_Index~log(Argentina_C),na.action=na.omit)
summary(mod)

plot(mod)

##################################################################################################################
# Data analysis for Mexico

library(plotly)
fig <- plot_ly(data=ALL_data,x = ~t, y = ~Mexico_C, mode = 'lines')
fig

fig <- plot_ly(data=ALL_data,x = ~t, y = ~Mexico_Index, mode = 'lines')
fig

fig <- plot_ly(data=ALL_data,x = ~Mexico_Index, type = "histogram")
fig

fig <- plot_ly(data=ALL_data,x = ~Mexico_C, type = "histogram")
fig

fig <- plot_ly(data=ALL_data[ALL_data$Mexico_C>0,],x = ~log(Mexico_C), y = ~Mexico_Index)
fig

cor(log(ALL_data$Mexico_C[ALL_data$Mexico_C>0]),ALL_data$Mexico_Index[ALL_data$Mexico_C>0],use="complete.obs")

mod=lm(data = ALL_data[ALL_data$Mexico_C>0,], formula = Mexico_Index~log(Mexico_C),na.action=na.omit)
summary(mod)

plot(mod)


##################################################################################################################
# Data analysis for US

library(plotly)
fig <- plot_ly(data=ALL_data,x = ~t, y = ~US_C, mode = 'lines')
fig

fig <- plot_ly(data=ALL_data,x = ~t, y = ~US_Index, mode = 'lines')
fig

fig <- plot_ly(data=ALL_data,x = ~US_Index, type = "histogram")
fig

fig <- plot_ly(data=ALL_data,x = ~US_C, type = "histogram")
fig

fig <- plot_ly(data=ALL_data[ALL_data$US_C>0,],x = ~log(US_C), y = ~US_Index)
fig

cor(log(ALL_data$US_C[ALL_data$US_C>0]),ALL_data$US_Index[ALL_data$US_C>0],use="complete.obs")

mod=lm(data = ALL_data[ALL_data$US_C>0,], formula = US_Index~log(US_C),na.action=na.omit)
summary(mod)

plot(mod)

