library(readr)

###### Fingal Data ###############
Fingal <- read_csv("IE0131A_Fingal.csv")
View(Fingal)

#Remove the first row, contains the first entry for Feb
Fingal <- Fingal[-1,]

newDate <- as.Date(Fingal$local)
values <- Fingal$value
Fingal_lat <- Fingal$latitude
Fingal_long <- Fingal$longitude

df <- data.frame(newDate,values)

summary(Fingal)
str(Fingal)

data_months <- months(df$newDate)
data_day <- format(df$newDate,format="%d")                      
data_values <- df$values
 

#create days and months variables for the respective data values
df1 <- data.frame(data_months,data_day,data_values,Fingal_lat,Fingal_long)
#Get the mean for each day so that the data can be compared to a different location
agg <- aggregate(values ~ data_day + Fingal_lat + Fingal_long, df1, mean)


######## South Dublin Data #################

SouthDublin <- read_csv("IE0095_SouthDublin.csv")
View(SouthDublin)
SouthDublin <- SouthDublin[-1,]

sd_lat <- SouthDublin$latitude
sd_long <- SouthDublin$longitude
sd_date <- as.Date(SouthDublin$local)
sd_values <- SouthDublin$value
df_sd <- data.frame(sd_date,sd_values)

sd_months <- months(df_sd$sd_date)
sd_day <- format(df_sd$sd_date,format="%d")
sd_values <- df_sd$sd_values

sd_agg_df <- data.frame(sd_months,sd_day,sd_values,sd_lat,sd_long)
sd_agg <- aggregate(sd_values ~ sd_day + sd_lat +sd_long, sd_agg_df, mean)

########### Dublin City Data ########################

DublinCity <- read_csv("dublin_city_data.csv")
View(DublinCity)
DublinCity <- DublinCity[-1,]

dc_lat <- DublinCity$latitude
dc_long <- DublinCity$longitude
dc_date <- as.Date(DublinCity$local)
dc_values <- DublinCity$value
df_dc <- data.frame(dc_date,dc_values)

dc_months <- months(df_dc$dc_date)
dc_day <- format(df_dc$dc_date, format="%d")
dc_values <- df_dc$dc_values

dc_agg_df <- data.frame(dc_months, dc_day, dc_values, dc_lat, dc_long)
dc_agg <- aggregate(dc_values ~ dc_day +dc_lat +dc_long, dc_agg_df, mean)

dates_dublin <- c("26th","27th","28th","29th","30th","31st")
comparison_fingal <- tail(agg$values,6)
comparison_sd <- sd_agg$sd_values
comparison_dc <- tail(dc_agg$dc_values,6)
dublin_comparison <- data.frame(comparison_fingal,comparison_sd,comparison_dc)
plot(dublin_comparison)

##### Linear regression South Dublin and Fingal#########
regression_sd_fingal <- lm(comparison_fingal ~ comparison_sd, data = dublin_comparison)
summary(regression_sd_fingal)
##### Linear regression South Dublin and Dublin City#########
regression_sd_dc <- lm(comparison_dc ~ comparison_sd, data = dublin_comparison)
summary(regression_sd_dc)
##### Multiple regression Fingal as dependent variable and south Dublin and Dublin city as independent variables#########
m_regression <- lm(comparison_fingal ~ comparison_dc + comparison_sd, data = dublin_comparison)
summary(m_regression)

########### India Data #############################

Koppal <- read_csv("Koppal_India.csv")
View(Koppal)

koppal_lat <- Koppal$latitude
koppal_long <- Koppal$longitude
koppal_date <- as.Date(Koppal$local)
koppal_values <- Koppal$value
df_koppal <- data.frame(koppal_date,koppal_values)

koppal_months <- months(df_koppal$koppal_date)
koppal_day <- format(df_koppal$koppal_date, format="%d")
koppal_values <- df_koppal$koppal_values

koppal_agg_df <- data.frame(koppal_months,koppal_day,koppal_values,koppal_lat, koppal_long)
koppal_agg <- aggregate(koppal_values ~ koppal_day + koppal_lat + koppal_long, koppal_agg_df, mean)

########### Davanagere India #######################
Davanagere <- read_csv("Davanagere_India.csv")
View(Davanagere)

davanagere_lat <- Davanagere$latitude
davanagere_long <- Davanagere$longitude
davanagere_date <- as.Date(Davanagere$local)
davanagere_values <- Davanagere$value
df_davanagere <- data.frame(davanagere_date,davanagere_values)

davanagere_months <- months(df_davanagere$davanagere_date)
davanagere_day <- format(df_davanagere$davanagere_date, format="%d")
davanagere_values <- df_davanagere$davanagere_values

davanagere_agg_df <- data.frame(davanagere_months,davanagere_day,davanagere_values, davanagere_lat, davanagere_long)
davanagere_agg <- aggregate(davanagere_values ~ davanagere_day + davanagere_lat + davanagere_long, davanagere_agg_df, mean)

########### Shivamogga India #######################
Shivamogga <- read_csv("Shivamogga_India.csv")
View(Shivamogga)

shivamogga_lat <- Shivamogga$latitude
shivamogga_long <- Shivamogga$longitude
shivamogga_date <- as.Date(Shivamogga$local)
shivamogga_values <- Shivamogga$value
df_shivamogga <- data.frame(shivamogga_date,shivamogga_values)

shivamogga_months <- months(df_shivamogga$shivamogga_date)
shivamogga_day <- format(df_shivamogga$shivamogga_date, format="%d")
shivamogga_values <- df_shivamogga$shivamogga_values

shivamogga_agg_df <- data.frame(shivamogga_months,shivamogga_day,shivamogga_values, shivamogga_lat, shivamogga_long)
shivamogga_agg <- aggregate(shivamogga_values ~ shivamogga_day + shivamogga_lat + shivamogga_long, shivamogga_agg_df, mean)

dates_India <- c("12th","13th","14th","15th","16th","17th","18th","19th","20th","21st")
comparison_koppal <- koppal_agg$koppal_values[12:21]
comparison_davanagere <- davanagere_agg$davanagere_values[1:10]
comparison_shivamogga <- shivamogga_agg$shivamogga_values[12:21]
india_comparison <- data.frame(comparison_koppal,comparison_davanagere,comparison_shivamogga)
plot(india_comparison)


##### Linear regression Shivamogga and Davanagere #########
regression_Shivamogga_Davanagere <- lm(comparison_shivamogga ~ comparison_davanagere, data = india_comparison)
summary(regression_Shivamogga_Davanagere)
##### Linear regression Davanagere and Koppal#########
regression_davanagere_koppal <- lm(comparison_davanagere ~ comparison_koppal, data = india_comparison)
summary(regression_davanagere_koppal)
##### Multiple regression Shivamogga as dependent variable and davanagere and koppal as independent variables#########
m_regression_india <- lm(comparison_shivamogga ~ comparison_davanagere + comparison_koppal, data = india_comparison)
summary(m_regression_india)
