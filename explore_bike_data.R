#Explore bike data to see if there is a relationship between weather and ridership ----
library(tidyverse)
df <- read.csv("data/daily_bike_data.csv")
df
summary(df)
ggplot(data = df) + 
  geom_line(aes(x = as.Date(dteday), y = cnt))

#More data exploration ----
#time trend of ridership
plot <-ggplot(data = df) + 
  geom_line(aes(x = as.Date(dteday), y = cnt))
plot

#Relationship between ridership and temperature
ggplot(data=df, aes (x = temp, y = cnt)) +
  geom_point()+ #adds the points, we see a trend but need a line....
  geom_smooth() #adds a nice regression line

#What is weathersit?
summary(df$weathersit)
unique (df$weathersit)
#Some dplyr verbs
#Mutate adds new variables to data frame
#Select....selects stuff
#Filter calls data up based on your desires
#Using dplyr to select/summarize/mutate/change data
df2 <- df %>%
  dplyr::mutate(
    weather_factor = factor(weathersit,
                            levels = c(1,2,3,4),
                            labels = c("Clear","Cloudy","Rainy","Heavy Rain")) #we mutated the numbered weather factors to nice labeled categories
  )
df2 %>% dplyr::select(dteday, weathersit, weather_factor) #select three variables of interest

#double equals tests whether each value in the column is equal to clear rather than assigning it to something, also piping to ggplot directly
df2 %>% dplyr::filter(weather_factor == "Rainy") %>% 
  ggplot(aes(x = temp, y=cnt)) + geom_point() + geom_smooth() #note change in plot
#you can drop variables using select
df3 <- df2 %>%
  dplyr::select(-weathersit)

#Using character lists
keep_vars <- c("dteday","weather_factor","temp","cnt")
df4 <- df2 %>% select( all_of(keep_vars))

#other ways of filtering
weather_factors_we_like <- c("Rainy","Cloudy")
df2 %>% dplyr::filter(weather_factor == "Rainy") #just use the filter function, silly

## dplyr::summarize - see things like mean counts for each weather factor, etc.
df2 %>% group_by(weather_factor) %>%#select variables to define the group 
  dplyr::summarize(
    cnt_mean = mean(cnt)#gives mean ridership for each weather factor we created
  )#we see differences in mean for each weather category

df2 %>% group_by(season,weather_factor) %>%
  dplyr::summarize(
    cnt_mean = mean(cnt)#shows means for each combo of the two variables specified 
  )

#tidyr transformations of data format -"wide data" with each variable as a separate column to "long data" or another useful format
#transforming from long to wide or vice-versa
#"wide" is many columns

#Transform to create separate temp variables for each month/year (wide-ify!)
months <- c("January","February","March","April","May","June","July","August","September","October","November","December")
df_wide
  dplyr::mutate(mnth_fac = factor(mnth, levels = months, labels = months)) %>%
  dplyr::select(yr,mnth,temp) %>%
  dplyr::group_by(yr,mnth) %>%
  dplyr::summarize(temp_mean=mean(temp)) %>%
  tidyr::pivot_wider(names_prefix = "temp", names_from = mnth, values_from = temp_mean)

#another pivot example
df %>%
  group_by(weekday) %>%
  summarize(mean_temp = mean(temp)) %>%
  pivot_wider(names_from = weekday,
              values_from = mean_temp)

#faceting-making many smaller plots to add dimensions to data visualizati8on
#Create separate plot for each factor in weatherfac variable column
ggplot(data = df2, aes(x=temp,y=cnt)) +
  geom_point() +
  geom_smooth(method = "lm", color = "steelblue") + #selecting lm makes a straight line with error shaded instead of the smooth line as previous. Can also do quadratic, etc.
  facet_wrap(~ weather_factor,scales = "free_y") + #free_y frees up the scaling
  theme_linedraw() + #THERE ARE THEMES!
  labs(x="Temperature",y="Ridership (County)") +
  ggtitle("Relationships Between Temperature and Ridership") #these commands do exactly what you think they do.

#now...long style
ggplot

#notes-this doesn't work because I flubbed the "long" format code.
ggplot(data=df_long,aes(x=value,y=cnt,color=variable)) +
  geom_point() +
  facet_wrap(~ weather_factor)

ggsave(plot=plot, filename ="temp_county_scatter.png")


