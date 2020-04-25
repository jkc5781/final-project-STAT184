---
title: "Final Project Report"
author: "Jared Cao, Macklin Yarris, Abby Crilley"
date: "April 24th, 2020"
output: 
    html_notebook:
    fig_height: 6
    fig_width: 10
---
### Clean up environment and load packages
```{r}
# clean up the RStudio environment 
rm(list = ls())

# load all packages here: `mosaic`, `tidyverse`, `lubridate`, and all others used
library(tidyverse)
library(mosaic)
library(lubridate)
library(DataComputing)
```

### Guiding Question:

## Are suicide rates and world happiness inversely correlated?

<br>

Our goal of this report is to look into whether or not countries that are generally happier have less citizens committing suicide. This topic is very interesting to research. Suicide is often a clear indicator of lack of happiness in a person's life, so we thought it would be very interesting to find a quantitative meausure of happiness among different regions of the world and compare it to the number of suicides in those regions. We have found a few data sets that will help us come up with an answer to this question. By combining data gathered on suicides across the world and reports on world happiness over the years, we have been able to make some insights.

<br>

## Data Access

<br>

### Download 2015 and 2016 World Happiness Value files and Suicide Rates file to analyze the correlation
```{r}
SuicideRates <-
  read.csv(file.choose())
```

```{r}
WorldHappiness2015 <-
  read.csv(file.choose())
```

```{r}
WorldHappiness2016 <-
  read.csv(file.choose())
```

```{r}
head(SuicideRates)
head(WorldHappiness2015)
head(WorldHappiness2016)
```

## Data Wrangling

<br>

### Join 2015 and 2016 World Happiness tables to make it easier to work with and analyze

```{r}
WorldHappiness2015 <-
  WorldHappiness2015 %>%
  mutate(year = "Happiness2015")

WorldHappiness2016 <-
  WorldHappiness2016 %>%
  mutate(year = "Happiness2016")

WorldHappiness <-
  WorldHappiness2015 %>%
  full_join(WorldHappiness2016)
```

### Only get years 2015 and 2016 from the suicide master table since we only have 2015 and 2016 World Happiness Values
```{r}
RecentSuicideRates <-
  SuicideRates %>%
  filter(year == 2015 | year == 2016)
```

### Select and group by key values in suicide and happiness table to gather all the important information
```{r}
RecentSuicideRates <-
  RecentSuicideRates %>%
  select(country, year, sex, population, suicides_no, suicides.100k.pop)

RecentSuicideRates <-
  RecentSuicideRates %>%
  group_by(country, year, sex) %>%
  summarise(totalsuicides = sum(suicides_no))
RecentSuicideRates

WorldHappiness <-
  WorldHappiness %>%
  select(country = Country, Happiness.Score, Generosity, year)
```

### Spread suicide table
```{r}
SuicideByYear <-
  RecentSuicideRates %>%
  spread(key = year, value = totalsuicides)
```

### Omit any NA values and rename 2015 and 2016 columns so we have values in all of the rows
```{r}
Suicide2015<-
  SuicideByYear[1:3]
names(Suicide2015)[3] <- "Year2015"

SuicideByYear <-
  SuicideByYear %>%
  na.omit()

names(SuicideByYear)[3] <- "Year2015"
names(SuicideByYear)[4] <- "Year2016"
```

### Spread World Happiness table and omit any NA values
```{r}
WorldHappinessByYear <-
  WorldHappiness %>%
  select(country, Happiness.Score, year) %>%
  spread(key = year, value = Happiness.Score) %>%
  na.omit

H2015<-WorldHappinessByYear[1:2]
```

### Values based on Generosity
```{r}
WorldHappinessByYear <-
  WorldHappiness %>%
  select(country, Generosity, year) %>%
  spread(key = year, value = Generosity) %>%
  na.omit

G2015<-WorldHappinessByYear[1:2]
names(G2015)[2] <- "Generosity2015"

SG2015<-
Suicide2015%>%
  inner_join(G2015)
SG2015
```


### Join the Happiness and Suicide tables to analyze the correlation
```{r}
SuicideHappiness <-
  SuicideByYear %>%
  inner_join(WorldHappinessByYear)

SH2015<-
Suicide2015%>%
  inner_join(H2015)
SH2015
```


## Data Visualization
```{r}
SuicideHappiness%>%
  ggplot(aes(x = Happiness2015, y = Year2015, color = country)) +
  geom_point() +
  geom_point(aes(x = Happiness2016, y = Year2016, color = country, fill = country), alpha = 0.5) +
  facet_wrap(~sex) +
  xlab("Happiness Scores in 2015-16") +
  ylab("Suicides in 2015-16") +
  theme(legend.position = "bottom") +
  ylim(0, 2000)
```
There is slight inverse correlation between suicide rate and happiness value, but not enough to say they effect each others values.

<br>

## Difficulties

The main technical difficulty we faced during our period of research was the lack of suicide data proivided for the 2016 year. There was much more suicide data from 2015 compared to 2016, as 2016 was the final year included in the original dataset. Although we were able to omit data that was not available from our analysis, by just focusing on data from 2015 we were able to draw more intuitive conclusions on how a country's suicide rates and happiness values compared to one another. Our focus of this project was not to look at the difference in suicides by year, so we thought it was appropriate to focus on just 2015 for some of our graph anaylsis.

<br>

# Graph of just 2015 suicide and happiness values
```{r}
SH2015%>%
  ggplot(aes(x = Happiness2015, y = Year2015)) +
  geom_point() +
  geom_smooth() +
  ylim(0, 10000) +
  ylab("Country Suicides in 2015") +
  xlab("Country Happiness Scores in 2015")
```

<br>

We separated the 2015 and 2016 values because there is more data from 2015 than 2016. There are about 100 glyphs in 2015 rather than the joined 2015 and 2016 there are only 28. From this graph we have more data points and can easily tell the rate of suicides start to drop when the happiness value reaches 7. We can conclude that when a country's happiness value is in the highest 5% range the suicide rates decrease. Since this is such a small percent, we can not say that there is enough evidence that shows when a country's happiness value is low they will have a high suicide rate or when the happiness value is high they have a low suicide rate.

<br>

### Graph of 2015 Generosity Score and Suicides.

```{r}
SG2015%>%
  ggplot(aes(x = Generosity2015, y = Year2015)) +
  geom_point() +
  geom_smooth() +
  ylim(0, 3000) +
  xlim(0, 0.4) +
  ylab("Country Suicides in 2015") +
  xlab("Country Generosity Scores in 2015")
```
```{r}
G2015 %>%
  summarise(meanG = mean(Generosity2015))
```

Taking a look at the generosity variable, we can see that countries with lower generosity scores, ranging from 0 and 0.2 will have a higher rate of suicide then countries with generosity scores above 0.2 as you can see in the graph. Countries that have a higher generosity score than the mean typically have lower suicide rates. 

<br>


## Conclusions

We were not able to confirm our guiding research question, as based on this data, there is no indication that world happiness scores and suicides are inversely correlated. One meaningful conclusion we made is that when a country's generosity score is above the world's average generosity score, there is a decline in total suicides in that country. This is clearly shown in the final graph of our report. This conclusion shows us that although happiness scores and total suicides don't have a clear negative correlation, various factors that contribute to happiness seem to have an inversed relationship with suicide numbers. However, our answer to our guiding research question is no, suicide rates and world happiness are not inversely correlated.

<br>
