# sds315_hw3

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, message=FALSE, echo=FALSE}
library(ggplot2)
library(dplyr)
library(tidyverse)
library(knitr)
library(lubridate)
library(mosaic)
```

*Problem 1 part A*
```{r, message=FALSE, echo=FALSE,fig.width=7, fig.height=2}

#load data set
gasprices <- read.csv("gasprices.csv")

#mutuate so that Y = Yes & N = No
gasprices <- gasprices |>
  mutate(Competitors= replace(Competitors, Competitors=="Y", "Yes"))|>
  mutate(Competitors= replace(Competitors, Competitors=="N", "No"))

#before the bootstrap, create a ggplot to visualize the two groups 
ggplot(data=gasprices) +
  geom_histogram(aes(x=Price),bins=15, color="black",fill="slategray2")+
  facet_wrap(~Competitors)+
  labs(
   title= "Gas Station Prices: Competitors in Sight vs No Competitors in Sight",
    x= "Price ($)",
    y= "Count")+
  theme_minimal()

#compute 95% bootstrap confidence interval 

#bootstrap
gasprices_bootstrap <- do(10000)*diffmean(Price ~ Competitors, data=resample(gasprices))

#our confidence interval reflecting our uncertainty 
confint(gasprices_bootstrap, level=0.95)

#make a histogram of the distribution
ggplot(data=gasprices_bootstrap)+
  geom_histogram(aes(x=diffmean), bins=50, color="black", fill="hotpink")+
  labs(
    title="Distribution of 95% CI for Gas Prices (No Competitors & Competitors)",
    x= "Sample Mean Difference in Price",
    y="Count")+
  theme_minimal()


```
*Problem 1 part B*
```{r, message=FALSE, echo=FALSE,fig.width=7, fig.height=2}


#before the bootstrap, create a ggplot to visualize the two groups 
ggplot(data=gasprices) +
  geom_point(aes(x=Price, y=Income), color="slategray2")+
  labs(
   title= "Gas Station Prices vs Income",
    x= "Price ($)",
    y= "Income ($)")+
  theme_minimal()

#mutuate to create a categorical variable for income 
gasprices <- gasprices|>
  mutate(high_income= Income > 52306 )

#compute 95% bootstrap confidence interval 

#bootstrap
gasprices_bootstrap2 <- do(10000)*diffmean(Price ~ high_income, data=resample(gasprices))

#our confidence interval reflecting our uncertainty 
confint(gasprices_bootstrap2, level=0.95)

#make a histogram of the distribution
ggplot(data=gasprices_bootstrap2)+
  geom_histogram(aes(x=diffmean), bins=50, color="black", fill="hotpink2")+
  labs(
    title="Distribution of 95% CI for Gas Prices (High Income & Low Income)",
    x= "Sample Mean Difference in Price",
    y="Count")+
  theme_minimal()

```
*problem 1 part c*
```{r, message=FALSE, echo=FALSE,fig.width=7, fig.height=2}


#mutuate so that Y = Yes & N = No
gasprices <- gasprices |>
  mutate(Stoplight= replace(Stoplight, Stoplight=="Y", "Yes"))|>
  mutate(Stoplight= replace(Stoplight, Stoplight=="N", "No"))

#before the bootstrap, create a ggplot to visualize the two groups 
ggplot(data=gasprices) +
  geom_histogram(aes(x=Price),bins=15, color="black",fill="slategray2")+
  facet_wrap(~Stoplight)+
  labs(
   title= "Gas Station Prices: Not at Stoplights vs at Stoplights",
    x= "Price ($)",
    y= "Count")+
  theme_minimal()


#compute 95% bootstrap confidence interval 

#bootstrap
gasprices_bootstrap3 <- do(10000)*diffmean(Price ~ Stoplight, data=resample(gasprices))

#our confidence interval reflecting our uncertainty 
confint(gasprices_bootstrap3, level=0.95)

#make a histogram of the distribution
ggplot(data=gasprices_bootstrap3)+
  geom_histogram(aes(x=diffmean), bins=50, color="black", fill="hotpink3")+
  labs(
    title="Boostraped Distribution of 95% CI: Gas Prices (At Stoplights & Not at Stoplights)",
    x= "Sample Mean Difference in Price",
    y="Count")+
  theme_minimal()

```
*problem 1 part d*
```{r, message=FALSE, echo=FALSE,fig.width=7, fig.height=2}

#mutuate so that Y = Yes & N = No
gasprices <- gasprices |>
  mutate(Highway= replace(Highway, Highway=="Y", "Yes")) |>
  mutate(Highway= replace(Highway, Highway=="N", "No")) 

#make ggplot to visualize relationship between price and highways
ggplot(data=gasprices)+
  geom_histogram(aes(x=Price), bins=15, color="black", fill="slategray2")+
  facet_wrap(~Highway)+
  labs(
   title= "Gas Station Prices: Not Accessible From a Highway or Accessible",
   x= "Price ($)",
   y= "Count")+
  theme_minimal()

#compute 95% bootstrap confidence interval 

#bootstrap
gasprices_bootstrap4 <- do(10000)*diffmean(Price ~ Highway, data=resample(gasprices))

#our confidence interval reflecting our uncertainty 
confint(gasprices_bootstrap4, level=0.95)

#make a histogram of the distribution
ggplot(data=gasprices_bootstrap4)+
  geom_histogram(aes(x=diffmean), bins=50, color="black", fill="hotpink4")+
  labs(
    title="Boostraped Distribution of 95% CI: Gas Prices (On Highway vs Not on Highway)",
    x= "Sample Mean Difference in Price",
    y="Count")+
  theme_minimal()


```
*problem 1 part e
```{r, message=FALSE, echo=FALSE,fig.width=7, fig.height=2}



#create new frame so that all non-shell brands are changed to "non-shell"
gasprices2 <- gasprices |>
  group_by(Brand) |>
  mutate(Brand=replace(Brand, Brand!="Shell", 'Non-Shell'))

#create plot for prices between non-shell brands and shell 
ggplot(data=gasprices2)+
  geom_histogram(aes(x=Price), bins=15, color="black", fill="slategray2")+
  facet_wrap(~Brand)+
  labs(
    title="Gas Station Prices: Non-Shell Brands vs Shell",
    x="Price ($)",
    y="Count"
  )+
  theme_minimal()

#compute 95% bootstrap confidence interval 

#bootstrap
gasprices_bootstrap5 <- do(10000)*diffmean(Price ~ Brand, data=resample(gasprices2))

#our confidence interval reflecting our uncertainty 
confint(gasprices_bootstrap5, level=0.95)

#make a histogram of the distribution
ggplot(data=gasprices_bootstrap5)+
  geom_histogram(aes(x=diffmean), bins=50, color="black", fill="pink1")+
  labs(
    title="Boostraped Distribution of 95% CI: Gas Prices (Shell & Not Shell)",
    x= "Sample Mean Difference in Price",
    y="Count")+
  theme_minimal()


  
```
*problem 2 part a*
```{r, message=FALSE, echo=FALSE,fig.width=6, fig.height=3}

#load data set
sclass <- read_csv("sclass.csv")

#filter data set
sclass2 <- sclass |>
  filter(year==2011 & trim=="63 AMG")

#compute 95% bootstrap confidence interval 


#bootstrap
sclass_bootstrap = do(1000)*mean(~mileage, data=resample(sclass2))

# Make a histogram of the distribution 
ggplot(data=sclass_bootstrap) +
  geom_histogram(aes(x=mean), bins=50, color="black", fill="darkslategray4") +
  labs(title="Bootstrap Distribution of Mean Mileage", x="Mean Mileage", y="Count")+
  theme_minimal()


#confidence interval
confint(sclass_bootstrap, level=0.95)

```
*problem 2 part b*
```{r, message=FALSE, echo=FALSE,fig.width=6, fig.height=3}

#create new df with filtered variables and mutated color variable
sclass3 <- sclass |>
  mutate(color=replace(color, color!="Black", "Non-Black"))|>
  filter(year==2014, trim=="550")

#conduct bootstrap
sclass_bootstrap2= do(1000)*prop(~(color=="Black"), data=resample(sclass3))

#make a histogram
ggplot(data=sclass_bootstrap2) +
  geom_histogram(aes(x=prop_TRUE), bins=50, color="black", fill="cadetblue3") + 
  labs(title="Bootstrap Distribution of Proportion of Black Cars", x="Proportion of Black Cars", y="Count")+
  theme_minimal()

#confidence interval
confint(sclass_bootstrap2, level=0.95)
```
*problem 3 part a*
```{r, message=FALSE, echo=FALSE,fig.width=7, fig.height=2}

#load data set
pilot <- read_csv("nbc_pilotsurvey.csv")

#create filtered data set 
pilot2 <- pilot |>
  filter(Show=="Living with Ed" | Show=="My Name is Earl")

#make ggplot to visualize relationship between the two shows 
ggplot(data=pilot2) +
  geom_histogram(aes(x=Q1_Happy), binwidth=1, position="dodge",color='black',fill='slategray2') +
  facet_wrap(~Show) +
  labs( 
     title="Viewer Happiness Response: Living with Ed vs. My Name is Earl",
    x= "Happiness Score for Q1",
    y="Count")+
  theme_minimal()


#compute 95% bootstrap confidence interval 


#bootstrap
pilot_bootstrap= do(1000)*diffmean(Q1_Happy~ Show, data=resample(pilot2))

# Make a histogram of the distribution 
ggplot(data=pilot_bootstrap) +
  geom_histogram(aes(x=diffmean), bins=50, color="black", fill="mediumpurple1") +
  labs(
       title="Boostraped Distribution of 95% CI",
       x="Sample Mean Difference in Score", 
       y="Count")+
  theme_minimal()

#confidence interval
confint(pilot_bootstrap, level=0.95)


```
*problem 3 part b*
```{r, message=FALSE, echo=FALSE,fig.width=7, fig.height=2}


#create filtered data set
pilot3 <- pilot |>
  filter(Show=="The Biggest Loser" | Show=="The Apprentice: Los Angeles")

#make ggplot to visualize relationship between the two shows
ggplot(data=pilot3)+
  geom_histogram(aes(x=Q1_Annoyed), binwidth=1, position="dodge", color='black', fill='slategray2')+
  facet_wrap(~Show)+
  labs( 
     title="Viewer Annoyance Response: The Biggest Loser vs.The Apprentice: Los Angeles",
    x= "Annoyance Score for Q1",
    y="Count")+
  theme_minimal()

#compute 95% bootstrap confidence interval 


#bootstrap
pilot_bootstrap2= do(1000)*diffmean(Q1_Annoyed~ Show, data=resample(pilot3))

# Make a histogram of the distribution 
ggplot(data=pilot_bootstrap2) +
  geom_histogram(aes(x=diffmean), bins=50, color="black", fill="mediumpurple3") +
  labs(
    title="Boostraped Distribution of 95% CI: Score (The Biggest Loser vs.The Apprentice: Los Angeles)", 
    x="Sample Mean Difference in Score", 
    y="Count")+
  theme_minimal()

#confidence interval
confint(pilot_bootstrap2, level=0.95)


```
*problem 3 part c*
```{r, message=FALSE, echo=FALSE,fig.width=6, fig.height=4}

#filter the data set & mutate
pilot4 <- pilot |>
  filter(Show=="Dancing with the Stars") |>
  mutate(confusing = Q2_Confusing >= 4)

#conduct bootstrap
pilot_bootstrap3= do(1000)*prop(~confusing, data=resample(pilot4))

#make a histogram
ggplot(data=pilot_bootstrap3) +
  geom_histogram(aes(x=prop_TRUE), bins=50, color="black", fill="mediumpurple4") +
  labs(
    title="Bootstrap Distribution of 95% CI: Score (Q2_Confusing ≥4)",
    x="Proportion",
    y="Count"
  ) +
  theme_minimal()

#confidence interval
confint(pilot_bootstrap3, level=0.95)




```
*problem 4*
```{r, message=FALSE, echo=FALSE,fig.width=6, fig.height=4}


#load data set
ebay <- read.csv("ebay.csv")

#compute revenue ratio
ebay <- ebay |>
  mutate(revenue_ratio = rev_after / rev_before)


#compute observed difference in revenue ratio
obs_diff <- mean(ebay$revenue_ratio[ebay$adwords_pause == 1]) - 
            mean(ebay$revenue_ratio[ebay$adwords_pause == 0])

#bootstrap
bootstrap_ebay <- do(10000) * {
  resampled_data <- resample(ebay)  # Resample the entire dataset
  diff(mean(revenue_ratio ~ adwords_pause, data = resampled_data))
}

#rename the result column
names(bootstrap_ebay) <- "result"

#plot
ggplot(data = bootstrap_ebay, aes(x = result)) +
  geom_histogram(bins = 50, color = "black", fill = "forestgreen") +
  labs(
    title = "Bootstrap Distribution of Revenue Ratio Difference",
    x = "Bootstrapped Difference in Revenue Ratio",
    y = "Frequency"
  ) +
  theme_minimal()

#compute ci
ci <- quantile(bootstrap_ebay$result, c(0.025, 0.975))

#print
print(paste("95% Confidence Interval:", round(ci[1], 4), "to", round(ci[2], 4)))
print(paste("Observed Difference in Revenue Ratio:", round(obs_diff, 4)))




```


