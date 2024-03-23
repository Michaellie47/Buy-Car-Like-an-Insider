---
title: "Buy Car like an Insider"
author: "Michael Song, Vivian Han, Michael Lie"
date: "`r Sys.Date()`"
output: openintro::lab_report
---

```{r load-packages, message=FALSE, include=FALSE}
library(tidyverse)
library(broom)
library(tidymodels)
library(patchwork)
library(lubridate)
```

```{r, message=FALSE, warning=FALSE, include=FALSE}

carprice <- read_csv("CarPrice_Assignment.csv")

```

# Introduction

The emerging electric cars are still problematic and people tend to buy fuel cars on this post pandemic period.

The National Bureau of Economic Research has concluded that starting in March 2020, job loss was rapid. About 16 million jobs were lost in the United States in the three weeks ending on 4 April. Approximately 5.4 million Americans lost their health insurance from February to May 2020 after losing their jobs. On 30 July 2020, it was reported that the U.S. 2nd quarter gross domestic product fell at an annualized rate of 33%. Additionally, according to the Light (gas) vehicle retail sales in the United States from 1976 to 2021 report conducted by Mathilde Carlier, the auto industry in the United States sold approximately 14.9 million light (gas) vehicle units, including retail sales of about 3.3 million autos and just under 11.6 million light truck units in the last year, which is quite interesting since the sale of gas cars even went up comparing to the year before last year when the pandemic hasn’t started. Above all, it is very important for us to carry out an analysis of the factors that might affect the price of a gas car, and help people choose the right, cheap, suitable car during the harsh post-pandemic period.

The data set that we are going to do analysis on is called “CarPrice_Assignment”(https://www.kaggle.com/hellbuoy/car-price-prediction). It is created by Manish Kumar, who works for the consulting company SAP. This data set was initially created in 2020 for a Chinese automobile company Geely Auto which wants to enter the American commercial automobile market in order to model the price of cars with the available independent variables. In this data set, there are 26 columns which contain 14 numeric variables and 12 categorical variables, and each variable represents an essential attribute of an automobile within the American market.

We decided to use the existing data to provide a general insight of the traditional automobile, which takes fossil fuel as primary energy source, for the American residents who are considering purchasing cars during the post-pandemic time.

### Codebook

| Variable Name | Variable Type | Description | Potential Values |
|---------------|---------------|-------------|------------------|
|`fueltype` | categorical variable | Car fuel type | gas, diesel | 
|`aspiration` | categorical variable | Aspiration used in a car | std, turbo | 
|`carbody` | categorical variable | Type of the body of a car | convertible, hardtop, hatchback, sedan, wagon | 
|`drivewheel` | categorical variable | Type of drive wheel | 4wd, fwd, rwd | 
|`carlength` | numeric variable | Length of car | 141.1 - 208.1 | 
|`carwidth` | numeric variable | Width of car | 60.3 - 72.3 | 
|`citympg` | numeric variable | Mileage in city | 13 - 49 | 
|`highwaympg` | numeric variable | Mileage on highway | 16 - 54 | 
|`price` | numeric variable | Price of car | 5118 - 45400 |
|`log_price`| numeric variable | Log10(price of car)| 3.7091 - 4.6571|


```{r, echo=FALSE}

glimpse(carprice)

```

### Response Variable

We decided to have `price` as our response variable because all of the car factors within this data set and our motivation that leads us choosing this data set exhibit changes in price.

Below we see a histogram distribution in `price` yet, it is skewed to the right and to undo the skewness of this distribution, we decided to take the log transformation of `price` with a base of 10 that transforms it into `log_price`

```{r echo=FALSE, fig.height=5, fig.width=15, message=FALSE, warning=FALSE}

price_1 <- carprice %>%
  ggplot(mapping = aes(x = price))+
  geom_histogram(binwidth = 2813.42)+
  labs(
    title = "Car Price Distribution",
    subtitle="Using data from Kaggle collected by Manish Kumar",
    caption="Source: carprice",
    x = "Car price (dollars)"
  )

carprice <- carprice %>%
  mutate("log_price" = log10(price))

price_2 <- carprice %>%
  ggplot(mapping = aes(x = log_price))+
  geom_histogram(binwidth = 0.0662)+
  labs(
    title = "Log10 transformation of Car Price Distribution",
    subtitle="Using data from Kaggle collected by Manish Kumar",
    caption="Source: carprice_train",
    x = "Log price (dollars)"
  )

price_1 + price_2

```

```{r, echo=FALSE, include=FALSE}

# calculating the histograms' binwidths

(45400 - 5118) / (14.31782106)
2813.42

(4.657056 - 3.7091) / (14.31782106)
0.0662

```


### Research Questions

From the model proposed by Michael Lie, we are going to unveil the relationship between the `carlength`, `carwidth`, and `fueltype` towards the log of the `price`. These explanatory variables have been chosen through many considerations because a research shows that the bigger a car (vehicle) is, the more it costs, meaning that the price of a car is higher if it is bigger. Moreover, a research exhibit that the government has implemented a higher tax on diesel fuel type cars instead of the gas fuel type cars, which shows another reason why they shows a correlation and worth to analyze for.

From the model proposed by Michael Song, we are going to explore the relationship between the fuel economy, car body and log of car price. We would like to discover how the fuel economy and car body affect the cost of a car. Evidence has shown that cars with higher fuel economy require new engine mechanics, lightweight design, and aerodynamics improvement, which will affect car price potentially. Therefore, I am choosing the `citympg`, `highwaympg`, and `carbody` as my explanatory variables. 

From the model proposed by Vivian, we are interested in finding that how would `wheelbase` and `drivewheel` affect `citympg`. Normally the longer wheelbase is, the less fuel economy it is. Because when a car with long wheelbase tries to turn around, it needs more turns than normal cars. Therefore, it consumes more fuel, which means less `citympg`. Also, cars equipped with 2-wheel drive gets better `citympg` than all wheels and 4 wheels drive. Because 4 wheel cars have to send power to other wheels, which consume extra energy.

### Explanatory Variables

We will be analyzing on `citympg`, `highwaympg`, `carbody`, `drivewheel`, `wheelbase`, `carlength`, `carwidth`, `fueltype` as our chosen explanatory variables (x - variables).

### Challenges/ Adversities

This data set doesn't having any missing (NA) values within it that makes it a good data set to analyze, but there are some difficulties/ challenges that could appear in our work process. 

First is selecting which `carbody` to use for us to prove that the bigger a car is, the more expense a person will spend. This is due to a categorical variable with 4 levels that could pose as one of the challenges we would face in the process of analyzing the data set.

Second is the right skewed distribution of our response variable `carprice` would be an issue if we hadn't do the log transformation of the response variable to make it more symmetric and correct the skewness.

Third is how do we differentiate/ distinguish the comparison between `highwaympg` vs `price` and `citympg` vs `price`.

Fourth is how would we decide which model is suitable to be the final model when the RMSE and adjusted R-squared values are similar/ the same.

# Model Building

### Model from the Training Dataset proposed by Michael Lie

The model I came up for `log_price` is:

$\hat{log\_price} = -0.702 + 0.005 * carlength + 0.058 * carwidth + 0.061 * fueltypegas$

To interpret the model best-fit line above, we need to hold the variables that we are not interpreting the same.

If we hold other variables in the model the same. We can interpret the slope of the `carlength`, `carwidth`, and the difference of intercepts based on `fueltype`.

First is for `carlength` to increase by 1, `price` by a factor of 10 is expected to increase on average, by 0.005.

Second is for `carwidth` to increase by 1, `price` by a factor of 10 is expected to increase on average, by 0.058.

Third is for `log_price` of cars with gas `fueltype` elements is higher on average, by 0.061.

To conclude my final model as it is stated above, I've made a lot of considerations in choosing the best explanatory variables that exhibits high correlations towards the response variable (`log_price`). First thing to do is to be setting the seed which then splitting the carprice data set here into two data sets, `carprice_train` (training data set) that we would want to calculate its adjusted r-squared and `carprice_test` (testing data set) which later we would want to use for calculating the Root Mean Square (RMSE). Throughout the process for building model, we will be using our training data set `carprice_train`.

```{r, echo=FALSE}

set.seed(100)

carprice <- initial_split(carprice, prop = 0.5)

carprice_train <- training(carprice)

carprice_test <- testing(carprice)

```

Before conducting the plots and summaries of my chosen explanatory variables, a combination between my background knowledge and research states that the bigger the vehicle (car) then the more capital it is needed to build the car which leads to higher prices. Moreover, `diesel` fuel engine type cost much more than `gas` fuel engine in the US which bigger cars uses `diesel` more than `gas` for smaller cars.

Energy Information Administration in Washington.DC exhibit that "The transition to less polluting, lower-sulfur diesel fuels in the United States affected diesel fuel production and distribution costs. The federal excise tax for on-highway diesel fuel of 24.3 cents per gallon is 6 cents per gallon higher than the federal excise tax on gasoline" (https://www.eia.gov/tools/faqs/faq.php?id=9&t=9#:~:text=The%20transition%20to%20less%20polluting,federal%20excise%20tax%20on%20gasoline.)

Below are summaries and plots that lead me to choose the explanatory variables that I'm going to implement in my model building. 

The box plot below exhibits a correlation on one of my chosen explanatory variable (categorical) which is `fueltype` and `log_price`.

```{r echo=FALSE, fig.height=5, fig.width=15, message=FALSE, warning=FALSE}

carprice_train %>%
  ggplot(mapping = aes(x = log_price,
                       fill = fueltype))+
  geom_boxplot()+
  labs(
    title = "Fuel Type vs Car Price",
    subtitle="Using data from Kaggle collected by Manish Kumar",
    caption="Source: carprice_train",
    x = "Log price (dollars)")+
  theme(
    axis.ticks.y=element_blank(),
    axis.text.y=element_blank()
  )

```
The histogram below will provide a stronger proof between the correlation of `log_price` and `fueltype`. Since the price of gas is cheaper than diesel then the histogram below proves the insight above, where a majority would choose to lean on using gas cars than diesel cars.

```{r echo=FALSE, fig.height=5, fig.width=15, message=FALSE, warning=FALSE}

carprice_train %>%
  ggplot(mapping = aes(x = log_price,
                       fill = fueltype))+
  geom_histogram(binwidth = 0.0662)+
  labs(
    title = "Fuel Type vs Car Price",
    subtitle="Using data from Kaggle collected by Manish Kumar",
    caption="Source: carprice_train",
    x = "Log price (dollars)")

carprice_train %>%
  count(fueltype)

```

Below are two scatter plots that I've used to determine which two explanatory variables (numerical) that I'm going to use. Both scatter plots proves that `carlength` and `carwidth` has some kind of linear relationship with a positive trend as expected at the start of this model building section with the `log_price`.

```{r echo=FALSE, fig.height=5, fig.width=15, message=FALSE, warning=FALSE}

scatterplot1 <- carprice_train %>%
  ggplot(mapping = aes(x = carlength,
                       y = log_price))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(
    title = "Car Length vs Car Price",
    subtitle="Using data from Kaggle collected by Manish Kumar",
    caption="Source: carprice_train",
    x = "Car length (inches)",
    y = "Log price (dollars)"
    )

scatterplot2 <- carprice_train %>%
  ggplot(mapping = aes(x = carwidth,
                       y = log_price))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(
    title = "Car Width vs Car Price",
    subtitle="Using data from Kaggle collected by Manish Kumar",
    caption="Source: carprice_train",
    x = "Car width (inches)",
    y = "Log price (dollars)"
  )

scatterplot1 + scatterplot2

```


Furthermore, let's exhibit the correlation values numerically between `carlength` and `log_price` as well as `carwidth` and `log_price`. `carlength` and `log_price` has a correlation value of 0.795 ~ 0.80 (rounded up). `carwidth` and `log_price` has a correlation value of 0.837 ~ 0.840 (rounded up). These values shows that they're nearing the number 1, meaning that both exhibits strong relationships with the response variable.

```{r, echo=FALSE}

carprice_train %>%
  summarise(cor(log_price, carlength),
            cor(log_price, carwidth))


```

Nevertheless, conducting the inference technique of confidence interval to prove the correlations between the numeric explanatory variables and the response variable is a crucial thought-process before building the model.

Below are showing that we're 95% confidence that the slope lies between the lower (2.5%) and upper (97.5%) boundaries. Both are showing that zero is not within the range, we can infer that there is some correlation or relationship between the variables within the data because if a slope could never be zero, this means that the variables are correlated. In other words, the line best fit could never be a horizontal line.


```{r, echo=FALSE}

set.seed(100)

boot_slope_price1 <- carprice_train %>%
                specify(log_price ~ carlength) %>%
                generate(reps = 5000, type = "bootstrap") %>%
                calculate(stat = "slope")

```

```{r, echo=FALSE}

boot_slope_price1 %>%
  summarise(
    lower = quantile(stat, 0.025),
    upper = quantile(stat, 0.975)
  )

```

```{r, echo=FALSE}

set.seed(100)

boot_slope_price2 <- carprice_train %>%
                specify(log_price ~ carwidth) %>%
                generate(reps = 5000, type = "bootstrap") %>%
                calculate(stat = "slope")

```

```{r, echo=FALSE}

boot_slope_price2 %>%
  summarise(
    lower = quantile(stat, 0.025),
    upper = quantile(stat, 0.975)
  )

```

After deciding on suitable explanatory variables to implement within my model. Choosing between main-fit model and interaction model is significant to conclude the final chosen model.

We will observe below the r-squared and adjusted r-squared values between the main-fit and interaction model to determine our final model.

```{r, main-fit, echo=FALSE}

carprice_main_fit <- lm(log_price ~ carlength + carwidth + fueltype, data = carprice_train)

tidy(carprice_main_fit)

```

```{r, interaction-fit, echo=FALSE}

carprice_interaction_fit <- lm(log_price ~ carlength * fueltype + carwidth, data = carprice_train)

tidy(carprice_interaction_fit)

```
Below are the results of the R-squared as well as adjusted R-squared for each of the model above (main-fit and interaction fit model). However, we are just going to observe the adjusted R-squared value and why is that? The adjusted R-squared is a better estimate for us to choose which model will be more suitable to use because the adjusted R-squared adjusts for the number of terms in the model and its value increases if and only if the new term improves the model.

The adjusted R-squared for the main-fit model is 0.7267 while the interaction model is 0.7240.

We can see here that the interaction model's R-squared decreases by 0.0027 or we could even say that there's not much of a difference between both models adjusted R-squared, then we can use the Occam's Razor principle. Occam's Razor states that the simplest explanation is preferable to one that is more complex, simple solutions and theories are easier to execute, and verify.

How do we apply the Occam's Razor principle to decide on which model to use? Realize that the main-fit model's slope is not changing, but only for the intercepts that's changing. However, the interaction-fit model's slopes varies and this would give us different best-fit lines to interpret. In other words, the main-fit model is a simpler model, while the interaction-fit model is a more complex model.

Therefore, by Occam's Razor principle, we will stick with the main effects model because there is no significant difference between the adjusted R-squared values in the main effects model and interactions effects model.

```{r, echo=FALSE}

glance(carprice_main_fit) %>%
  select(r.squared, adj.r.squared)

glance(carprice_interaction_fit) %>%
  select(r.squared, adj.r.squared)

```

Below is the calculated main effects fit RMSE.

The Root Mean Square (RMSE) result from the `carprice_main_fit` is 0.14

```{r, include=FALSE}

carprice_pred <- predict(carprice_main_fit, newdata = carprice_test) %>%
  bind_cols(carprice_test %>%
              select(log_price)
            ) %>%
    rename(prediction = ...1)

carprice_pred

```

```{r, echo=FALSE}

rmse(carprice_pred, truth = log_price, estimate = prediction)

```


### Model from the Training Dataset proposed by Michael Song

The model I came up for `log_price` is

$\hat{log\_price} = 4.777 - 0.003 * citympg - 0.019 * highwaympg + 0.077 * carbodyhardtop - 0.126 * carbodyhatchback - 0.048 * carbodysedan - 0.121 * carbodywagon$

To interpret the model best-fit line above, we need to hold the other variables that we are not interpreting the same. If we hold other variables in the model the same, we can interpret the slope of the `citympg`, `highwaympg`, and the difference of intercepts based on `carbody`.

There are four kinds of car body, including hardtop, hatchback, sedan, and wagon, and each of them have different coefficients.
First, for `citympg` to increase by 1, `price` by a factor of 10 is expected to decrease on average by 0.003.
Second, for `highwaympg` to increase by 1, `price` by a factor of 10 is expected to decrease by 0.019.
Thirdly, for `log_price` of cars with `carbodyhardtop` is higher on average by 0.077; for `log_price` of cars with `carbodyhatchback` is lower on average by 0.126; for `log_price` of cars with `carbodysedan` is lower on average by 0.048; for `log_price` of cars with `carbodywagon` is lower on average by 0.121.

In my model building and analysis, I set the seed which splits the carprice data set into two data sets. `carprice_train` (training data set) that we would want to calculate its adjusted r-squared and `carprice_test` (testing data set) which later we would want to use for calculating the Root Mean Square (RMSE). Throughout the process for building model, we will be using our training data set `carprice_train`.

Before we dive into the regression models, I would like to explore the relationship between fuel economy(`citympg` and `highwaympg`) and `carbody` using side-by-side boxplots. The response/dependent variable Y is `citympg` & `highwaympg`, and the predictor/independent variable X is `carbody`.

Research as shown that fuel economy is highly affected by car body due to the fact that different car body have different structures and aerodynamics, which determines the resistance the car faces when driving. 

Below are summaries and plots that lead me to choose the explanatory variables that I'm going to implement in my model building. 

According to the box plot, the hatchback car shows the best fuel economy in both city and highway conditions. Since the median for the `citympg` (26.00) and `highwaympg` (31.00) of the hatchback car are the smallest among all the other car body.

Answering to the purpose of our proposal, giving people insights during the post-pandemic period, if the consumer wants to save his money in long term, the advice I would give is to buy hatchback cars, disregarding the car price.

```{r echo=FALSE, fig.height=5, fig.width=15, message=FALSE, warning=FALSE}

citympg_boxplot <- ggplot(carprice_train, aes(x=citympg, y=carbody)) + 
  geom_boxplot()+
  labs(title = "The city fuel economy of different car types",
       subtitle="Using data from Kaggle collected by Manish Kumar",
       caption="Source: Car Price dataset",
       x = "City Fuel economy(miles per gallon)",
       y = "Car Body Type")

data.hatchback.city <- carprice_train %>% 
    filter(carbody == "hatchback") %>% 
    select(citympg)
summary(data.hatchback.city)
data.hatchback.city.num <- as.numeric(unlist(data.hatchback.city))
sd(data.hatchback.city.num)

highwaympg_boxplot <- ggplot(carprice_train, aes(x=highwaympg, y=carbody)) + 
  geom_boxplot()+
  labs(title = "The highway fuel economy of different car types",
       subtitle="Using data from Kaggle collected by Manish Kumar",
       caption="Source: Car Price dataset",
       x = "Highway Fuel economy(miles per gallon)",
       y = "Car Body Type")

data.hatchback.hw <- carprice_train %>% 
    filter(carbody == "hatchback") %>% 
    select(highwaympg)
summary(data.hatchback.hw)
data.hatchback.hw.num <- as.numeric(unlist(data.hatchback.hw))
sd(data.hatchback.hw.num)

citympg_boxplot + highwaympg_boxplot
```

Then, I am going to introduce two scatter plots and one box plot in order to address our research questions. We are going to find out what are the correlation between my chosen explanatory variables(`citympg`, `highwaympg`, `carbody`) and response variable(`log_price`). Doing so can help us determine their degrees of affecting the car price respectively.

From the scatter plot below of `citympg` and `log_price`, we can conclude that the price and `citympg` seemly have a linear regression, since the trend of the data are decreasing, the two variables log_price and `citympg` have a negative correlation. Similarly, explanatory variable `highwaympg` also shows a negative correlation with the `log_price`.

According to the box plot, the graph and our calculation show that the hatchback car body is the most cheap one, which is only 10039 dollars for the mean price value.

```{r echo=FALSE, fig.height=5, fig.width=15, message=FALSE, warning=FALSE}
lmcity <- lm(log_price ~ citympg, data = carprice_train)

print(lmcity)

citympg_scatter<-ggplot(data = carprice_train, aes(x=log_price, y=citympg)) + 
  geom_point() +
  geom_smooth(method="auto", se=FALSE, linetype="dashed",
              color="red") +
  geom_smooth(method=lm, se=FALSE,
              color="blue") +
  labs( x = "log price(dollars)",
        y = "Fuel economy in city(miles per gallon)",
        caption="Source: Car Price dataset",
        subtitle="Using data from Kaggle collected by Manish Kumar",
        title = "Relationship between price and fuel economy in city")

lmhighway <- lm(log_price ~ highwaympg, data = carprice_train)

print(lmhighway)

hwmpg_scatter<-ggplot(data = carprice_train, aes(x=log_price, y=highwaympg)) + 
  geom_point() +
  geom_smooth(method="auto", se=FALSE, linetype="dashed",
              color="red") +
  geom_smooth(method=lm, se=FALSE,
              color="blue") +
  labs( x = "log price(dollars)",
        y = "Fuel economy in highway(miles per gallon)",
        caption="Source: Car Price dataset",
        subtitle="Using data from Kaggle collected by Manish Kumar",
        title = "Relationship between price and fuel economy in highway")


carbody_box<- ggplot(carprice_train, aes(x=log_price, y=carbody)) + 
  geom_boxplot()+
  labs(title = "The car price of different car types",
       subtitle="Using data from Kaggle collected by Manish Kumar",
       caption="Source: Car Price dataset",
       x = "log price(dollars)",
       y = "Car Type")

data.hatchback.price <- carprice_train %>% 
    filter(carbody == "hatchback") %>% 
    select(price)
summary(data.hatchback.price)
data.hatchback.price.num <- as.numeric(unlist(data.hatchback.price))

citympg_scatter + hwmpg_scatter + carbody_box
```

Furthermore, let's evaluate the correlation values between `citympg` and `log_price` as well as `highwaympg` and `log_price`. `citympg` and `log_price` has a correlation value of -0.77 ~ -0.80 (rounded up). `highwaympg` and `log_price` has a correlation value of -0.785 ~ 0.80 (rounded up). These values shows that they're nearing the number -1, meaning that they are both showing strong relationships with the response variable.

```{r, echo=FALSE}
carprice_train %>%
  summarise(cor(log_price, citympg),
            cor(log_price, highwaympg))

```

The lower quantile is -0.0295 and upper quantile is -0.0188. And we're 95% confidence that the slope lies between the lower (2.5%) and upper (97.5%) boundaries. Zero is not within this range, we can infer that there is some correlation between `log_price` and `citympg`. 

```{r, echo=FALSE, message=FALSE, warning=FALSE}
set.seed(100)

city_slope <- carprice_train %>% 
  specify(log_price ~ citympg) %>% 
  generate(reps = 5000, type = "bootstrap") %>% 
  calculate(stat = "slope")
city_slope %>% get_confidence_interval(level = 0.95)
```

```{r, echo=FALSE, message=FALSE, warning=FALSE}
city_slope %>% 
  summarise(
    lower = quantile(stat, 0.025),
    upper = quantile(stat, 0.075)
  )
```

The lower quantile is -0.0278 and upper quantile is -0.0183. And we're 95% confidence that the slope lies between the lower (2.5%) and upper (97.5%) boundaries. Zero is not within this range, we can infer that there is some correlation between `log_price` and `highwaympg`. 

```{r, echo=FALSE, message=FALSE, warning=FALSE}
set.seed(100)

hw_slope <- carprice_train %>% 
  specify(log_price ~ highwaympg) %>% 
  generate(reps = 5000, type = "bootstrap") %>% 
  calculate(stat = "slope")
hw_slope %>% get_confidence_interval(level = 0.95)
```

```{r, echo=FALSE, message=FALSE, warning=FALSE}
hw_slope %>% 
  summarise(
    lower = quantile(stat, 0.025),
    upper = quantile(stat, 0.975)
  )
```

Since the main-fit model's slope is not changing, and only for the intercepts are changing. Therefore, applying the Occam's Razor principle, the main fit model is a simpler model, while the interaction fit model is a more complex model. So we will stick with the main effects model afterwards.

```{r, echo=FALSE, message=FALSE, warning=FALSE}
carprice_main <- lm(log_price ~ citympg + highwaympg + carbody, data = carprice_train)

tidy(carprice_main)
```

```{r, include = FALSE}
carprice_pred_2 <- predict(carprice_main, newdata = carprice_test) %>% 
  bind_cols(carprice_test %>% 
              select(log_price) 
            )%>% 
              rename(prediction_2 = ...1)

carprice_pred_2
```

Below is the result of the adjusted R-squared for the main fit model. The adjusted R-squared is a better estimate for us to choose which model will be more suitable to use because it adjusts for the number of terms in the model and its value increases if and only if the new term improves the model.

The adjusted R squared is 0.659. There are about 65.9% of the variation in log_price is explained by carprice_main_fit model. The adjusted R squared are less than 0.7, which means it is not very effective for us to use the `citympg`, `highwaympg`, and `carbody` to explain the variation in `log_price`.  

According to the RMSE value 0.139 and comparing to range 0.907, we can see that 0.139 is smaller than 0.9. And RMSE(Root Mean Square Error) represents the difference from the data points to the predicted regression line. Therefore, the smaller the rmse is, the better the model predicts the price. So the model predicts the ratings well.

The challenges for my model is that although I have six factors within my main fit model, but the value of adjusted R-squared goes down, which means the eloquence of my model decreases.

```{r, echo=FALSE, message=FALSE, warning=FALSE}
glance(carprice_main) %>% select(adj.r.squared)

rmse(carprice_pred_2, truth = log_price, estimate = prediction_2)
```

```{r, echo=FALSE}
carprice_test%>%
  summarize(max_price=max(log_price),
            min_price=min(log_price))

4.6161 - 3.7091
```


### Model from the Training Dataset proposed by Vivian Han
 
```{r, echo=FALSE}

lm (log_price ~ citympg+ as.factor(drivewheel) + wheelbase, data=carprice_train)

```
 
$\hat{log\_price} =3.39522-0.01350 * citympg – 0.02382 * drivenwheelfwd + 0.09807 *drivenwheelrwd + 0.00991 * wheelbase$
 
Explanation of coefficient:
 
- Holding all other variables the same, for every 1 unit increase in citympg, we expect price by factor of 10 to decrease, on average, by -0.01350 units. 
 
-Holding all other variables the same, for every 1 unit increase in drivenwheelfwd, we expect price by factor of 10 to decrease, on average, by – 0.02382 units.
 
-Holding all other variables the same, for every 1 unit increase in drivenwheelrwd, we expect price by factor of 10 to increase, on average, by 0.09807 units.
 
- Holding all other variables the same, for every 1 unit increase in wheelbase, we expect price by factor of 10 to increase, on average, by 0.00991 units.
 
 
###adjusted R square 
The adjusted R squared is 0.74. There are about 74% of the variation in log_price is explained by model1. And it is worthwhile to use the wheelbase, citympg and drivewheel to explain the variation in log_price, since there are 74% being explained.

```{r, echo=FALSE}

model1<- lm(log_price~ wheelbase+citympg+drivewheel, data=carprice_train)
tidy(model1)
glance(model1) %>% select(r.squared, adj.r.squared)

```
###RMSE
According to the RMSE value 0.12 and comparing to range 0.9, we can see that 0.12 is smaller than 0.9. And RMSE(Root Mean Square Error) represents the difference from the data points to the predicted regression line. Therefore, the smaller the rmse, the better the model predicts the price. So the model predicts the ratings well.

```{r, echo=FALSE,warning=FALSE}

carprice_pred <- predict(model1, newdata=carprice_test) %>%
  bind_cols(carprice_test%>% select(log_price)) %>%
  rename(pred=...1)
rmse(carprice_pred, truth=log_price, estimate=pred)

```


#range of dataset
```{r, echo=FALSE}

carprice_test%>%
  summarize(max_price=max(log_price),
            min_price=min(log_price))
4.616108-3.7091

```

1.From the scatter plot below between `citympg` (x variable) and `price` (y variable) , we can conclude that price and `citympg` seemly have a linear regression, since the trend of the data are decreasing, the two variables price and `citympg` have a negative correlation, which is -0.69, meaning that it has a strong correlation closing to -1.
```{r echo=FALSE, fig.height=5, fig.width=15}

ggplot(data=carprice_train , 
       mapping=aes(x=citympg, y=log_price)) +
  geom_point()+ geom_smooth(method = "lm")+
  labs(title="Relationship between citympg and price",
       subtitle="Data from Kaggle collected by Manish Kumar",
       caption="Source: carprice dataset",
       x="Mileage in city",
       y="Price")

```

```{r, echo=FALSE}

Boot_slope_price3 <- carprice_train %>%
                specify (log_price ~ citympg) %>%
                generate (reps = 5000, type = "bootstrap") %>%
                calculate (stat = "slope")

```

```{r,echo=FALSE, message=FALSE,warning=FALSE}

city_slope <- carprice_train %>% 
  specify(citympg ~ log_price) %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "slope")
city_slope %>% get_confidence_interval(level = 0.95)

```

The lower quantile is -30.44031 and upper quantile is -22.0157. We 95% confident that the slope lies between the lower (2.5%) and upper (97.5%) boundaries. From the lower quantile to upper quantile zero is not within the range, we could infer that there is a relationship between the two variables. This means log_price and citympg are somehow related. 
 
```{r, echo=FALSE, message=FALSE, warning=FALSE}

city_slope %>% 
  summarise(
    lower = quantile(stat, 0.025),
    upper = quantile(stat, 0.975))

```

2.The following boxplot describes the relationship between x variable drivewheel and y variable price. After comparing the median, we can conclude that rwd (Rear Wheel Drive) sell the highest price by factor of 10 among three types of drivewheel.
 
```{r echo=FALSE, fig.height=5, fig.width=15}

 boxplot(split(carprice_train$log_price, carprice_train $drivewheel), xlab="drivewheel", ylab="log_price") 

```

3. The following distribution describes relationship between response variable price and explanatory variable wheelbase (distance between center of the front wheels and center of the rear wheels). The histogram is right skewed, with outliers. The mean of histogram is about 98.9, median is 97 and standard deviation is 6. We can conclude that most wheelbase are around 97 inches with the highest price among data set.

```{r, echo=FALSE}

carprice_train %>% nrow() 
carprice_train  %>%
  summarise(max_wheelbase=max(wheelbase),
            min_wheelbase=min(wheelbase),
            median_wheelbase = median(wheelbase),
            mean_wheelbase = mean(wheelbase),
            sd_wheelbase= sd(wheelbase))
(120.9-86.6)/sqrt(205)

```

```{r echo=FALSE, fig.height=7.5, fig.width=15}

ggplot(data=carprice_train, 
       mapping=aes(x=wheelbase)) +
  geom_histogram(binwidth=2) +
  labs(title="Distribution between wheelbase and price",
       subtitle="Data from Kaggle collected by Manish Kumar",
       caption="Source: carprice dataset",
       x="wheelbase",
       y="log_price")

```
 
```{r, echo=FALSE}

wheelbase_slope <- carprice_train %>% 
  specify(wheelbase ~ log_price) %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "slope")
city_slope %>% get_confidence_interval(level = 0.95)

```

The lower quantile is 15.56991 and upper quantile is 25.87954. And we're 95% confidence that the slope lies between the lower (2.5%) and upper (97.5%) boundaries. Zero is not within this range, we can infer that there is some correlation between log_price and wheelbase. 
 
```{r, echo=FALSE}

wheelbase_slope%>% 
  summarise(
    lower = quantile(stat, 0.025),
    upper = quantile(stat, 0.975))

```

### Tabular Comparison of each Model's Results

 Model        | Training Adjusted R-squared | Testing Root Mean Square Error (RMSE)
--------------|-----------------------------|----------------------------------
 Michael Lie  |           0.730             |             0.139
 Michael Song |           0.659             |             0.139
 Vivian Han   |           0.746             |             0.122


# Results

We've decided to go with Model proposed by Michael Lie as our final model (best model) that we will fit to the original whole data set. This final model we chose is based on the adjusted R-squared value, RMSE value, complexity of the model, and how well it would be to explain our motivation and scientific questions in the introduction part.

As we know, the higher of an adjusted R-squared value exhibit a better model because the Adjusted R-squared values explains the percentage of the variability towards the model. As for the Root Mean Square Error (RMSE), we would want a low RMSE because it represents the difference from the data points to the predicted regression line.

After comparing all three models, we can see that Vivian Han's model exhibits the highest adjusted R-squared value yet, Michael Lie's and Michael Song's shows the lowest RMSE values. These values conflicts to what we define above because the highest adjusted R-squared value and lowest RMSE values comes from different models. However, we can see that the difference of adjusted R-squared values between Michael Lie's and Vivian's model is within 10^-2 which means a really small difference of adjusted R-squared can be seen here and Michael Lie's model proves a lower RMSE value by a 10^-1 difference with Vivian's. Therefore, we decided that Michael Lie's model is the best model that we would fit to the original data set.

In addition, after a long back and forth discussion concerning why choosing Michael Lie's model rather than Vivian's, we came up with the conclusion that Vivian's explanatory variables doesn't explain our motivation as well as scientific questions as well as Michael Lie's explanatory variables. This is due to we are trying to provide insights on buying a car within the US economically wise and not performance wise which Vivian's explanatory variables tries to explain (`citympg`, `wheelbase`, `drivewheel`).

Furthermore, choosing Michael Lie's result would help us succeeding our motivation that starts all of this data analysis. We decided to analyze this data because we wanted to provide general insights of the traditional automobile, which takes fossil fuel as primary energy source, for the American residents who are considering purchasing cars during the post-pandemic time. Through analyzing a variety of factors that would affect `Price` that later we transformed into `log_price` (reasoning explained above), we could help people to spend their money on a car that its price is equivalent the value of its efficiency especially during the harsh post-pandemic period that affects the world's economy on a global scale.


We could see how price increases when the size of a car increases (`carlength`, `carwidth`) as well as how a majority of people prefer gas cars because the US government impose less tax on the gas `fueltype` cars rather than the diesel `fueltype` ones. 

$\hat{log\_price} = -0.702 + 0.005 * carlength + 0.058 * carwidth + 0.061 * fueltypegas$

To interpret the final model above which is from the whole data set, we need to hold the variables that we are not interpreting the same.

If we hold other variables in the model the same. We can interpret the slope of the `carlength`, `carwidth`, and the difference of intercepts based on `fueltype`.

First is for `carlength` to increase by 1, `price` by a factor of 10 is expected to increase on average, by 0.005.

Second is for `carwidth` to increase by 1, `price` by a factor of 10 is expected to increase on average, by 0.058.

Third is for `log_price` of cars with gas `fueltype` elements is higher on average, by 0.061.

```{r, echo=FALSE, warning=FALSE, message=FALSE}

carprice_final_model <- lm(log_price ~ carlength + carwidth + fueltype, data = carprice)

tidy(carprice_final_model)

```

Through this analysis, we hope that within this post-pandemic situation, people within the US who are trying to own a car could place their money that reflects the value of the car (money efficiency wise). Therefore, we recommend people that within this economic recession situation (post pandemic) that they would choose smaller and gas cars simultaneously to survive within the harsh economic decline to due the COVID-19 global pandemic.


# Reference
https://www.kaggle.com/hellbuoy/car-price-prediction
https://www.fueleconomy.gov/feg/why.shtml
https://www.mblaw.org/blog/2020/04/does-the-size-of-a-car-impact-safety/ 
https://www.caranddriver.com/research/a31518112/what-is-good-gas-milage/
https://www.usautosales.info/blog/front-wheel-drive-vs-rear-wheel-drive/#:~:text=Most%20of%20the%20time%2C%20front,are%20over%20the%20front%20wheels
https://www.eia.gov/tools/faqs/faq.php?id=9&t=9#:~:text=The%20transition%20to%20less%20polluting,federal%20excise%20tax%20on%20gasoline
https://www.convenience.org/Topics/Fuels/Key-Facts-About-Diesel

























