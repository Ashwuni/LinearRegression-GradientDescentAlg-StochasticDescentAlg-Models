---
title: 'Assignment 1'
author: "Qirui Liu, Ashwuni Kumar, Gunchica Bhalla"
geometry: margin=.75in
output:
  html_document:
    df_print: paged
    theme: cosmo
  word_document: default
  pdf_document: default
header-includes:
- \usepackage{graphicx}
- \usepackage{color}
graphics: yes
fontsize: 11pt
---

<!-- You can check templates from this website and change html theme: https://www.datadreaming.org/post/r-markdown-theme-gallery/ -->
<!-- It won't affect the PDF or Word. -->

```{r}
knitr::opts_chunk$set(message=FALSE, warning=FALSE)
library('tidyverse')
songs <- read.csv('spotify_songs.csv')
head(songs)


```

## Question 1: Linear Regression
Y Variavle :danceability (numerical)

X Variables chosen:

1. playlist_genre (categorical)
2. energy (numerical)
3. valence (numerical)
4. tempo (numerical)

### 1.1. (10 pts) 

Give basic insights into your numeric variable you have picked as output variable using one categorical variable you selected. 

- What are the min / max values and median of the output variable, $Y$?
- What is median of the output value among different classes of the categorical variable you picked? You must use `group_by` and `summarize` functions.

```{r}
min(songs$danceability)
max(songs$danceability)
median(songs$danceability)
```
```{r}
group_by(songs,songs$playlist_genre)%>%
  summarise(songs_median = median(danceability))

```

### 1.2. (10 pts) 

Visualize the variables you selected.

- Draw histogram of the numeric variables you selected.
- Draw distribution of the output variable $Y$ with respect to the different classes of your categorical variable. The plot must somehow show the distributional differences among different classes. You can use boxplot, histogram, or other visuals (e.g. density ringes).

- Draw scatter plot between one of your numeric inputs and the output variable. Discuss whether the plot indicate a relation, if it is linear, if there are outliers? Feel free to remove the outlier. Feel free to transform the data.
```{r}
library('gridExtra')

g1 <- ggplot(songs, aes(x=energy)) + 
  geom_histogram(colour='white') 
g2 <- ggplot(songs, aes(x=tempo)) + 
  geom_histogram(colour='white') 
g3 <- ggplot(songs, aes(x=valence)) +
  geom_histogram(colour='white') 
g4 <- ggplot(songs, aes(x=danceability)) +
  geom_histogram(colour='white')  

grid.arrange(g1,g2,g3,g4, ncol=2)

```

```{r}
songs$playlist_genre <- as.factor(songs$playlist_genre)
ggplot(songs, aes(x=playlist_genre, y=danceability, colour=playlist_genre)) + 
  geom_boxplot()
```

```{r}
ggplot(songs, aes(y=danceability, x=valence, colour=playlist_genre)) + 
  geom_point()

```
Discuss: From visual observation, we see there is a weak, positive, linear relationship between danceability and valence. We see there are outliers along the bottom half of the graph. These outliers – mainly from the playlist genre ‘rock’ but a few outliers are from the genres ‘rap’, ‘pop’, ‘r&b’, and ‘latin’ - appear to have a flatter trend as valence increases, whilst all other data points are generally increasing linearly in danceability.  

### 1.3. (15 pts) 

Using the all dataset, fit a regression:

1. Using the one numeric input variable fit a simple regression model.

  - Write down the model.
	- Fit the regression line.
	- Summarize the output.
	- Plot the input and output variables in a scatter plot and add the predicted values as a line.
	- Interpret the results. Is it a good fit? Is your input variable good in explaining the outputs?



Model : Di ~ B0 + B1valence + Ei
```{r}
fit1 <- lm(danceability ~ valence, data = songs)
fit1
summary(fit1)
coef(fit1)


```

```{r}
preds <- predict(fit1)
ggplot(songs, aes(x=songs$valence, y=songs$danceability)) + 
  geom_point(alpha=.3, size=2) + 
  geom_line(aes(y=preds), colour="green")
sigma(fit1)
```



Ans: Summarize output: 

H0: B1 = 0 

H1: B2 != 0 

The p value coefficient for valence can be rejected confidently, as it is a nonzero value. 

The F-statistics shows that the model is significant as the F test's null hypothesis is "all the coefficients are 0" and, as the coefficient is not zero, we can reject the hypothesis and thus the model is significant. 

The R-squared is 0.1092 which seems good. 

The function is predicting the Target Value to be: 

Y = Danceability = 0.5498360 + 0.2056826*Valence 

Thus, from the f-statistics, and the r-squared value, we can determine that this model is a good fit. This can be corroborated by the RMSE value. Using the sigma () function we know that the Root Mean Squared Error (RMSE) is 0.1369333 which is not that significant, thus further implying our model is acceptable. However, adding more input variables would help in improving the accuracy of the model. The input variable is good as it has a very small p-value; where a smaller p value signifies a better ability to explain the output.   



2. Using all your input variables, fit a multiple linear regression model

   - Write down the model
   - Fit the regression line and summarize the output
   - Interpret the results. Is it a good fit? Are the input variables good in explaining the outputs?
   
Model : Di ~ B0 + B1valence +B2energy+B3tempo+B4playlist_genre +Ei
```{r}
songs$playlist_genre <- as.factor(songs$playlist_genre)
fit2 <- lm(danceability ~ valence + energy + tempo + playlist_genre, data = songs)
summary(fit2)$coefficients

preds <- predict(fit2)
sigma(fit2)
```


Ans: Using the sigma () function we know that the RMSE value is 0.1183157. This is lower, and therefore better, than the previous RMSE value of 0.1369333 - which corroborates that adding input variables helped us improve the RMSE value. As the RMSE value is relatively small we can say that this model is a good fit.  

From the t-values we can see the significance of the input variables; the significance of the input variable is gathered from how far the value is from zero. Using this method, we see our t-values of valence (76.6625907), tempo (-21.2576437), and energy (-31.3115345) imply that our input values of valence, energy and tempo of a song are significant in explaining the danceability of a song.  



3. Now, do the same things as you did, but this time add an interaction between one categorical and one numeric variable.
   - Write down the model, fit to the data, summarize and interpret the results.
   
Model : Di ~ B0 + B1valence +B2energy+B3tempo+B4playlist_genre +Ei
   
```{r}

songs$playlist_genre <- as.factor(songs$playlist_genre)
fit3 <- lm(danceability ~ valence + energy + tempo + playlist_genre +playlist_genre:valence, data = songs)
summary(fit3)$coefficients

preds <- predict(fit3)

```


Discussion: The RMSE value is 0.1180451 which is relatively low which leads us to believe that the model is acceptable. 

From the summary of output, we see that not all playlist genres significantly affect the danceability of a song. For instance, from the summary you can see that the t-value for playlist genre Latin (1.4909394) is not good in explaining the output as it is less than the desired 1.96 distance mark (post #146 from Campuswire). Alternatively, from the summary we can see the t-value and p-value are significant: the t-value (-13.4627546) is well below the desired -1.96 distance mark (post #146 from Campuswire) and the p-value (3.333464e-41) is relatively very small.  

   
4. Which model you fit is the best in predicting the output variable? Which one is the second and third best? Rank the models based on their performance.

```{r}
sapply(list(fit1,fit2,fit3),sigma)
```


The best fit is model fit3 which added the interaction, the second-best fit is model fit2 which uses all the input variables, and the third best fit is model fit1 which only uses one numerical variable. This can be determined by comparing the Root Mean Squared Error values of all three models to see which model’s prediction is closest to target. We obtain these RMSE values by using the inbuilt sigma() function in R; the values of fit1, fit2, and fit3 being 0.1369333, 0.1183157, and 0.1178535, respectively. By comparing these RMSE values we see the smallest value, and therefore the best fit, was for model fit3 and the largest value was for fit1. 



### 1.4. (15 pts) 

In this section, you will do the same you did in 1.3, but this time you will first split the data into train and test.

- Select seed to fix the random numbers you will generate using `set.seed(...)`. 
- Split your data into test and train sets with 20/80 test-train ratio. 
- Fit the model to the train set and evaluate the how well the model performed on test set.
- Which model performed the best on test set? Rank the models based ion their performance.
- Is the rank the same as the one you had in 1.3?

```{r}
set.seed(200)
train_size <- floor(0.8 * nrow(songs))
train_inds <- sample(1:nrow(songs), size = train_size)
test_inds  <- setdiff(1:nrow(songs), train_inds)

train <- songs[ train_inds , ] 
test  <- songs[ test_inds , ]

cat('train size:', nrow(train), '\ntest size:', nrow(test))
```
```{r}
library('caret')
test1 <- lm(danceability ~ valence, data = train)
test2 <- lm(danceability ~ valence + energy + tempo + playlist_genre, data = train)
test3 <- lm(danceability ~ valence + energy + tempo + playlist_genre +playlist_genre:energy, data = train)

pred1 <- predict(test1, newdata=test)
pred2 <- predict(test2, newdata=test)
pred3 <- predict(test3, newdata=test)

rmse1 <- RMSE(pred1, test$danceability)
rmse2 <- RMSE(pred2, test$danceability)
rmse3 <- RMSE(pred3, test$danceability)

rmses <- c(rmse1,rmse2,rmse3)
rmses
```


Using the same method as before – comparing RMSE values where the smallest value is the best fit – we can determine that the best fit is model fit3 which added the interaction, the second-best fit is model fit2 which uses all the input variables, and the third best fit is model fit1 which only uses one numerical variable. The ranking is the same as before.  

\pagebreak

## Question 2: Gradient Descent Algorithm (By hand)

In case you want to take a picture (screenshot) of your notebook (tablet), you can use the below lines to embed the image to the output PDF file:


```{r}
knitr::include_graphics('conspiracy.jpg')
```





\pagebreak

## Question 3. Gradient Descent Algorithm


### 3.1. Get familiar

You will use horsepower as input variable and miles per gallon (mpg) as output:

1. Plot the scatterplot between `mpg` ($Y$) and `horsepower` ($X$).
    - Is the relationship positive or negative? Does mpg increase or reduce as horsepower increases?
    - Is the relationship linear?
2. Plot the scatterplot between `log(mpg)` and `log(horsepower)`.
    - Is the relationship positive or negative?
    - Is the relationship linear?
3. Which of the two versions is better for linear regression?


```{r}
library(ISLR)
head(Auto)
```


```{r}
ggplot(Auto, aes(y=mpg, x=horsepower)) + 
  geom_point()

fit4 <- lm(mpg ~ horsepower, data = Auto)
fit4
plot(fit4)

```


3.1.1 Ans: From the slope of the linear regression line and from visual observation we can determine that mpg and horsepower have a negative relationship. In other words, as the horsepower increases, mpg decreases and vice versa. With respect to the linearity of the line, we can use the residual plot of the model to determine this. The residual plot (refer to Residuals vs Fitted above) demonstrates a patten as the slope of the plot is not flat, therefore implying that this is not a linear relationship. This conclusion agrees with the teams’ opinion as the scatterplot does not seem to be following a very linear trend in our opinion. 



       
```{r}
ggplot(Auto, aes(y=log(mpg), x=log(horsepower))) + 
  geom_point()
```





3.1.2 From the slope of the linear regression line and from visual observation we can determine that log(mpg) and log(horsepower) have a negative relationship. In other words, as the log(horsepower) increases, log(mpg) decreases and vice versa. With respect to the linearity of the line, we can use the residual plot of the model to determine this. The residual plot (refer to Residuals vs Fitted below) slope is relatively flat, therefore implying that this is a linear relationship. This conclusion agrees with the teams’ opinion as the scatterplot seem to be following a relatively linear trend in our opinion. 
  
  
```{r}
fit4 <- lm(mpg ~ horsepower, data = Auto)
fit4
#plot(fit4)

fit5 <- lm(log(mpg) ~ log(horsepower), data = Auto)
fit5
plot(fit5)

sapply(list(fit4,fit5),sigma)

```


3.1.3 Ans: Using the inbuilt sigma() function to determine the RMSE values for the two plots, we see that the RMSE value of second version – log(mpg) and log(horsepower) – is significantly lower than the RMSE value of the first version. Therefore, we can confidently claim that the second version – log(mpg) and log(horsepower) – is better for linear regression. 

### 3.2. Fill in the code

The code below estimates the coefficients of linear regression using gradient descent algorithm. If you are given a single linear regression model;

$$Y = \beta_0 + \beta_1 X $$

where $Y=[Y_1,\dots,Y_N]^T$ and $X=[X_1,\dots,X_N]^T$ are output and input vectors containing the observations.

The algorithm estimates the parameter vector $\theta = [\beta_0,\beta_1]$ by starting with an arbitrary $\theta_0$ and adjusting it with the gradient of the loss function as:

$$\theta := \theta + \frac \alpha N X^T(Y - \theta X)$$

where $\alpha$ is the step size (or learning rate) and $(Y - \theta X)^T X$ is the gradient. At each step it calculates the gradient of the loss and adjusts the parameter set accordingly.
```{r}



GDA <- function(x, y, theta0, alpha = 0.01, epsilon = 1e-8, max_iter=25000){
  
  # Inputs
  # x      : The input variables (M columns)
  # y      : Output variables    (1 column)
  # theta0 : Initial weight vector (M+1 columns)
  
  x     <- as.matrix(x)
  y     <- as.matrix(y) 
  N     <- nrow(x)
  i     <- 0
  theta <- theta0
  x     <- cbind(1, x) # Adding 1 as first column for intercept
  imprv <- 1e10
  cost  <- (1/(2*N)) * t(x %*% theta - y) %*% (x %*% theta - y)
  delta <- 1
  while(imprv > epsilon & i < max_iter){
    i <- i + 1
    grad <- (t(x) %*% (y-x %*% theta))
    theta <- theta + (alpha / N) * grad
    cost  <- append(cost, (1/(2*N)) * t(x %*% theta - y) %*% (x %*% theta - y))
    imprv <- abs(cost[i+1] - cost[i])
    if((cost[i+1] - cost[i]) > 0) stop("Cost is increasing. Try reducing alpha.")
  }
  if (i==max_iter){print(paste0("maximum interation ", max_iter, " was reached"))} else {
    print(paste0("Finished in ", i, " iterations"))
  }
  
  return(theta)
  
}
```

### 3.3. Run GDA


1. Run the code with the above parameters. How many iterations did it take to estimate the parameters?
```{r}
plot_line <- function(theta) {
  ggplot(Auto, aes(x=log(horsepower),y=log(mpg))) + 
    geom_point(alpha=.7) + 
    geom_abline(slope = theta[2], intercept = theta[1], colour='firebrick') + 
    ggtitle(paste0('int: ', round(theta[1],2), ', slope: ', round(theta[2],2)))
}

library('ISLR')
library('tidyverse')
x <- log(Auto$horsepower)
y <- log(Auto$mpg)
theta0 <- c(1,1)
theta   <- GDA(x, y, theta0, alpha = 0.05, epsilon = 1e-5)

x1=plot_line(theta)
x1
```
Answer :It takes 3193 iterations.



2. Reduce epsilon to `1e-6`, set `alpha=0.05` run the code. 
    - How many iterations did it take to estimate the parameters?
    - Does the result improve? Why or why not?
```{r}
plot_line <- function(theta) {
  ggplot(Auto, aes(x=log(horsepower),y=log(mpg))) + 
    geom_point(alpha=.7) + 
    geom_abline(slope = theta[2], intercept = theta[1], colour='firebrick') + 
    ggtitle(paste0('int: ', round(theta[1],2), ', slope: ', round(theta[2],2)))
}

library('ISLR')
library('tidyverse')
x <- log(Auto$horsepower)
y <- log(Auto$mpg)
theta0 <- c(1,1)
theta   <- GDA(x, y, theta0, alpha = 0.05, epsilon = 1e-6)

x2=plot_line(theta)
x2

x3 <- lm(y ~ x, data = Auto)
preds <- predict(x3)
ggplot(Auto, aes(x=x, y=y)) + 
  geom_point(alpha=.3, size=2) + 
  geom_line(aes(y=preds), colour="blue")
```


3.3.2 Ans: It took 7531 iterations to estimate the parameters. The result did improve as we can visually determine that the line is a better fit for the trend of the data. Our conclusion can be further corroborated by comparing both lines the linear regression line of the data (graph with the blue line above) and we see that the slope of the second estimation – where epsilon is 1e-6 and alpha=0.05 - is much closer to that of the linear regression.  

    
3. Reduce alpha to `alpha=0.01`
   - How many iterations did it take?
   - Did the resulting line change? Why or why not?
   
```{r}
plot_line <- function(theta) {
  ggplot(Auto, aes(x=log(horsepower),y=log(mpg))) + 
    geom_point(alpha=.7) + 
    geom_abline(slope = theta[2], intercept = theta[1], colour='firebrick') + 
    ggtitle(paste0('int: ', round(theta[1],2), ', slope: ', round(theta[2],2)))
}

library('ISLR')
library('tidyverse')
x <- log(Auto$horsepower)
y <- log(Auto$mpg)
theta0 <- c(1,1)
theta   <- GDA(x, y, theta0, alpha = 0.01, epsilon = 1e-6)

plot_line(theta)
```

3.3.3 Ans: It took 22490 iterations to estimate the parameters. The resulting line did change. This is because the alpha impacts how often we recalculate the gradient. A smaller alpha results in the gradient being recalculated more often. Thus, the smaller alpha – 0.01 -  translates to a better fitted line as more frequent recalculation of the gradient will increase the chances of obtaining an optimal solution. 


4. Set alpha back to `alpha=0.05` and try `theta0=c(1,1)` vs. `theta0=c(1,-1)`:
   - How many iterations did it take? Which is less than the other?
   - Why starting with a negative slope have this effect?
   
```{r}
plot_line <- function(theta) {
  ggplot(Auto, aes(x=log(horsepower),y=log(mpg))) + 
    geom_point(alpha=.7) + 
    geom_abline(slope = theta[2], intercept = theta[1], colour='firebrick') + 
    ggtitle(paste0('int: ', round(theta[1],2), ', slope: ', round(theta[2],2)))
}

library('ISLR')
library('tidyverse')
x <- log(Auto$horsepower)
y <- log(Auto$mpg)
theta0 <- c(1,1)
theta   <- GDA(x, y, theta0, alpha = 0.05, epsilon = 1e-6)

fit6 = plot_line(theta)

```

```{r}
plot_line <- function(theta) {
  ggplot(Auto, aes(x=log(horsepower),y=log(mpg))) + 
    geom_point(alpha=.7) + 
    geom_abline(slope = theta[2], intercept = theta[1], colour='firebrick') + 
    ggtitle(paste0('int: ', round(theta[1],2), ', slope: ', round(theta[2],2)))
}

library('ISLR')
library('tidyverse')
x <- log(Auto$horsepower)
y <- log(Auto$mpg)
theta0 <- c(1,-1)
theta   <- GDA(x, y, theta0, alpha = 0.05, epsilon = 1e-6)

fit7 = plot_line(theta)
grid.arrange(fit6,fit7, ncol=2)

```


3.3.4 Ans: The first estimation – alpha=0.05 and theta0=c(1,1) - took 7531 iterations, and the second estimation - alpha=0.05 and theta0=c(1,-1) - took 7265 iterations. The second estimation took less iterations because the theta0=c(1,-1) matches the negative slope of the data. This initial similarity resulted in less iterations being required to complete the estimation as the starting point was closer to the end result.  


5. Reduce epsilon to `epsilon = 1e-8` and try `alpha=0.01`, `alpha=0.05` and `alpha=0.1`.
   - What effect does alpha have on iterations and resulting fitted line?

```{r}
plot_line <- function(theta) {
  ggplot(Auto, aes(x=log(horsepower),y=log(mpg))) + 
    geom_point(alpha=.7) + 
    geom_abline(slope = theta[2], intercept = theta[1], colour='firebrick') + 
    ggtitle(paste0('int: ', round(theta[1],2), ', slope: ', round(theta[2],2)))
}

library('ISLR')
library('tidyverse')
x <- log(Auto$horsepower)
y <- log(Auto$mpg)
theta0 <- c(1,-1)
theta   <- GDA(x, y, theta0, alpha = 0.01, epsilon = 1e-8)

fit8 = plot_line(theta)
fit8
```

```{r}
plot_line <- function(theta) {
  ggplot(Auto, aes(x=log(horsepower),y=log(mpg))) + 
    geom_point(alpha=.7) + 
    geom_abline(slope = theta[2], intercept = theta[1], colour='firebrick') + 
    ggtitle(paste0('int: ', round(theta[1],2), ', slope: ', round(theta[2],2)))
}

library('ISLR')
library('tidyverse')
x <- log(Auto$horsepower)
y <- log(Auto$mpg)
theta0 <- c(1,-1)
theta   <- GDA(x, y, theta0, alpha = 0.05, epsilon = 1e-8)

fit9 = plot_line(theta)
fit9
```

```{r}
# plot_line <- function(theta) {
#   ggplot(Auto, aes(x=log(horsepower),y=log(mpg))) + 
#     geom_point(alpha=.7) + 
#     geom_abline(slope = theta[2], intercept = theta[1], colour='firebrick') + 
#     ggtitle(paste0('int: ', round(theta[1],2), ', slope: ', round(theta[2],2)))
# }
# 
# library('ISLR')
# library('tidyverse')
# x <- log(Auto$horsepower)
# y <- log(Auto$mpg)
# theta0 <- c(1,-1)
# theta   <- GDA(x, y, theta0, alpha = 0.1, epsilon = 1e-8)
# 
# fit10 = plot_line(theta)
# 
# grid.arrange(fit8,fit9,fit10, ncol=2)

```


Comments: The code with alpha = 0.1 for this question has been written and commented out above as the program says the cost is increasing and we should therefore reduce alpha. 
The alpha impacts the size of the step for each iteration. A higher alpha result in larger steps which could lead to passing the minimum value and a lower alpha result in smaller steps which takes longer but converges to the lowest point. As a result, the alpha and the number of iterations share an inverse relationship as a smaller step size – a smaller alpha – results in more steps – more iterations – needing to be made and vice versa. Furthermore, alpha impacts the resulting fitting line due to the learning rate and how often we recalculate the gradient. A smaller alpha result in the gradient being recalculated more often. This, in turn, translates to a better fitted line as this constant recalculation of the gradient will lessen the chances of simply quickly obtaining a suboptimal solution. 

\pagebreak


4. BGD vs. SGD (20 pts)

Below code is the same code as before GDA, but it calculates the gradient using for loops instead of matrix algebra. In addition to the previous one, it returns the cost calculated after each epoch (iteration).

```{r}
BGD <- function(x, y, theta0, alpha = 0.01, epsilon = 1e-8, max_iter=25000){
  
  # Inputs
  # x      : The input variables (M columns)
  # y      : Output variables    (1 column)
  # theta0 : Initial weight vector (M+1 columns)
  
  x     <- as.matrix(x)
  y     <- as.matrix(y) 
  N     <- nrow(x)
  i     <- 0
  theta <- theta0
  x     <- cbind(1, x) # Adding 1 as first column for intercept
  imprv <- 1e10
  cost  <- (1/(2*N)) * t(x %*% theta - y) %*% (x %*% theta - y)
  delta <- 1
  while(imprv > epsilon & i < max_iter){cost
    i <- i + 1
    grad <- 0
    for(j in 1:length(y)){
      grad_chng <- x[j, ] * c(y[j]-x[j, ] %*% theta)
      grad <- grad + grad_chng 
    }
    theta <- theta + (alpha / N) * grad
    cost  <- append(cost, (1/(2*N)) * t(x %*% theta - y) %*% (x %*% theta - y))
    imprv <- abs(cost[i+1] - cost[i])
    if((cost[i+1] - cost[i]) > 0) stop("Cost is increasing. Try reducing alpha.")
  }
  print(paste0("Stopped in ", i, " iterations"))
  
  cost <- cost[-1]
  return(list(theta,cost))
}

x <- log(Auto$horsepower)
y <- log(Auto$mpg)
res <- BGD(x, y, c(1, -1), alpha = 0.005, epsilon = 1e-5, max_iter = 10)

theta <- res[[1]]
loss  <- res[[2]]
ggplot() + 
  geom_point(aes(x=1:length(loss), y=loss)) +
  labs(x='iteration')
tail(loss, n=1)

```


SGD:
```{r}
SGD <- function(x, y, theta0, alpha = 0.01, epsilon = 1e-8, max_iter=25000){
  
  # Inputs
  # x      : The input variables (M columns)
  # y      : Output variables    (1 column)
  # theta0 : Initial weight vector (M+1 columns)
  
  x     <- as.matrix(x)
  y     <- as.matrix(y) 
  N     <- nrow(x)
  i     <- 0
  theta <- theta0
  x     <- cbind(1, x) # Adding 1 as first column for intercept
  imprv <- 1e10
  cost  <- (1/(2*N)) * t(x %*% theta - y) %*% (x %*% theta - y)
  delta <- 1
  while(imprv > epsilon & i < max_iter){cost
    i <- i + 1
    for(j in 1:length(y)){
    grad <- 0
    grad_chng <- x[j, ]  * c(y[j]-x[j, ] %*% theta)
    grad <- grad + grad_chng 
    theta <- theta + (alpha / N) * grad

    }
    cost  <- append(cost, (1/(2*N)) * t(x %*% theta - y) %*% (x %*% theta - y))
    imprv <- abs(cost[i+1] - cost[i])
    if((cost[i+1] - cost[i]) > 0) stop("Cost is increasing. Try reducing alpha.")
  }
  print(paste0("Stopped in ", i, " iterations"))
  
  cost <- cost[-1]
  return(list(theta,cost))
}

x <- log(Auto$horsepower)
y <- log(Auto$mpg)
res <- SGD(x, y, c(1, -1), alpha = 0.005, epsilon = 1e-5, max_iter = 10)

theta <- res[[1]]
loss  <- res[[2]]
ggplot() + 
  geom_point(aes(x=1:length(loss), y=loss)) +
  labs(x='iteration')

tail(loss, n=1)

```

4.2 Ans: Both algorithms yielded minimum loss. At a glance, the resulting losses do not appear to differ from the only observation. However, the SGD yielded a higher result (2.519) than BGD (2.23) in reality if we look at the actual values of the last point.


