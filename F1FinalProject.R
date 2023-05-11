```R
#Main Research Question: What are the most significant factors contributing towards the driver ratings in the official Formula One video game?
#Side Questions: 
# - Which is the most significant interaction / combination of factors towards the driver ratings?
# - Does a large social media following lead to inflated or deflated driver ratings, if there is any effect at all? 

#Types of Data Used / Explored: 
# - Driver ratings from the last three F1 video games, obtained from the games’ official website / GitHub repositories. 
# - Real-world / on-track driver statistics, obtained from the official F1 website (which has a dedicated statistics page). 
# - Driver social media statistics, obtained from news articles.
#This data is all publicly available, but all of the datasets (besides those on GitHub) were not able to be downloaded in any format (e.g., csv, etc.). Thus, I had to manually input most of the data into Microsoft Excel myself to create my desired dataset.

#For clarification, the name of each annual F1 video game is "F1" + (the year that the video game was released). 
#Thus, "F1 2020", "F1 2021", and "F1 2022" are the names of three video games—released in three successive years—but all belonging to the same F1 video game franchise.

#Data Sources / Links:
#Note - The F1 video game statistics were initially discovered on GitHub repositories and then cross-referenced with the official video game websites. All of the ratings aligned, but I linked both sources anyway.  
# - F1 2020 (Video Game) Driver Ratings: https://www.formula1game.com/2020/news/f1-2020-driver-ratings-update-abudhabigp, https://github.com/toUpperCase78/formula1-datasets/blob/master/f1_2020_videogame_driver_ratings_jan2021.csv 
# - F1 2021 (Video Game) Driver Ratings: https://www.ea.com/games/f1/f1-2021/news/f1-2021-driver-ratings-qatargp, https://github.com/toUpperCase78/formula1-datasets/blob/master/f1_2021_videogame_driver_ratings_jan2022.csv  
# - F1 2022 (Video Game) Driver Ratings:  https://www.ea.com/games/f1/f1-22/driver-ratings/ratings-database, https://github.com/toUpperCase78/formula1-datasets/blob/master/f1_22_videogame_driver_ratings_initial.csv  
# - F1 (Real-World) Driver Statistics - https://www.formula1.com/en/results.html/2023/races.html 
# - F1 Driver Social Media Statistics - https://www.crash.net/f1/news/1018117/1/which-f1-drivers-have-most-social-media-followers,  https://racingnews365.com/which-drivers-have-the-most-social-media-followers, https://www.planetf1.com/features/f1-drivers-social-media-followers/

#Variables:
#Dependent Variable = Final Driver Rating 
#Driver ratings in-game update throughout the season. For the sake of consistency, I am defining the dependent variable to be the drivers’ final rating at the end of the season, which is usually determined in January of the following year (from the game’s release).
#(Potential) Independent Variables:
# - Experience, Racecraft, Awareness, Pace 
#These are all official scores listed in the F1 video game. There is no formula made publicly available as to how these scores are calculated for each driver.
#Experience is said to be related to the number of races that a driver has started / participated in over the course of their career.
#Racecraft is said to be related to the number of overtakes that a driver is able to accomplish on track (whether or not this is a season-centric or a career-centric figure is unknown). 
#Awareness is said to be related to the number of DNFs (i.e., did not finish the race) that a driver has accumulated (whether or not this is a season-centric or a career-centric metric is unknown).
#Pace is said to be related to the number of pole positions (i.e., finishing first in the qualifying race and starting the official race at the front of the pack / in the #1 position on track) that a driver has earned (again, whether or not this is a season-centric or a career-centric metric is unknown).
# - Initial Rating, Teammate Rating 
#These are both taken from the video game data. “Initial rating” refers to a drivers’ in-game rating at the beginning of that same season. “Teammate rating” refers to the final rating of a drivers’ teammate, as there are two drivers per team (for a total of twenty drivers in the league). 
# - Contract Cost, Salary, Buyout
#These figures are all listed in the video game data. The values listed are in the unit dollars per year. 
# - Career Wins, Number of WDC (World Driver's Championship) Titles, Number of WCC (World's Constructors’ Championship) Titles
#These are all career statistics taken from the official F1 website and its dedicated statistics page. 
# - Number of Wins that Season, Number of Podiums (i.e., top three finishes) that Season, WDC Points / Standings that Season, WCC Points / Standings that Season 
#These are all season statistics taken from the official F1 website and its dedicated statistics page. 
# - Number of Instagram Followers, Do They Have a Twitch Channel? 
#These figures were all taken from news articles. 
#”Do They Have a Twitch Channel?” is the only categorical variable in the dataset.
#”Number of Instagram Followers” and “Contract Cost / Salary / Buyout” are continuous, quantitative variables.
#All of the other variables are discrete, quantitative variables, as they have specific scales of values that they can take on. For instance, the “Rating” variable can (theoretically) take on any whole number between 0 to 100, though it’s more like 50 to 100 in practice. 

#Sample Size: n = 60
#The data includes the driver ratings for the last three F1 video games, from 2020 to 2022.
#These three games are the only ones who have official websites (created by EA Sports themselves, which is the company that publishes the F1 video game) dedicated to posting driver ratings. I cross-referenced the ratings listed on the official website with those listed on a GitHub repository to ensure that they matched.

f1 <- read.csv("~/Documents/R/STAT 214 Term Project/Final Data Set/EF1_Data_Set.csv", stringsAsFactors=TRUE) #Importing the dataset, which I gave the variable name "f1".
names(f1) = tolower(names(f1)) #Changing all of the column header names in the dataset to lowercase.
View(f1) #Viewing the dataset.

head(f1,3) #Displaying the first three entries of each column in the dataset.
str(f1) #Displaying the structure of the data set (e.g., column names, variable types, etc.).
summary(f1) #Displaying the five number summary for each quantitative variable and the number of counts per level for each qualitative variable. 
colnum = ncol(f1) #There are 23 columns, or 23 variables in this data set. 
rownum = nrow(f1) #There are 60 rows, or 60 observations in this data set.

#-----

#Creating Trivial Models & Averaged Models:
#The trivial model consists of the calculated mean of the "Ratings" column (which is our response variable). No regression procedure is involved.
model1 = mean(f1$rating) #The mean of the (60) driver ratings included in this dataset is 83.55. 

#Constructing a histogram of the actual driver ratings compared to the mean.
hist(f1$rating, 
     breaks=5,
     xlim=c(60,100),
     ylim=c(0,20), 
     main="Distribution of Driver Ratings in the F1 Video Game (2020-2023)",
     xlab="Driver Ratings")
abline(v=model1, col="red") #Drawing a vertical line at the value of the mean driver rating.
text(model1+5.5, 19, "Mean=83.55") #Labeling the line.

#The distribution of driver ratings appears to be slightly left-skewed based on the shape of the histogram.
#This indicates that most drivers tend to earn ratings on the higher-end of the scale and that there are only a few drivers who have much lower ratings than the mean.

med=median(f1$rating) #Calculating the median of the driver ratings, which is 84.
minimum=min(f1$rating) #Calculating the minimum, which is 69.
maximum=max(f1$rating) #Calculating the maximum, which is 95. 
interquartile=IQR(f1$rating) #Calculating the interquartile range (Q3-Q1), which is 10.
#The median of the driver ratings is 84, while the mean (from above) was calculated to 83.55. 
#Since the mean is slightly lower than the median, the ratings data is in fact slightly left-skewed.
lower_outlier = med - 1.5*interquartile #Calculating the lower bound for outliers.
View(sort(f1$rating)) #Sorting the ratings data in ascending order, from the lowest to highest scores.

#The lower cutoff point for outliers would be 69. There are two drivers in the dataset with ratings of 66 and 67. Thus, these data points are mild outliers (even if there do not appear to be any obvious outliers based on the histogram itself).
#However, there are no extreme outliers in this data set, which are defined as points that are more than three times the interquartile range above or below the median.

#The second baseline model is calculated by averaging (per driver) the four ratings that the F1 game developers base their official ratings off of.
#These four baseline rating categories are "Experience", "Racecraft", "Awareness", and "Pace."
f1$average_rating = (f1$experience + f1$racecraft + f1$awareness + f1$pace)/4 #Calculating the average of these four ratings for each observation (driver) in the dataset. Creating a new column called "average_rating" to store these averages.
model2 = lm(f1$rating ~ f1$average_rating) #Creating a regression model with "Ratings" (y) in terms of this new "Average Rating" (x) variable.
summary(model2) #Displaying a summary of the model.

#The estimated coefficient for this one independent variable (i.e., the calculated average rating) is significant with a p-value of 2*10^-16.  
#This is determined by a t-test, which determines if each β coefficient is significantly different from 0 (and thus, if x contributes information towards predicting the value of y). Estimated coefficients with significant p-values are deemed useful terms to include in the model. The individual t-tests are conducted at the ɑ=0.05 level, as is standard.
#The F-statistic is 440.5 (df1=1, df2=58) with a p-value of 2.2*10^-16. This means that the model is useful over a trivial model (like that of model1). 
#The F-statistic is a “global model utility” metric.
#The F-statistic is calculated by the following formula: F = [(SSyy-SSE) / k] / [SSE / (n-(k+1))]. Thus, the F-statistic can be understood as the amount of variability (in y) explained by the model (per each variable), divided by the amount of variability (in y) not accounted for by the model (per each data point minus its residual degrees of freedom — n-k-1). 
#The F-test for model utility sets out to determine whether or not at least one of the β coefficients is nonzero (i.e., provide convincing evidence for the alternative hypothesis), which would cause the model to have more predictive power than the trivial model (which has zero slope, as in model1).
#The F-test for model utility can be more useful / reliable than conducting individual t-tests when it comes to multiple regression models, which carry a heightened risk of at least one Type I error occurring across multiple estimated parameters / coefficients. This would mean that there could be some variables whose coefficients are not significantly different from 0, but are still included in the model due to a Type I error, or vice versa (i.e., a Type II error). 
#However, the multiple R-squared value (which is being cited instead of the adjusted R-squared value because there is only one independent variable in play at the moment) for this model is 0.8837. This implies that only 88.37% of the variability in driver ratings (y) is explained by the model as currently constructed. This figure can be improved.
#The residual standard error (s) is also listed as 2.346 (df=58). This figure can potentially be reduced in future models.

sigma = summary(model2)$sigma
cv = 100 * summary(model2)$sigma / model1 #Calculating the coefficient of variation for this model (where CV = 100*(s/yhat)).
#The coefficient of variation for this model was determined to be ~2.81. This indicates the residual standard error was only 2.81% of the mean value for driver ratings.

#CV values > 10% are often taken as proof that the residual standard error (and thus the SSE, or the sum of squared errors / residuals) for the model is too high. We are well below this threshold with a value of s that equals 2.346.
#Future models will result in further reductions to the residual standard error value. Thus, we will not have to worry about this metric any further.

#Creating a scatter plot to visualize to what extent does this model fit the data.
plot(f1$rating ~ f1$average_rating,
     xlab = 'Average of the Four Rating Criteria',
     ylab = 'Overall Rating',
     main = 'Scatterplot of Overall Rating in terms of Average of the Four Rating Criteria',
     las =1,
     pch=20
)
abline(lm(f1$rating~f1$average_rating), col="red") #Drawing the least-squares regression line calculated by this model.

#Adding the vertical residuals to the scatterplot.
x=f1$average_rating #This is the independent variable.
y=f1$rating #This is the dependent / response variable. 
yhat = summary(model2)$coefficient[1] + summary(model2)$coefficient[2]*x #This is the least-squares regression line (model2).
for (i in 1:length(x)){
  lines(c(x[i],x[i]),c(y[i],yhat[i]),lwd=2,col="black")
} #Drawing a line between each of the actual average ratings and those predicted by the LSRL (residual = actual - predicted).

#This scatterplot demonstrates that the baseline model is a reasonably good fit for the data. There appears to be a moderate, positive linear relationship between these two variables.
#However, especially at the lower and upper end of the range of x values (i.e., the calculated average ratings), the residuals appear to grow larger. This is a site for improvement. 

#The third baseline model is calculated with four independent variables, which are the four "baseline" variables for the in-game ratings.
#So, this third baseline model calculates the "Rating" DV in terms of the four IVs ("Experience", "Racecraft", "Awareness", and "Pace").
model3 = lm(rating ~ experience + racecraft + awareness + pace, data=f1)
summary(model3)

#The estimated coefficients for the terms associated with each of independent variables are significant (based on the t-tests conducted at the ɑ=0.05 level). 
#It should be noted that since the ultimate purpose of our model is for predictive use (rather than aiming for coefficient interpretability), that these values are not our main interest. Still, I believe them to be important to consider. 
#The F-statistic has increased to 689.5 (df1=4, df2=55) with an identical p-value of 2.2*10^-16, which is extremely close to 0 and indicates that the model has high predictive utility over the trivial model.
#The multiple R-squared (i.e., multiple coefficient of determination) value has increased to 0.9804 in this model. This indicates that 98.04% of the sample variation in the driver's ratings can be explained by the first-order linear model with these four independent variables, which is a significant improvement over the value of 88.37% previously reported. 
#The adjusted R-squared value (which accounts for / penalizes the increased number of independent variables in the model, in comparison to the multiple R-squared value) has also increased to 0.979 (from 0.8817). 
#The residual standard error (s) is now 0.9875 (df=55), which is a significant reduction from 2.346 (the s-value for model2).
#There remains some room to improve this model (e.g., increase the R-squared value) with additional independent variables, but this will likely compromise the interpretability of this model. Since the aim of this project is to identify a collection of independent variables that affect the driver ratings—to then predict future ratings—the damage to the interpretability isn’t too much of a cause for concern. The “best” model will be tailored to these specific purposes.

#Constructing pair plots to determine the level of correlation between the main four variables (i.e., Experience, Racecraft, Awareness, Pace).
options(repr.plot.width = 10, repr.plot.height = 10, repr.plot.res = 300)
pairs(f1[,c("experience","racecraft","awareness", "pace")],
      pch=20,
      lower.panel = NULL 
)

#Calculating the correlation coefficients between (pairs of) these four variables:
cor(f1$experience, f1$racecraft) #r = 0.458
cor(f1$experience, f1$awareness) #r = 0.182
cor(f1$experience, f1$pace) #r = 0.462
cor(f1$racecraft, f1$awareness) #r = 0.320
cor(f1$racecraft, f1$pace) #r = 0.737
cor(f1$awareness, f1$pace) #r = 0.464

#The pair plots demonstrate that each pairing amongst these four variables is positively correlated, which is consistent with their calculated r-values. 
#According to the (estimated) correlation coefficients calculated above, a few of the four "baseline" independent variables for this rating prediction model are moderately to highly correlated.
#For instance, "Racecraft" and "Pace" have a correlation coefficient (r-value) of 0.737, which indicates a high level of multicollinearity between them.
#"Experience" and "Awareness", "Experience” and “Pace", and "Awareness” and “Pace" all have r-values between 0.45 and 0.47, which indicates a moderate level of multicollinearity between these variables. This is less of a cause for concern than the first case. 
#However, these four variables are all utilized by the official EA Sports team in calculating driver ratings for the F1 game (hence why I refer to them as the “main” four predictor variables at various points in this report). 
#Thus, for practical reasons (and that of integrity), these variables do appear to be important to include in the model, despite the threat that their multicollinearity poses to identifying other influential independent variables and/or coefficient interpretability. 

#To further explore the degree of multicollinearity between these variables and whether this should be a major cause for concern…
library(car) #Calling the library "car" to calculate the VIFs (e.g., variable inflation factor) of each of the independent variables. 
#VIFs are calculated by the formula (1/(1-(Ri)^2)), where Ri^2 is the multiple coefficient of determination for the model that regresses the independent variable in question (xi) against the remaining IVs.
vif(model3)
#VIFs: "Experience" = 1.324, "Racecraft" = 2.279, "Awareness" = 1.277, "Pace" = 2.616
#None of the independent variables have a VIF value above 3. A VIF value greater than 5 indicates a moderate level of multicollinearity, while a score of 10 indicates a high level of multicollinearity.
#Since these four independent variables all have VIF values that are below this threshold, this re-affirms my confidence in keeping them all in the final model.

#-----

#The rest of this R-script explores potential improvements to these first three trivial / baseline models.
#This will culminate in the choice of a "best" possible model, which strikes a balance between both predictive ability (primary) and basic interpretability. 

#Changing Variables:
#Twitch is a social media live-streaming platform. Ever since the COVID-19 pandemic in 2020, a handful of F1 drivers have established Twitch channels. Many of them play the F1 video games on their streams.
#Some of these drivers have accumulated significant followings on the Twitch platform. It's been a way for them to expand their influence / reach beyond the F1 audience. It has also brought a lot of non-F1 fans into the sport.
#I have an inkling that drivers with greater social media followings (and prominence to the general public) might rank higher in the video game than they would otherwise. Quantifying a driver's talent isn't an objective process and there might be certain benefits to rating a well-liked driver higher than average. 
#Turning the categorical variable "Do They Have a Twitch Channel?" into a dummy variable (0,1), so that it may be used in the prediction model.
f1$twitch.channel.dummy = ifelse(f1$twitch.channel. == "Yes", 1, 0)

#Flipping the WDC and WCC standings from 1-20 to 20-1.
#For context, the WDC (World Driver's Championship) is an individual title. The driver who accumulates the most points (by finishing in top places in the races) by the end of the season wins this title.
#The WCC (World Constructors’ Championship) is a team-earned title. There are two drivers per team (and thus, twenty drivers total each season). The team whose drivers collectively earn the most points by the end of the season wins this title.
#Flipping the standings ensures that the drivers who finish the highest in the championship standings / perform the best get the greatest "weights" of this independent variable during the model building process. Otherwise, the scale for this performance metric would be inverted. This would make it unable to be interpreted alongside the other variables.
f1$wdc.standing.that.season = 21 - f1$wdc.standing.that.season #Reversing the WDC standings.
f1$wcc.standing.that.season = 21 - f1$wcc.standing.that.season #Reversing the WCC standings.

#In this step, I've removed the categorical variables that can not be used in the prediction equation (e.g., driver name, team) from the dataset.
#I also removed the average rating column (that I calculated earlier for Trivial Model #2) and the "Twitch Channel?" variable from before it was transformed into a dummy variable.
#Lastly, I removed the "WDC Points that Season" and "WCC Points that Season" columns. I decided to rely on standings rather than raw point values because different amounts of races each year result in a different amount of points that the #1, #2, #3 (and so on) drivers can earn.
#I could have transformed both of these columns into listing points per race, but I felt that this metric was a bit obscure. I see the standings as a more likely predictor variable.
f1 = subset(f1, select = -c(team, driver, wdc.points.that.season, wcc.points.that.season, twitch.channel., average_rating))

#Importing the olsrr library in order to perform all-possible and stepwise regressions for the purpose of variable screening.
#Since I have 18 potential independent variables at this point, performing these regression procedures will allow me to narrow down which independent variables deserve significant attention to potentially be included in the prediction equation.
#This will also assist me in exploring what independent variables may be correlated with each other / what multicollinearity exists in my data set.
library(olsrr)

#Creating a baseline, first-order linear model with all potential independent variables.
base_model <- lm(rating ~ ., data=f1)
summary(base_model)
#The only independent variables with significant p-values are “Experience”, “Racecraft”, “Awareness”, “Pace”, “Contract Cost”, “Wins that Season”, and “Instagram Followers.”
#The adjusted R-squared value is listed at 0.9856, which is actually quite high.
#The residual standard error has also been reduced to 0.8285 (df=41).  
#The F-statistic has lowered quite substantially to 245.5 (df1=16, df2=41), though its p-value remains at 2.2*10^-16.
#This is evidently not the best possible model.

#There are also no estimated coefficients for the "Salary" or "Buyout" independent variables. R-Studio states that this is due to "missingness."
#"Contract Cost", "Salary", and "Buyout" are all monetary figures listed on the official F1 video game website for each driver.
#Exploring the correlations between the “Contract Cost”, “Salary”, and “Buyout” variables:
cor(f1$contract.cost, f1$salary) # r=1
cor(f1$salary, f1$buyout) # r=1
cor(f1$contract.cost, f1$buyout) # r=1
#These three variables share a correlation coefficient of 1 with each other (in any combination). This means that they are all perfectly correlated.
#To eliminate this multicollinearity, I have chosen to remove the independent variables "Salary" and "Buyout" from my data set. 
#Contract cost is a more general figure, as it is the sum of a driver's salary and their buyout cost. Therefore, it is a more comprehensive means of quantifying what a driver costs / is worth on the free agency market. 
f1 = subset(f1, select = -c(salary, buyout)) #Removing "Salary" and "Buyout" columns from the data set.
new_base_model <- lm(rating ~ ., data=f1) #Constructing a new base model with the remaining potential independent variables, which will all be tested during the variable selection process.

#Variable Screening Methods - All-Possible-Regressions Selection Procedure:
#This is a brute-force method that determines the best possible first-order linear regression model for each number of potential independent variables (from 1 to 16 in this case).
#The adjusted R-squared metric is being used to determine the best possible model, since it controls for the number of independent variables actually included in the model.
#The adjusted R-squared equation reads as follows: Ra^2 = 1 - [(n-1)/(n-(k+1))]*(1-R^2). Models with higher numbers of independent variables (denoted by k) are penalized in this calculation.
k = (ols_step_best_subset(new_base_model, metric='adjr')) #Creating a subset of all possible regressions, featuring the best model for each number of IVs, and saving it to the variable “k”.
View(ols_step_best_subset(new_base_model, metric='adjr')) #Viewing this subset.
#The best model with 1 IV includes "Pace", with an adjusted R-squared value of 0.876. 
#This was a bit surprising to me. Pace is obviously a significant factor in a drivers' performance, but I expected a driver’s racecraft score, or the number of WDC titles, wins and/or podiums that they’ve accumulated throughout their career, to be more influential than pure pace.
#The best model with 4 IVs includes "Experience", "Racecraft", "Awareness", and "Pace" and has an adjusted R-squared value of 0.979. This is model3 / the baseline model, since these four IVs are the ones specified by the game developers.
#The best model with 6 IVs includes "Experience", "Racecraft", "Awareness", "Pace", "Contract Cost", and "Instagram Followers" and has an adjusted R-squared value of 0.984.
#The best model with 8 IVs includes "Experience", "Racecraft", "Awareness", "Pace", "Contract Cost", "Initial Rating", "Wins that Season", and "Instagram Followers" and has an adjusted R-squared value of 0.985.
#The adjusted R-squared value never increases above 0.987 for any of the models. 
#Based on these models, it's evident that "Experience", "Racecraft", "Awareness", and "Pace" are all relevant predictor variables that should be included in the final model. This confirms my previous speculation.
#Whether or not to include "Contract Cost", "Initial Rating", "Wins that Season", and "Initial Rating" in the final prediction equation is still up for debate, since all of these models have similar adjusted R-squared values at ~0.98.
#There is not a single model that is obviously the best of the bunch at this point. Thus, I will now attempt to use other variable screening methods and compare the models that each of them produce.

plot(k) #Using plots (from the all-steps-regression procedure) to aid in the variable selection process.
#For each of these six metrics, their value begins to plateau at an x-value between six and eight. This indicates that the “best” number of independent variables to include in the model is likely between these two numbers. 
#Eight seems to be the “turning point” for the C(p), AIC, and SBC/SBIC graphs, where the figure goes from decreasing to slowly increasing once again. 

#Variable Screening Methods - Stepwise Regression (w/ a significance level of 0.05 to enter the model and 0.10 to be eliminated from it):
#Stepwise regression is another method to identify which independent variables are useful to include in the model / eliminate those that are insignificant to the prediction equation. Unlike in forward selection and backward elimination, it continuously tests the significance of each independent variable, even after it has been already added to/eliminated from the model. It considers the influence of all of the independent variables in context with each other.
ols_step_both_p(new_base_model, penter=0.05, prem=0.1, details=TRUE)
#The stepwise regression process produces a model with eight independent variables (down from sixteen potential IVs).
#These independent variables were discovered to be “Pace”, “Initial Rating”, “Awareness”, “Racecraft”, “Experience”, “Contract Cost”, “Instagram Followers”, and “Wins that Season.”
#This is the same model as the 8 IV model from the all-possible-regressions selection procedure.

#-----

#Test (!) - Conducting forward selection and backward elimination (w/ the same significance levels) to compare the models:

#Variable Screening Methods (Comparison) - Forward Selection:
ols_step_forward_p(new_base_model, penter = 0.05, details=TRUE)
#Forward selection, in this case, produces an identical model with the same eight independent variables in the model created by stepwise regression.

#Variable Screening Methods (Comparison) - Backward Elimination:
ols_step_backward_p(new_base_model, prem = 0.1, details=TRUE)
#Backward elimination also produces an identical model.

#-----

#Summary of the Model Created via All-Possible-Regressions / Stepwise Regression Methods:
stepwise_model = lm(rating ~ experience + racecraft + awareness + pace + initial.rating + contract.cost + wins.that.season + instagram.followers, data=f1)
summary(stepwise_model)
#After testing all four of these variable screening methods, I would conclude that the linear, first-order model with eight independent variables (i.e., “Pace”, “Initial Rating”, “Awareness”, “Racecraft”, “Experience”, “Contract Cost”, “Instagram Followers”, and “Wins that Season”) is the best of the bunch.
#This model was produced by all four methods (i.e., all-possible-regressions, stepwise, forward selection, and backward elimination).
#The multiple R-squared value of this model is 0.988 and the adjusted R-squared value is 0.986. This implies that over 98.6% of the sample variation in driver ratings can be explained by the first-order linear model constructed with these eight independent variables (after this proportion is adjusted for the number of independent variables in the model).
#The root mean squared error / s-value is 0.807, which is a reduction from 0.9875 (as calculated for model3, which was the baseline model with the four main IVs).
#The s-value is calculated based on 49 degrees of freedom rather than 55 in this model, as a few observations were "deleted due to missingness." This message may be referring to some form of correlation, as this was the same phrasing used in a previous case of severe multicollinearity between the “Contract Cost”, “Salary”, and “Buyout” variables.
#The F-statistic is 516.47 (df1=8, df2=49) and the p-value for the F-test of model utility is listed as 0.0000. Thus, the model as currently constructed is significantly more useful than no model / a trivial model would be in predicting driver ratings. At least one of the β coefficients is nonzero. 
#This is a bit lower than the F-statistic for model3, but the specifications for the degrees of freedom are different, which makes them difficult to compare on a one-to-one basis.

#These metrics are all satisfactory. However, a closer examination at the prediction equation reveals a key shortcoming of the model as currently constructed.
# y = -1.764 + 0.452x1 + 0.101x2 + 0.172x3 + 0.226x4 + 0.086x5 + 0.000x6 + 0.000x7 + 0.114x8
#Where x1 = "Pace", x2 = "Initial Rating", x3 = "Awareness", x4 = "Racecraft", x5 = "Experience", x6 = "Contract Cost", x7 = "Instagram Followers", and x8 = "Wins that Season.”
#The estimated coefficients for the "Contract Cost" and "Instagram Followers" variables are listed as 0.000 (to three decimal places). Their standard error is also listed as 0.000.
#To further investigate, I will examine the standardized regression coefficients (which are listed under the "Std. Beta" column in the model summary).

#"Pace" has—by far—the highest standardized β coefficient at 0.504, followed by "Racecraft" at 0.277.
#The standardized β regression coefficient for "Contract Cost" is -0.138. The absolute value of this term makes it the fifth most influential variable in the prediction equation, with "Initial Rating", "Instagram Followers", and "Wins that Season" all being less important (according to this metric).
#However, this standardized regression coefficient implies that the "Contract Cost" variable is negatively correlated with a driver's rating, which does not appear to follow the general trend of the data.
#The standardized β regression coefficient for "Instagram Followers" is 0.065, which makes it the second "least important" variable in the prediction equation according to this metric.

#Transforming the "Contract Cost" and "Instagram Followers" variables:
f1$new.contract.cost = (f1$contract.cost)/1000000 #Creating a new column, "New Contract Cost", which consists of the original "Contract Cost" values divided by one million. Thus, this new column can be understood as "Contract Cost (in Millions)."
f1$new.instagram.followers = (f1$instagram.followers)/1000000 #Creating a new column, "New Instagram Followers" (which is just "Instagram Followers (in Millions)").

#Re-creating the stepwise model with these two transformed variables:
new_stepwise_model = lm(rating ~ experience + racecraft + awareness + pace + initial.rating + new.contract.cost + wins.that.season + new.instagram.followers, data=f1)
summary(new_stepwise_model)
#Many of the metrics from the original stepwise model (e.g., the p-values for the estimated coefficients, the s-value, the adjusted R-squared value, and the F-statistic) have not changed after this transformation. What has now changed are the values for the estimated coefficients and their standard errors. None of the estimated coefficients are equal to 0.000 (to three decimal places) anymore, and they are all still significant at the α=0.05 level according to the t-tests performed.
#None of the standard errors for the coefficients appear to be particularly large either. 
#The largest standard error (relative to the estimated coefficient) belongs to the "Initial Rating" variable. The estimated coefficient is only 2.28 times greater than the standard error, but this is not a small enough value to be too much cause for concern. 

#The estimated coefficient for the "New Contract Cost" variable is still negative under this new model, which implies that ratings tend to decrease with increases to a driver's contract cost (with all other independent variables held fixed) in this prediction equation.
#This is a bit counter-intuitive. It would, in theory, make more sense for drivers with higher ratings to be paid more (all other factors held constant).
#However, it would be misleading to assume that the remaining independent variables do not interact with each other. There is likely some variable interacting with "Contract Cost" that could be the driving force behind its negative estimated coefficient in this linear, first-order model.
#Furthermore, driver contracts are not solely based on their on-track performance. Their contract costs also reflect their "brand value", a large chunk of which involves marketing opportunities off-track.
#Thus, while it is a bit surprising, it's not totally out of the question that the coefficient for the "Contract Cost" variable might actually be negative.

f1 = subset(f1, select = -c(contract.cost, instagram.followers)) #Removing the original "Contract Cost" and "Instagram Followers" columns (pre-transformation).
new_base_model2 <- lm(rating ~ ., data=f1) #Constructing a new base model once again.

ols_step_both_p(new_base_model2, penter=0.05, prem=0.1, details=TRUE) #Repeating the stepwise regression process to ensure that it produces a model with the same eight independent variables included, which it does.

#-----

#Test (!) - Transforming the “Contract Cost” Variable into the Form ln(x):
#I've decided to test transforming the independent variable "Contract Cost" from its standard, linear form x to ln(x), since it's a monetary value.
#The ln(x) transformation would ensure that the predictive power of a driver's contract cost on their in-game rating lessens as their contract cost increases. The difference between drivers making $500,000 and $1,000,000 should have greater influence on ratings than between drivers making $9,500,000 and $10,000,000.
#Note: In R, log(x) is the natural log function.
f1$transformed.cost = log(f1$new.contract.cost) #Transforming the variable and saving it to the column "Transformed Cost."
model4 = lm(rating ~ experience + racecraft + awareness + pace + initial.rating + transformed.cost + wins.that.season + new.instagram.followers, data=f1) #Constructing the new model, with the "Transformed Cost" variable replacing the original "Contact Cost."
summary(model4)
#It turns out that the transformation of the “Contract Cost” variable into the form ln(x) damages the p-values of both the "Wins that Season" and "Instagram Followers" variables.
#This model also now has a higher residual standard error at 0.854 (compared to 0.807), which means that the average residual value (i.e., the difference between a driver's actual rating and their predicted rating according to this model) has increased, which is not ideal.
#Its F-statistic is 461.2 (df1=8, df2=49) with a p-value of 2.2*10^-16. This is evidently a large F-statistic and a small p-value, but not the best that we've seen across all models.
#Its adjusted R-squared value is also still ~0.985, which is not an improvement over the previous model (Ra^2=0.9864).
#Thus, transforming this variable does not appear to be a particularly useful procedure. There is not convincing evidence that performing this transformation benefits the model in any significant way. In fact, it seems that the transformation actually damages the significance of a few of the included variables. 

f1 = subset(f1, select = -c(transformed.cost)) #Removing the transformed variable from the dataset.

#-----

#Test (!) - Examining Multicollinearity in the Dataset:
#It should be noted that the standard errors of the estimated coefficients are inflated when multicollinearity exists in the dataset. This makes it difficult to interpret these coefficients in a meaningful way.
#Multicollinearity also makes it difficult to identify (through variable selection procedures) which IVs would be significant in the prediction equation. 

library(car) #Calling the "car" library to calculate VIFs.
vif(new_base_model2) #Calculating the VIFs for the base model (with all 16 potential IVs).
#The “Initial Rating”, “Number of WDC Titles”, “Number of WCC Titles”, “WDC Standing that Season”,  “WCC Standing that Season”, and “Career Wins” variables all have VIF values > 10.
#This means that there is severe multicollinearity present in the dataset.

#Attempting to discern where this multicollinearity exists:
cor.test(f1$career.wins, f1$number.of.wdc.titles) #Two of these variables, "Career Wins" and "Number of WDC Titles", have a correlation coefficient (r) whose point estimate is 0.990.
cor.test(f1$career.wins, f1$number.of.wcc.titles) #The point estimate for the correlation coefficient of "Career Wins" and "Number of WCC Titles" is 0.885.
cor.test(f1$wdc.standing.that.season, f1$wcc.standing.that.season) #The point estimate for the correlation coefficient between "WDC Standing that Season" and "WCC Standing that Season" is 0.940.
cor.test(f1$wcc.standing.that.season, f1$teammate.rating) #The point estimate for the correlation coefficient of "WCC Standing that Season" and "Teammate Rating" is 0.712.
cor.test(f1$initial.rating, f1$wdc.standing.that.season) #The point estimate for the correlation coefficient of "Initial Rating" and "WDC Standing that Season" is 0.802.
#All of these IVs have relatively high correlations (in the positive direction).

cor.test(f1$new.instagram.followers, f1$twitch.channel.dummy) #The point estimate for the correlation coefficient between "Instagram Followers" and "Do They Have a Twitch Channel?" is only -0.06, which is close to 0 / nonsignificant.
#I assumed that the number of Instagram followers that a driver has and whether or not they own and operate a Twitch channel would be likely correlated, since these are both social media-related factors. My hypothesis was that drivers who have larger Instagram followers would be more likely to have a Twitch channel as well. This is evidently false.

#-----

#Test (!) - Transforming the "Wins that Season" variable into the form sqrt(x):

#Since so few drivers actually manage to earn a win each season, I've decided to test a transformation of the variable "Wins that Season" from the form x to sqrt(x). This decreases the weight of each win for those drivers who have >1 win to reduce the range of the IV.
f1$transformed.season.wins = sqrt(f1$wins.that.season) #Transforming the variable and saving it to the column "Transformed Season Wins."

model5 = lm(rating ~ experience + racecraft + awareness + pace + initial.rating + new.contract.cost + transformed.season.wins + new.instagram.followers, data=f1) #Constructing the new model, with the "Transformed Season Wins" variable replacing the original "Wins that Season."
summary(model5)
#However, in the summary of this new model, the "Season Wins" variable no longer has a coefficient that is deemed significantly different from 0 at the α=0.05 level, since its p-value is ~0.09.
#The model has a higher residual standard error of 0.824 as well (with df=49). 
#Its adjusted R-squared value is 0.9858, which is lower than the 0.9864 (as calculated for new_stepwise_model). 
#Thus, there is not convincing evidence that this square root transformation adds predictive power to the model equation. I've decided to undo this transformation as a result.

f1 = subset(f1, select = -c(transformed.season.wins)) #Removing the "Transformed Season Wins" column from the dataset.

#Test (!) - Transforming "Wins that Season" into a dummy variable:

#Again, since so few drivers actually earn a win each season, I've decided to now test the effects of transforming the variable into a dummy variable.
#1 = Driver has won one or more races that season, 0 = Driver has not won a race that season.
f1$season.wins.dummy = ifelse(f1$wins.that.season > 0, 1, 0)

model6 = lm(rating ~ experience + racecraft + awareness + pace + initial.rating + new.contract.cost + season.wins.dummy + new.instagram.followers, data=f1) #Constructing the new model, with "Season Wins (Dummy Variable)" replacing the original "Wins that Season" variable.
summary(model6)
#The p-value for the estimated coefficient for "Season Wins (Dummy Variable)" is ~0.46, which is not significant.
#The residual standard error (s) has increased to 0.84 (df=49), which is not ideal.
#The adjusted R-squared value has (slightly) decreased even further to 0.9851, which is also not ideal.
#The F-statistic for model5 (w/ the ln(x) transformation) was 495.2 (df1=8, df2=49), while the F-statistic for this model is 471.7 (df1=8, df=49). Thus, the F-statistic has also decreased.
#All evidence points to this specific transformation being even less useful than the previous transformation attempt. Thus, I will not perform either of these transformations on the final model. 

f1 = subset(f1, select = -c(season.wins.dummy)) #Removing the "Seasons Wins (Dummy)" column from the dataset.

#-----

#Examining Residuals:
res = resid(new_stepwise_model) #Saving the residuals for the final stepwise model / our best model so far under the variable "res."
#Constructing a residual plot with the model's predicted (fitted) driver ratings on the x-axis and the corresponding residuals on the y-axis.
plot(x=fitted(new_stepwise_model), 
     y=res,
     xlab="Predicted / Fitted Ratings",
     ylab="Residuals",
     main="Residual Plot",
     pch=20) 
abline(0,0) #Drawing a horizontal line at y=0 on the graph.

#The residual plot (of the estimated y-values / ratings vs. the estimated residuals) depicts a clear pattern, in which the residuals are plotted further away from the line y=0 as the estimated rating increases. Thus, the absolute value of the residuals increases alongside the estimated rating.
#This implies that the data is heteroscedastic, or that the error term ε does not have a constant variance σ^2 for all settings of the independent variable.
#Based on this residual plot, the response variable (y, or the driver ratings) appears to have a multiplicative distribution. The variance grows proportionally to the square of the mean, with var(y)=[E(y)]^2 * σ^2.
#To satisfy the condition of homoscedasticity (i.e., constant variance), we can transform the response variable into the form ln(y). This is known as a variance stabilizing transformation.
#Note - At first glance, I thought the response variable followed a Poisson distribution, so I applied a sqrt(y) transformation. However, the residual plot for the new model (produced by stepwise regression after the transformation) followed a similar pattern as did the original residual plot. Thus, I discarded that attempt and went with this multiplicative case instead.

f1$transformed.rating = log(f1$rating) #Transforming the dependent variable from the form y to ln(y).
f1 = subset(f1, select = -c(rating)) #Removing the "Rating" column from the dataset.
new_base_model3 <- lm(transformed.rating ~ ., data=f1) #Constructing a new base model with the transformed dependent variable and all other independent variables.

ols_step_both_p(new_base_model3, penter=0.05, prem=0.1, details=TRUE) #Performing stepwise regression with the same conditions as previous attempts.
#Only seven independent variables are included in this stepwise model, which are "Pace", "Initial Rating", "Awareness", "Racecraft", "Experience", "Contract Cost", and "Instagram Followers."
#This means that the "Wins this Season" variable is no longer a significant predictor variable following this transformation to the dependent variable.
model8 = lm(transformed.rating ~ pace + initial.rating + awareness + racecraft + experience + new.contract.cost + new.instagram.followers, data=f1) #Creating a regression model based on the results of the stepwise regression procedure (i.e., with the same seven independent variables). 
summary(model8)
#All of the estimated coefficients in this model are listed as being significant (according to t-tests conducted at the ɑ=0.05) except for the “Instagram Followers” variable, whose p-value is ~0.052. However, this is such a small difference from 0.05 that I’d also deem this variable to be practically significant.
#The residual standard error (s) has significantly decreased. It was 0.807 (df=49) in the previous “best” model, which was created via stepwise regression as well. Here it is ~0.109 (df=50). This implies that the difference between the model’s predicted ratings versus the observed (actual) ratings has, on average, lessened quite significantly.
#The adjusted R-squared value has decreased slightly from 0.9864 to 0.9838. The F-statistic has also decreased to 494.6 (df1=7, df2=50), but the p-value remains 2.2*10^-16. 
#Still, the significant reduction to the residual standard error (while maintaining the significance / near-significance of all coefficients and a relatively high adjusted R^2 value) seems to imply that this model may be the “best” so far.

res2 = resid(model8) #Re-calculating the residuals based on this transformed model. 
#Creating another residual plot (of estimated residuals vs. the predicted values) with the transformed model.
plot(x=fitted(model8), 
     y=res2,
     xlab="Predicted / Fitted Ratings",
     ylab="Residuals",
     main="Residual Plot",
     pch=20) 
abline(0,0) #Drawing a horizontal line at y=0.
#There no longer appears to be an obvious pattern amongst the residuals, as there was previously. Thus, the transformation of the dependent variable y to the form ln(y) appears to have worked and reduced the heteroscedasticity of the model. 

#Partial residuals are calculated by adding together the estimated residual to each independent variable term (i.e., the coefficient and the variable itself).
#Partial residuals thus measure the influence of one specific independent variable on the dependent variable (independent of / separate from all other variables in the model). 
library(car) #Calling the “car” library.
crPlots(model8) #Using the “car” library to create partial residual plots.
#Based on the partial residual plots, the relationships between each of the seven independent variables and the response variable (ratings) appears to be fairly linear. None of the solid pink graphs (which depict the actual relationship between a given IV and the DV) differ too significantly from the blue, dotted lines (which depict the linear relationship between a given IV and the DV).
#The only two of the pink graphs that feature some (but still very minimal) curvature are the “Awareness” and “Instagram Followers” variables. Thus, we will test transformations on these variables.  

f1$new.awareness = log(f1$awareness) #Transforming the “Awareness” variable from the form x to ln(x), as the slightly curvilinear pink graph appears to follow a logarithmic pattern.
model9 = lm(transformed.rating ~ pace + initial.rating + new.awareness + racecraft + experience + new.contract.cost + new.instagram.followers, data=f1) #Constructing a new model with this transformed variable.
summary(model9)
#Compared to model8, the same seven independent variables are listed as having significant p-values (at the ɑ=0.05 level). The p-value for the “Instagram Followers” variable has increased from 0.052 to 0.064, though this is still somewhat small. 
#The residual standard error has been reduced slightly, from 0.1086 to 0.0107 (df=50). 
#The adjusted R-squared value has increased slightly from 0.9838 to 0.9842.
#The F-statistic has also increased from 494.6 to 509.5 (df1=7, df2=50), with its p-value still being listed as 2.2*10^-16. 
#Given that little damage to the model has resulted from this transformation, and that small improvements were made to the s-value and the Ra^2 values, I will keep this transformation as part of the model moving forward. 

crPlots(model9)
#The transformation of the “Awareness” variable from the form x to ln(x) did not actually change the shape of its partial residual plot very much (if at all). Thus, it’s safe to assume that the relationships between the IVs and the DV are all linear enough to avoid needing further transformations.
f1 = subset(f1, select = -c(new.awareness)) #Removing the transformed “Awareness” column from the dataset. 

#To check the normality assumption regarding the distribution of ε, a normal probability plot can be constructed. In a normal probability plot, the estimated residuals are graphed versus their expected values (if they were distributed normally).
qqnorm(res2) #Creating a normal probability plot, also known as a Normal Q-Q plot.
qqline(res2) #Drawing the line, which depicts the theoretical linear relationship between the estimated residuals and their theoretical values (if distributed normally). 

#Though there is some slight curvature to this graph, the points all fall reasonably close to the straight line, so the assumption is likely satisfied.
#The earlier variance stabilizing transformation that we performed (y->ln(y)) is likely responsible for this, given that non-normality and heteroscedasticity often co-occur alongside one another. 
#It should be noted that the normality assumption is also not too restrictive, so moderate departures will have little influence on the model and its estimated coefficients.

#There also do not appear to be any extreme outliers (with large vertical residuals) in this dataset based on all of the residual plots created so far.

#Testing for Influential Points:
#Leverage measures the influence of the observed y-value on its own predicted value.
#The threshold for an influential observation (i.e., one with high leverage) is determined by the following equation: hi > 2(k+1)/n
hats = hatvalues(model8) #Calculating the leverage for each point in the model.
hi = 2*(7+1)/60 #Calculating the threshold for high leverage, based on k=8 β coefficients (excluding β0) and n=60 observations.
#The threshold value is calculated to be ~0.267. 
sort(hats) #Sorting the leverage values in ascending order.
#There are four observations with leverage values greater than the threshold of 0.267:
#Observation 53 (Lewis Hamilton - 2020, Actual Rating=93) has a leverage value of  ~0.285.
#Observation 38 (Mick Schumacher - 2021, Actual Rating=80) has a leverage value of ~0.293.
#Observation 22 (Lewis Hamilton - 2021, Actual Rating=94) has a leverage value of ~0.344.
#Observation 2 (Lewis Hamilton - 2022, Actual Rating=93) has a leverage value of ~0.486.
#These are not necessarily outliers, just influential points, and “accurate” ones at that — as in, they are not the result of any kind of measurement error.
#Lewis Hamilton is widely regarded as one of the greatest Formula 1 drivers of all time. Thus, it makes sense that his ratings across the years likely act as a kind of “standard”, upon which the other drivers are graded.
#Mick Schumacher—on the other hand—was a Formula 1 rookie in 2021. His rookie status likely made his driver rating difficult for the EA Sports team to pinpoint. His driver rating was also likely influenced by the weight of his last name, which carries a great legacy in motorsport (as his father, Michael Schumacher, is also regarded as one of the greatest Formula 1 drivers of all time).
#Thus, I do not believe it to be necessary to remove these points from the dataset / the model-building process.

#-----

#Continuing with Model Building - Introducing Interaction Terms, Higher-Order Terms, etc.:

#Variable screening methods do have some key pitfalls; namely, they can only produce first-order, linear models. Higher-order terms and interaction terms are not taken into account, but very few real-world situations can be modeled by first-order, linear prediction equations.
#Thus, we will attempt to identify interaction terms / higher-order terms that might be significant for our prediction model.

#Creating a new model with an interaction term between "Pace" and "Racecraft."
#Creating a new model (a potential improvement on model8, the current “best model”) with an interaction term between "Pace" and "Racecraft."
model10 = lm(transformed.rating ~ pace + initial.rating + awareness + racecraft + experience + new.contract.cost + new.instagram.followers + pace*racecraft, data=f1) 
summary(model10)
#I figured that the "Pace" and "Racecraft" interaction term might be significant, as I believed that the combination of both factors (i.e., whether or not a driver is both fast and skilled in over-taking others on-track) would be extremely telling in how a driver performs and thus, how highly they are rated.
#My hypothesis was proven correct by the model summary. The “Pace * Racecraft” interaction term has a p-value of ~0.021, which is significant at this level. All of the other IVs remained significant as well. 
#The residual standard error has decreased slightly from model8, from 0.01086 to 0.0138 (df=49). 
#The adjusted R-squared value has also increased from 0.9838 to 0.9852. 
#The F-statistic has been reduced slightly from 494.6 to 474.3 (df1=8, df2=49), but the p-value is still practically zero at 2.2*10^-16. 
#Thus, I will keep this interaction term in the model moving forward.

#Creating a new model with an interaction term between "Initial Rating" and "Wins that Season."
model11 = lm(transformed.rating ~ pace + initial.rating + awareness + racecraft + experience + new.contract.cost + new.instagram.followers + pace*racecraft + initial.rating*wins.that.season, data=f1) 
summary(model11)
#I presumed that the combination of a driver's initial rating and their number of wins that season would be significant because the win metric would help affirm / disprove whether or not a driver's rating at the beginning of the season was "correct."
#I presumed that the combination of a driver's initial rating and their number of wins that season would be significant because the win metric would help affirm / disprove whether or not a driver's rating at the beginning of the season was "correct” or accurate.
#However, the addition of this interaction term reduces the p-value associated with the “Wins that Season” variable to 0.97. The p-value associated with the interaction term itself is ~0.996. Both of these are non-significant, likely due to the high multicollinearity between them.
#Thus, I will not be keeping this interaction term moving forward.

#Creating a new model with a second-order “Pace” term.
model12 = lm(transformed.rating ~ pace + I(pace^2) + initial.rating + awareness + racecraft + experience + new.contract.cost + new.instagram.followers + pace*racecraft, data=f1) 
summary(model12)
#Knowing that pace is the most significant predictive factor of a driver's rating (based on a 1 IV model, as well as the fact that it is the first included variable in the models produced by stepwise regression), I decided to add a squared term for this variable to the model.
#The estimated coefficient for the squared “Pace” variable term is determined to be significant, with a p-value of ~0.046. However, the p-values of the estimated coefficients for the “Racecraft” and “Pace * Racecraft” interaction terms have become insignificant. This is, again, likely due to multicollinearity. 
#It should be noted that multicollinearity is said to be unavoidable when introducing higher-order terms (as xi and xi^2 will always "know" information about each other, since they are related values).
cor(f1$pace, f1$pace^2) #Calculating the correlation coefficient (r-value) between "Pace" and "Pace^2".
#The correlation coefficient between these two variables is ~0.998, which means that there is an extreme level of multicollinearity between them.

#In an attempt to combat this multicollinearity, I will code the "Pace" variable. 
#Coding quantitative independent variables helps reduce multicollinearity by reducing rounding errors in parameter estimates and standard errors, which inflate the p-values (for the individual t-tests related to each parameter coefficient).
f1$coded.pace = (f1$pace - mean(f1$pace))/(sd(f1$pace)) #Coding the "Pace" independent variable, based on the formula: u=(x-xbar)/(s)
#Creating a new model, but with the “Pace” terms in their coded forms.
model13 = lm(transformed.rating ~ coded.pace + I(coded.pace^2) + initial.rating + awareness + racecraft + experience + new.contract.cost + new.instagram.followers + coded.pace*racecraft, data=f1) 
summary(model13)

#However, standardizing / coding the “Pace” variable does not appear to fix the issues that the quadratic “Pace” term introduces to the model. The first-order “Pace” term has a nonsignificant p-value of 0.8612, though the quadratic term is deemed significant at the ɑ=0.05 level, with a p-value of 0.0459. The “Pace (Coded) * Racecraft” interaction term also has a high p-value at 0.2284. 
#Minimal, if any, improvements are made to the residual standard error and adjusted R-squared value. 
#Thus, I will not be including a second-order term for the “Pace” variable in the final prediction equation either. Truthfully, I do not believe that including any higher-order terms will be necessary (or practical) for this prediction equation.

#-----

#Creating Prediction Intervals:
#Due to the transformation of our dependent variable from the form y to ln(y), we can not produce confidence intervals, as the transformation back into the proper scale would cause the confidence interval to lose its meaning. This is not the case for prediction intervals, in which the antilog can be taken without issue. 
new_data = data.frame("pace"=90, "initial.rating"=90, "awareness"=77, "racecraft"=94, "experience"=67, "new.contract.cost"=6, "new.instagram.followers"=5.9, "pace*racecraft"=8460) #Creating a new data-frame containing Lando Norris’ 2022 ratings / statistics. His actual rating (by the end of the season) was an 89. 
predict(model10, newdata=new_data, interval="prediction", level=0.95) #Creating a prediction interval based on model10 and the “new_data” data frame. There is a 95% likelihood that any drivers with these same statistics will earn a rating in this range in the future.

#The 95% prediction interval for the driver rating (in the form ln(y)) based on these statistics is (4.449, 4.493). The point estimate / center of the prediction interval is 4.471.
#To un-do this ln(y) transformation, I would calculate e to the power of each component in the prediction interval.
fit = exp(4.470869) #The point estimate / center of this prediction interval for driver ratings is ~87.43.
lwr = exp(4.449111) #The lower bound for the interval is ~85.55.
upr = exp(4.492627) #The upper bound for the interval is ~89.36.
#Norris’ actual rating of 89 is contained within this 95% prediction interval, which indicates that it works reasonably well. 
sort(abs(resid(model10))) #Sorting the absolute value of the residuals (of actual driver ratings versus their predicted values based on this model) in ascending order.
#Norris’ observation, which is observation number six, has one of the highest residuals (in terms of its magnitude) in the entire dataset. Thus, since his actual driver rating is contained within the 95% prediction interval, it is safe to assume that the same applies for most other data points.

new_data2 = data.frame("pace"=97, "initial.rating"=94, "awareness"=82, "racecraft"=98, "experience"=74, "new.contract.cost"=10.5, "new.instagram.followers"=9.5, "pace*racecraft"=9506) #Creating a new data-frame containing Max Verstappen’s 2022 ratings / statistics. His actual rating (by the end of the season) was a 95. Verstappen's observation for this year had the highest residual out of all points in the dataset. 
predict(model10, newdata=new_data2, interval="prediction", level=0.95) #Creating a prediction interval based on model10 and the “new_data2” data frame. There is a 95% likelihood that any drivers with these same statistics will earn a rating in this range in the future.

fit2=exp(4.529687) #The center of this prediction interval for driver ratings is ~92.73.
lwr2=exp(4.507247) #The lower bound for the interval is ~90.67.
upr2=exp(4.552128) #The upper bound for the interval is ~94.83.
#Verstappen's actual driver rating of 95 is narrowly not included in this 95% prediction interval. However, since Verstappen has the highest residual out of any observation in the dataset, this should not be too much of a cause for concern.

#-----

#Conclusion:

#Main Question: What are the most significant factors contributing towards the driver ratings in the official Formula One video game?
#The “best” model was discovered to be model10 = lm(transformed.rating ~ pace + initial.rating + awareness + racecraft + experience + new.contract.cost + new.instagram.followers + pace*racecraft, data=f1). 
#Thus, the most significant variables contributing towards the driver ratings in the official Formula 1 video game were “Pace”, “Initial Rating”, “Awareness”, “Racecraft”, “Experience”, “Contract Cost (in Millions)”, and “Instagram Followers (in Millions”). 
#The adjusted R-squared value for this model was 0.9852, while the residual standard error (s-value) was 0.0138 (df=49). #The F-statistic was 474.3 (df1=8, df2=49) with a p-value at 2.2*10^-16, which is practically zero. Thus, the model demonstrated high utility (over a trivial model), strong predictive power, and relatively small residuals (on average). 
#”Pace” was discovered to be the single most important contributing factor in any predictor model, regardless of the number of independent variables included. 
#”Pace”, “Awareness”, “Racecraft”, and “Experience” are four factors that EA Sports themselves claim are utilized in the calculation of these in-game driver ratings. My final model also demonstrates that a driver’s initial rating, contract cost, and instagram follower count were factors with predictive power.

#Side Questions: 
#Which is the most significant interaction / combination of factors towards the driver ratings?
#The sole interaction term with significant predictive influence was discovered to be that between the “Pace” and “Racecraft” variables. This makes practical sense, as perhaps the most essential combination of skills for a driver’s on-track performance is that of speed and over-taking ability (on other cars). 

#Does a large social media following lead to inflated or deflated driver ratings, if there is any effect at all? 
#The estimated β value for the “Instagram Followers” variable—the only social media-related variable with significant predictive ability—was found to be 9.689*10^-4 (β7). This is a tiny value, but given that the standard error of the term is also quite small—at 3.670*10^-4, or nearly three times smaller—it is safe to assume that the actual coefficient value is in fact positive.
#Thus, the number of Instagram followers that a driver has was discovered to be positively correlated with their in-game rating, meaning that a large social media following often co-occurs with an inflated driver rating (according to what would otherwise be predicted by the model).
#A causal relationship can not be determined based on the observational nature of the data and the influence of other variables. However, the prospect that drivers with greater social media popularity might be enjoying a boost to their in-game ratings is certainly intriguing. 