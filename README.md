# HW7

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
load("NHIS_2014.RData")
attach(data_use1)
```

<p style="color:rgb(182,18,27);font-family:corbel">Mónica Martínez-Raga</p>
<p style="color:rgb(182,18,27);font-family:corbel">HW7- Fall 2020</p>
<p style="color:rgb(182,18,27);font-family:corbel">Predicting Healthcare Coverage</p>
<p style="color:rgb(182,18,27);font-family:corbel">Collaborators: Isabela Vieira</p>


Why are Republicans running against socialized healthcare and winning in great numbes in the South of the US? Are registered Reps voting against their interest, more scared of the word "socialized" than "uncovered"? Or are they genuinely better off voting Republican, having private health insurance, spending less on medical bills, or benefiting from conservative conventions like marriage?

In this lab, we will predict what type of American resident is covered by health insurance. We don't have voter data to correlate whether these models can subsequenctly predict a Republican, but just to make it fun, I'll assume people who are covered are less likely to care about socialized healthcare.  

I decided to maintain "data_use1" as defined by Dean Foster because frankly the amaount of varaibles was a bit overwhelming and I trust his judgement.

First, I'm looking at regional distribution in my data set. Most people surveyed live in the South of the US, which coincidentally gives us enough data to work with. 
```{r}
summary(data_use1 $ REGION)
```

The first indication that my assumptions may hold is that most people in the survey and in the South are indeed already covered by insurance. This definitely is an indicator that people may be less interested in learning the benefits of socialized medicine, since they may not face the direct consequence of not having insurance. The story may not be the same for individual service coverage, which varies from illness, medicine, medical facility and quality of health plan.

In summary, 84% of the entire population is covered, whereas the Southern population is covered 85%. For comparison, the Northeast, a more democratic area, is covered 91% (there was also less than half as much people surveyed). But we can assume that because of democratic policies, more people are covered. I for instance am under the NY state plan as a student, therefore we already have access to some form of socialized healthcare albeit through private insurance. I don't know the extent to which states subsidize healthcare in the South but the fact remains that relatively much less people have it.
```{r}
summary(data_use1$NOTCOV[REGION] == 0)
summary(data_use1$NOTCOV[REGION == "South"] == 0)
summary(data_use1$NOTCOV[REGION == "Northeast"] == 0)
```


Now that we have this bit of information, let's refine our dataset.
I am choosing something close to the voter population, therefore most people over 18 and under 100 are eligible (assuming there's a negligible amount of voters over 100). If I were to fix the data, I would also add CITIZENP == 1 to target voters further. But I tried and the code ran too long so I didn't know if I was doing it incorrectly. 


```{r}
use_varb <- (AGE_P >= 18) & (AGE_P <=100)
data_use2 <- subset(data_use1,use_varb)
attach(data_use2)
```

Now we create levels for income brackets, including defining NA. We can assume that wealthier persons have more health insurance (and tend to benefit from conservative policies overall, just saying).

```{r}
data_use2$earn_lastyr <- as.factor(data_use2$ERNYR_P)
levels(data_use2$earn_lastyr) <- c("0","$01-$4999","$5000-$9999","$10000-$14999","$15000-$19999","$20000-$24999","$25000-$34999","$35000-$44999","$45000-$54999","$55000-$64999","$65000-$74999","$75000-84999","$85000-94999","$95000 and over","NA")
data_use2$earn_lastyr[(is.na(data_use2$earn_lastyr)==TRUE)]<-"NA"
```

We run a logit model to account for non-linearity in AGE and other variables.
Estimates with a negative slope indicate a correlation of the category being more prone to coverage, and vice versa. This is because NOTCOV is a binary variable where "0" means covered and "1" means not covered. 


```{r}
model_logit1 <- glm(NOTCOV ~ AGE_P + I(AGE_P^2) + female + AfAm + Asian + RaceOther  
                    + Hispanic + educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv 
                    + married + widowed + divorc_sep + veteran_stat + REGION + region_born,
                    family = binomial, data = data_use1)
summary(model_logit1)
#stargazer(model_logit1, type = "text")
```
As we see above, with age people may become less covered. This makes sense because of retirement, or being kicked out of post-retirement health insurance given higher probability for pre-existing conditions or expensive/chronic illnesses. 

The intercept is using with REGION == "Northeast" as it's missing in the summary. We can see that all regions are less covered, the South relatively less within our filtered sample. 

```{r}
summary(data_use2$NOTCOV[REGION == "South"] == 0)
```
Some other interesting things I see are that of all races, "Hispanic" are prone to be less covered relative to other races in the Northeast. I can assume it is because of the much higher amount of Hispanic people in the West and South. In addition, within maritual status, "married" is the only significant variable. It makes sense because a lot of people receive healthcare through their partner's plans once married. It's not surprising to see that "regionborn" is positive, since we are comparing with those born in the US, which would have more direct access to work or healthcare eligibility than immigrants. Not surprised either by the trend in educational attainment. 

These are somewhat conclusive though because most are binary variables, but lets move on to make predictions with other models. First we set variables as dummies so to change them from factors and filter out NAs.

```{r}
d_region <- data.frame(model.matrix(~ data_use2$REGION))
d_region_born <- data.frame(model.matrix(~ factor(data_use2$region_born)))  # snips any with zero in the subgroup
dat_for_analysis_sub <- data.frame(
  data_use2$NOTCOV,
  data_use2$AGE_P,
  data_use2$female,
  data_use2$AfAm,
  data_use2$Asian,
  data_use2$RaceOther,
  data_use2$Hispanic,
  data_use2$educ_hs,
  data_use2$educ_smcoll,
  data_use2$educ_as,
  data_use2$educ_bach,
  data_use2$educ_adv,
  data_use2$married,
  data_use2$widowed,
  data_use2$divorc_sep,
  d_region[,2:4], # "1" is Northeast (intercept)
  #d_region[,2:4],
  d_region_born[,2:12]) # need [] since model.matrix includes intercept term
names(dat_for_analysis_sub) <- c("NOTCOV",
                                 "Age",
                                 "female",
                                 "AfAm",
                                 "Asian",
                                 "RaceOther",
                                 "Hispanic",
                                 "educ_hs",
                                 "educ_smcoll",
                                 "educ_as",
                                 "educ_bach",
                                 "educ_adv",
                                 "married",
                                 "widowed",
                                 "divorc_sep",
                                 "Region.Midwest",
                                 "Region.South",
                                 "Region.West",
                                 "born.Mex.CentAm.Carib",
                                 "born.S.Am",
                                 "born.Eur",
                                 "born.f.USSR",
                                 "born.Africa",
                                 "born.MidE",
                                 "born.India.subc",
                                 "born.Asia",
                                 "born.SE.Asia",
                                 "born.elsewhere",
                                 "born.unknown")
```


```{r}
install.packages("standardize")
```

We now standardize and split our variables into training and test sets. I'll use 15% training data given the professor's warning that my computer may crash otherwise. I don't want to find out, but I do want to increase the training data to improve accuracy of my tests.

```{r}
require("standardize")
set.seed(654321)
NN <- length(dat_for_analysis_sub$NOTCOV)
# restrict_1 <- as.logical(round(runif(NN,min=0,max=0.6))) # use fraction as training data
restrict_1 <- (runif(NN) < 0.15) # use 10% as training data
summary(restrict_1)
dat_train <- subset(dat_for_analysis_sub, restrict_1)
dat_test <- subset(dat_for_analysis_sub, !restrict_1)
sobj <- standardize(NOTCOV ~ Age + female + AfAm + Asian + RaceOther + Hispanic + 
                      educ_hs + educ_smcoll + educ_as + educ_bach + educ_adv + 
                      married + widowed + divorc_sep + 
                      Region.Midwest + Region.South + Region.West + 
                      born.Mex.CentAm.Carib + born.S.Am + born.Eur + born.f.USSR + 
                      born.Africa + born.MidE + born.India.subc + born.Asia + 
                      born.SE.Asia + born.elsewhere + born.unknown, dat_train, family = binomial)
s_dat_test <- predict(sobj, dat_test)
```

As we see above, our training observation amount to about 15% of the dataset.

With the code below, we are able to observe below each category's portion covered and uncovered.
```{r}
summary(sobj$data)
```

Now we run the OLS and the logit with standardized objects to compare which model predicts best.

The OLS is showing the increase in non-coverage for each additional unit increase of the category. Since most of our variables are binary, a linear model will not serve us very well.
```{r}
# OLS
model_lpm1 <- lm(sobj$formula, data = sobj$data)
summary(model_lpm1)
pred_vals_lpm <- predict(model_lpm1, s_dat_test)
pred_model_lpm1 <- (pred_vals_lpm > 0.5)
table(pred = pred_model_lpm1, true = dat_test$NOTCOV)
```

Instead we try a logit model again, which will show the probabilty of non-coverage per category. This is a better model to visualize our binary variables, as we can use the estimates to predict likehood category by category.

```{r}
# logit 
model_logit1 <- glm(sobj$formula, family = binomial, data = sobj$data)
summary(model_logit1)
pred_vals <- predict(model_logit1, s_dat_test, type = "response")
pred_model_logit1 <- (pred_vals > 0.5)
table(pred = pred_model_logit1, true = dat_test$NOTCOV)
```

Comparing confusion matrices, our OLS model has a 85.07% accuracy, vs. our logit which has a 84.11%. Therefore, even though our logit is more intuitive, the linear model seems to be a better predictor. I don't really know why this is the case, maybe because linear models are too simplistic?


Now we will run a Random Forest model, which to amplify the scope of our training data. This takes a while....

The model below shows a certain number of variables that are more significant in predicting whether a person is not covered. For example, those born in Mexico, Central Am and the Caribbean are far less likely to be covered, and thus the racial group Hispanic. Age, being married, and having a highschool or bachelor's degree are also good indicators. Out of all regions, the South is more prone as well to be less covered.

The second confusion matrix, tells us that the model predicted 84.4% of the values correctly. Therefore it is more accurate than the logit model but not the OLS.
```{r}
install.packages("randomForest")
```

```{r}
require('randomForest')
set.seed(54321)
model_randFor <- randomForest(as.factor(NOTCOV) ~ ., data = sobj$data, importance=TRUE, proximity=TRUE)
print(model_randFor)
```

```{r}
round(importance(model_randFor),2)
varImpPlot(model_randFor)
# look at confusion matrix for this too
pred_model1 <- predict(model_randFor,  s_dat_test)
table(pred = pred_model1, true = dat_test$NOTCOV)
```

Now we run the Support Vector Machine. 
"The goal of an SVM is to take groups of observations and construct boundaries to predict which group future observations belong to based on their measurements." More in link: https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwiP1vW72ortAhXMFlkFHckWAX8QFjABegQIARAC&url=http%3A%2F%2Fuc-r.github.io%2Fsvm&usg=AOvVaw1v_TmEV9X0bVdRoTyUIuFp

Confusion matrix shows a accuracy of 83.3%. Since it took too long to test out the possible parameters, I left them as is. Using cost = 10 and gamma = 0.1.

```{r}
install.packages("e1071")
```


```{r}
require(e1071)
svm.model <- svm(as.factor(NOTCOV) ~ ., data = sobj$data, cost = 10, gamma = 0.1)
svm.pred <- predict(svm.model, s_dat_test)
table(pred = svm.pred, true = dat_test$NOTCOV)
```

Lastly, I'm testing the Elastic Net model using alpha = 1. This model is supposed combine the workabilities of Lasso and Ridge models. Lasso eliminates features to reduce overfitting, and Ridge reduces the impact of features that are not important in predicting Y. 

```{r}
install.packages("glmnet")
```


```{r}
# Elastic Net (try 1)
require(glmnet)
model1_elasticnet <-  glmnet(as.matrix(sobj$data[,-1]),sobj$data$NOTCOV, alpha = 1) 
# default is alpha = 1, lasso
par(mar=c(4.5,4.5,1,4))
plot(model1_elasticnet)
vnat=coef(model1_elasticnet)
vnat=vnat[-1,ncol(vnat)] # remove the intercept, and get the coefficients at the end of the path
axis(4, at=vnat,line=-.5,label=names(sobj$data[,-1]),las=1,tick=FALSE, cex.axis=0.5) 
plot(model1_elasticnet, xvar = "lambda")
plot(model1_elasticnet, xvar = "dev", label = TRUE)
print(model1_elasticnet)
cvmodel1_elasticnet = cv.glmnet(data.matrix(sobj$data[,-1]),data.matrix(sobj$data$NOTCOV)) 
cvmodel1_elasticnet$lambda.min
log(cvmodel1_elasticnet$lambda.min)
coef(cvmodel1_elasticnet, s = "lambda.min")
pred1_elasnet <- predict(model1_elasticnet, newx = data.matrix(s_dat_test), s = cvmodel1_elasticnet$lambda.min)
pred_model1_elasnet <- (pred1_elasnet < mean(pred1_elasnet)) 
table(pred = pred_model1_elasnet, true = dat_test$NOTCOV)
model2_elasticnet <-  glmnet(as.matrix(sobj$data[,-1]),sobj$data$NOTCOV, alpha = 1) 
# or try different alpha values to see if you can improve
```

This time, the model shows an accuracy of 51.4%, which is significantly lower than our other models. I will change my alpha to see if we can improve our true positives.

Alpha needs to be between [0,1]. I first tried alpha = 0.5, and the true positives actually decreased, but by a negligible amount. With no strategy apart from trail and error, I found that 0.1 increases my true positives the most. Still, model accuracy is very low compared to the other models. 


For both tries however, Mexican, Central American and Caribbean born people are important variables in predicting lack of health coverage. Something we've seen in all models. Morever, in this model immigrants hold a higher importance than other groups, opposite to what we saw in the Random Forest model.
```{r}
# Elastic Net (try 2)
require(glmnet)
model1_elasticnet <-  glmnet(as.matrix(sobj$data[,-1]),sobj$data$NOTCOV, alpha = 0.1) 
# default is alpha = 1, lasso
par(mar=c(4.5,4.5,1,4))
plot(model1_elasticnet)
vnat=coef(model1_elasticnet)
vnat=vnat[-1,ncol(vnat)] # remove the intercept, and get the coefficients at the end of the path
axis(4, at=vnat,line=-.5,label=names(sobj$data[,-1]),las=1,tick=FALSE, cex.axis=0.5) 
plot(model1_elasticnet, xvar = "lambda")
plot(model1_elasticnet, xvar = "dev", label = TRUE)
print(model1_elasticnet)
cvmodel1_elasticnet = cv.glmnet(data.matrix(sobj$data[,-1]),data.matrix(sobj$data$NOTCOV)) 
cvmodel1_elasticnet$lambda.min
log(cvmodel1_elasticnet$lambda.min)
coef(cvmodel1_elasticnet, s = "lambda.min")
pred1_elasnet <- predict(model1_elasticnet, newx = data.matrix(s_dat_test), s = cvmodel1_elasticnet$lambda.min)
pred_model1_elasnet <- (pred1_elasnet < mean(pred1_elasnet)) 
table(pred = pred_model1_elasnet, true = dat_test$NOTCOV)
model2_elasticnet <-  glmnet(as.matrix(sobj$data[,-1]),sobj$data$NOTCOV, alpha = 0.1) 
# or try different alpha values to see if you can improve
```

After running these models, the most accurate turned out to be the stadardized OLS model has the highest accuracy. At the same time, certain variables held importance, including demographical ones like Hispanic and age. It's interesting some models showed a level of importance for the Southern region, and most a level of importance for Hispanic persons, which we know are at a higher rate undocumented in this country. As we saw in states like Arizona and Nevada, the naturalization of these persons can have an impact on democratic and healthcare policies in the South, given that those most affected by lack of healthcare may also be less likely to vote for socialized healthcare due to ineligibility. 
