set.seed(42)

#to read the file:
x<-read.csv("Muscular Data_Project_10csv.csv")
x<-as.data.frame(x) #converting into dataframe

# shuffling
rows <- sample(nrow(x))
x<-x[rows,]

#splitting
train_x<-x[1:146,]
test_x<-x[147:209,]

plot(x) #plots graphs
#summarises the data:
summary(x)

#applying logistic regression to determine model:
#backward elimination processs to determine the best model.

fit<- glm(train_x$Carrier~train_x$creatine_Kinase+train_x$Hemopexin+train_x$Pyrovate_Kinase+
            train_x$Age,
          data = x, family = "binomial")

# summarises results
summary(fit)

# generate confidence intervals:
confint(fit) # using log likelihood

# odds ratio 
exp(coef(fit))

#odds ratio and 95% CI
exp(cbind(OR = coef(fit), confint(fit)))

#overall significance of model
p_val<-with(fit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

#log likelihood 
logLik(fit)

#residual plot to check residual band
y<-as.data.frame(fit$residuals)

ggplot(y, aes(x= 1:nrow(y),y= y$`fit$residuals`,colour= "CAB2D6")) + 
        geom_point()+
        ggtitle("Residual Plot")+ 
        labs(x=" ",y=" Residual")

#making predictions:
probabilities <- fit %>% predict(test_x, type = "response")
predicted <- ifelse(probabilities > 0.5,1,0)
mean(predicted == test_x$Carrier,na.rm=T) # checking accuracy

#collinearity
car::vif(fit)

#plots

mydata <- train_x %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

# influential values
plot(fit, which = 4, id.n = 3)

