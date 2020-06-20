###############################
#   STA 412 - Group Project   #
#   Mark, Tanu, Pete          #
#                             #
#   Facebook Data Analysis    #
###############################

### Import Data ###

# Set working directory - change to your PCs project location
setwd("C:/GitProjects/Facebook_BuzzFeed_Analysis")

# Read in Dataset - change to your PCs data location
fb_data = read.csv("Data/facebook-fact-check.csv")

# Drop unnessary columns
fb_data = as.data.frame(fb_data[,c(3,4,7,8,10,11,12)])

# Omit NaNs
fb_data = na.omit(fb_data)
head(fb_data)

# Display feature types and classes
sapply(fb_data, mode)
sapply(fb_data, class)

# Fix levels of Rating variable for plots
fb_data$Rating = factor(fb_data$Rating, levels = c("mostly true",
                                                   "mixture of true and false",
                                                   "mostly false",
                                                   "no factual content"))

# Descriptive Statistics

# Histogram for share_count, reaction count, and comment_count

attach(fb_data)
par(mfrow=c(1,2))
hist(share_count)
hist(reaction_count)

# Max, Min, Median values and Means for each rating for share_count

# share_count max
max(share_count)
# share_count min
min(share_count)
# share_count median
median(share_count)
# share_count means for each rating
aggregate(share_count~Rating,fb_data,mean)
# share_count variances for each rating
aggregate(share_count~Rating,fb_data,var)
# share_count standard deviation for each rating
aggregate(share_count~Rating,fb_data,sd)

# Max, Min, Median values and Means for each rating for reaction_count

# reaction_count max
max(reaction_count)
# reaction_count min
min(reaction_count)
# reaction_count median
median(reaction_count)
# reaction_count means for each rating
aggregate(reaction_count~Rating,fb_data,mean)
# reaction_count variances for each rating
aggregate(reaction_count~Rating,fb_data,var)
# reaction_count standard deviation for each rating
aggregate(reaction_count~Rating,fb_data,sd)

## Q-Q Plots for Normality
par(mfrow=c(1,2))
attach(fb_data)
qqnorm(share_count)
qqline(share_count)
qqnorm(reaction_count)
qqline(reaction_count)

# Logarithmic Transformation
fb_data$log_share_count= log(fb_data$share_count) 
fb_data$log_reaction_count= log(fb_data$reaction_count)

# New Q-Q Plots
par(mfrow=c(1,2))
attach(fb_data)
qqnorm(log_share_count)
qqline(log_share_count)
qqnorm(log_reaction_count)
qqline(log_reaction_count)

### Our First Hypothesis

##### Hypothesis 1: Test 1

# One-Way Analysis of Means for share_count

# Test Using Logarithmic Transformation
oneway.test(log_share_count~Rating, fb_data, var.equal = F)

##### Hypothesis 1: Test 2

# One-Way Analysis of Means for reaction_count

# Test Using Logarithmic Transformation
oneway.test(log_reaction_count~Rating, fb_data, var.equal = F)

### Our Second Hypothesis

# Plots for Linear Relationships
par(mfrow=c(1,2))
plot(log_share_count~Rating, data=fb_data)
plot(log_reaction_count~Rating, data=fb_data)

### F Test for Nested Models

# F Test for Nested Models -> share_count

shares.lm = lm(log_share_count ~ Rating, data = fb_data)
shares_full = shares.lm
shares_reduced = lm(log_share_count ~ 1, data = fb_data)
anova(shares_reduced, shares_full)

#F Test for Nested Models -> reaction_count

reactions.lm = lm(log_reaction_count ~ Rating, data = fb_data)
reactions_full = reactions.lm
reactions_reduced = lm(log_reaction_count ~ 1, data = fb_data)

### Linear Regression

# Predict share count based on rating
summary(shares.lm)
contrasts(fb_data$Rating)
plot(fb_data$Rating, fb_data$log_share_count)
abline(shares.lm)

# Predict reaction count based on rating
summary(reactions.lm)
contrasts(fb_data$Rating)
plot(fb_data$Rating, fb_data$log_reaction_count)
abline(reactions.lm)

### Confidence Intervals

#Confidence Intervals for share_count model

# 95% CI for beta0 and beta1 
confint(shares.lm) 

#Confidence Interval for reaction_count model

# 95% CI for beta0 and beta1 
confint(reactions.lm)

### Multiple Regression

shares.multlm = lm(log_share_count ~ Category + Page + Post.Type + Rating, data = fb_data)
summary(shares.multlm)





