library(tidyverse)
library(apaTables)

# load population data
population_data <- read_csv("population_data.csv")

glimpse(population_data)


# get sample 1
set.seed(1)
sample1_analytic_data <- sample_n(population_data, size=200)

glimpse(sample1_analytic_data)


# run the regression
sample1_lm_results <- lm(performance ~ IQ, data=sample1_analytic_data)
summary(sample1_lm_results)
# intercept estimate = intercept, IQ estimate = slope


# get the regression in APA style, with more helpful info (ie. b, CIs, beta, r, r2)
apa.reg.table(sample1_lm_results)
# slope is .24, 95% CI[.19, .29], intercept is 50.60, 95% CI[45.40, 55.79]

#run regression for population
population_lm_results <- lm(performance ~ IQ, data=population_data)
summary(population_lm_results)
# slope is .20, intercept is 55 - captured in sample CI!


# predicted value for single person
x_axis_range <- data.frame(IQ=c(120))
CI_data <- predict(sample1_lm_results, 
                   newdata = x_axis_range, interval = "confidence", level=.95)
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))
CI_data
#fit is the Y value on the regression line - best guess of Y


#predicted value for entire x axis range
min_predictor <- min(sample1_analytic_data$IQ)
max_predictor <- max(sample1_analytic_data$IQ)
x_axis_range <- data.frame(IQ=seq(min_predictor, max_predictor, by=.5))
CI_data <- predict(sample1_lm_results, 
                   newdata = x_axis_range, interval = "confidence", level=.95)
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))
# CI_data


#create prediction intervals
PI_data <- predict(sample1_lm_results, 
                   newdata = x_axis_range, interval = "prediction", level=.95)
PI_data <- as.data.frame(cbind(x_axis_range, PI_data))
# PI_data


#graph regression line with CI and PI :)
reg_plot <- ggplot(sample1_analytic_data, aes(x=IQ, y=performance))
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic()
reg_plot <- reg_plot + geom_smooth(data=CI_data, aes(x=IQ, y=fit, ymin=lwr, ymax=upr), stat="identity")
reg_plot <- reg_plot + geom_smooth(data=PI_data, aes(x=IQ, y=fit, ymin=lwr, ymax=upr), stat="identity")
print(reg_plot)


#shortcut for CI line
reg_plot <- ggplot(sample1_analytic_data, aes(x=IQ, y=performance))
reg_plot <- reg_plot + geom_point()
reg_plot <- reg_plot + theme_classic()
reg_plot <- reg_plot + geom_smooth(method="lm", se=TRUE)
reg_plot <- reg_plot + geom_smooth(data=PI_data, aes(x=IQ, y=fit, ymin=lwr, ymax=upr), stat="identity")
print(reg_plot)

