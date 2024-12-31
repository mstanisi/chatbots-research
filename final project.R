install.packages("tidyverse")
install.packages("ggthemes")

library (tidyverse)
library(ggthemes)
library(dplyr)

read.csv('/Users/markos98/Desktop/Data Analysis/AI dataset.csv', header = TRUE)

tools <- read.csv('/Users/markos98/Desktop/Data Analysis/AI dataset.csv', header = TRUE)

summary(tools6)
head(tools6$grade.improved)

mean(tools6$stress)

tools$gpa <- as.integer(tools$gpa)
tools$year <- as.factor(tools$year)
tools$course <- as.factor(tools$course)
tools$outdated <- as.factor(tools$outdated)
tools$id <- as.factor(tools$id)

tools <- mutate(tools, course = recode(course, 
                                    'Theoretical Based;' = 'theoretical', 
                                    'Mathematical Based;' = 'mathematical', 
                                    'Programming Based;' = 'programming', 
                                    'Modeling Based;' = 'modeling'))

tools$mean.quality.of.education = (tools$quality.of.education + tools$dup.quality.of.eduation) / 2
tools$quality.of.education <- NULL
tools$dup.quality.of.eduation <- NULL
names(tools)[names(tools) == 'mean.quality.of.education'] <- 'quality.of.education'

read.csv('/Users/markos98/Desktop/Data Analysis/tools.csv', header = TRUE)
tools <- read.csv('/Users/markos98/Desktop/Data Analysis/tools.csv', header = TRUE)


tools <- mutate(tools, gpa = recode(gpa, 
                                    '3' = 3.5, 
                                    '2' = 3, 
                                    '1' = 2.5))
tools[is.na(tools)] <- 4
mean(tools$gpa)
mean(tools$grade.improved)

tools[13, "course"] <- 'programming'

summary(tools)
write.csv(tools, '/Users/markos98/Desktop/Data Analysis/tools1.csv')
tools <- read.csv('/Users/markos98/Desktop/Data Analysis/tools1.csv', header = TRUE)

hist(tools6$reliance) #skews left, to say more reliance
boxplot(tools6$reliance) #but looks normal here
hist(tools6$copy.paste)#normal
hist(tools6$stress)#skewed left, to say less stress
boxplot(tools6$stress)#skewed left
hist(tools6$info.recall)#skews left
boxplot(tools6$info.recall) #outlier, 5 - FIXED
hist(tools6$chat.length)#looks normal
boxplot(tools6$chat.length)#skewed left, mean of 2
hist(tools6$false.info)#looks normal
boxplot(tools6$false.info)#outlier, 5 - FIXED
hist(tools6$timesaved)#skews right, but normal
boxplot(tools6$timesaved)#right
hist(tools6$grade.improved)#skews left, grades not improved, this is interesting
boxplot(tools6$grade.improved)#skews left


library(stats)
library(car)
library(corrplot)
library("PerformanceAnalytics")
library(GGally)

median.false.info.6 <- median(tools6$false.info)
tools6$false.info.imputed <- tools6$false.info
max.false.info.6 <- max(tools6$false.info)
tools6$false.info.imputed <- replace(tools6$false.info.imputed, 
      tools6$false.info.imputed == max.false.info.6, median.false.info.6)
boxplot(tools6$false.info.imputed)

median.info.recall.6 <- median(tools6$info.recall)
tools6$info.recall.imputed <- tools6$info.recall
max.info.recall.6 <- max(tools6$info.recall)
tools6$info.recall.imputed <- replace(tools6$info.recall.imputed, 
      tools6$info.recall.imputed == max.info.recall.6, median.info.recall.6)
boxplot(tools6$info.recall.imputed)

tools6$info.recall <- tools6$info.recall.imputed
tools6$false.info <- tools6$false.info.imputed

tools_final <- select(tools, -c('X.1','X', 'id', 'info.recall', 'false.info', 'false.info.recall', 'median.info.recall'))
write.csv(tools_final, '/Users/markos98/Desktop/Data Analysis/tools2.csv')
tools3 <- select(tools_final, -c('median.false.info'))
write.csv(tools3, '/Users/markos98/Desktop/Data Analysis/tools3.csv')

glimpse(tools3)
tools.df6 <- select(tools6, reliance, copy.paste, stress, chat.length, timesaved, grade.improved, info.recall, false.info, quality.of.education)
pairs(tools.df6)
#check out corr between low gpa and grade improvement
shapiro.test(tools6$grade.improved)
head(tools3)

corr_plot_pearson_1 <- cor(tools.df6, method = "pearson")
corr_plot_pearson_1
#time.saved and reliance scores .61 SIGNIFICANT
#grade.improved and reliance scores .53 SIGNIFICANT
#grade.improved and time.saved scores .48
#quality.of.education and stress scores .56 SIGNIFICANT
#quality.of.education and reliance scores .56 SIGNIFICANT
#quality.of.education and timesaved scores .48

tools6.df <- select(tools6, grade.improved, timesaved, reliance, stress)
tools6.df.matrix<-cor(tools6.df)
corrplot(tools6.df.matrix, method="number") #main correlations visualized
cor.mtest (tools6.df.matrix)
#the lowest p-value is .3 between stress and time saved

grade.improved.dep <- tools6$grade.improved
grade.improved.ind <- tools6$grade.improved
timesaved.ind <- tools6$timesaved
timesaved.dep <- tools6$timesaved
reliance.ind <- tools6$reliance
reliance.dep <- tools6$reliance
stress.ind <- tools6$stress
stress.dep <- tools6$stress
quality.of.education.dep <- tools6$quality.of.education
quality.of.education.ind <- tools6$quality.of.education

plot(reliance.ind, grade.improved.dep, 
     main = "Effect of Reliance on AI Tools on Grade Improvement",
     xlab = "Reliance",
     ylab = "Grade Improvement")
plot(stress.ind, quality.of.education.dep, 
     main = "Effect of Stress Levels on Perceived Quality of Education",
     xlab = "Stress Level",
     ylab = "Quality of Education")

linear_model_timesaved <- lm(stress.dep ~ timesaved.ind)
summary(linear_model_timesaved)
#p-value of 2e-07 *** and intercept is <2e-16 ***
#This means time saved relieves stress, obviously. 

linear_model_stress2 <- lm(quality.of.education.dep ~ stress.ind)
summary(linear_model_stress2)
#p-value is 4.41e-13 *** and intercept is 1.32e-10 ***

linear_model_reliance <- lm(grade.improved.dep ~ reliance.ind)
summary(linear_model_reliance)
#p-value of <2e-16 *** but intercept is 0.286 

linear_model_stress <- lm(grade.improved.dep ~ stress.ind)
summary(linear_model_stress)
#p-value is .011 and intercept is <2e-16 ***
#This means an alleviation of stress coincides with grade improvement. 
#This is USEFUL

plot(stress.ind, grade.improved.dep,
     main = "Effect of Stress Alleviation on Grade Improvement",
     xlab = "Stress Level",
     ylab = "Grade Improvement")
#Visualization does not look significant, a different graph is needed. 
#Histogram would be better, stress level as categorical
#Same for GPA!

linear_model_gradesandtime <- lm(grade.improved.dep ~ timesaved.ind)
summary(linear_model_gradesandtime)
#Uninteresting trend, though p-value is < 2.2e-16
plot(timesaved.ind, grade.improved.dep,
     main = "Effect of Stress Alleviation Because of AI Tools on Grade Improvement",
     xlab = "Time Saved",
     ylab = "Grade Improvement")
#Looks good though

read.csv('/Users/markos98/Desktop/Data Analysis/tools3.csv', header = TRUE)
tools4 <- read.csv('/Users/markos98/Desktop/Data Analysis/tools3.csv', header = TRUE)

tools4$course
glimpse(tools4)

conditions.tools4.courses <- c("Modeling Based:", "Programming Based")
replacements.tools4.courses <- c("modeling","programming")
tools4$course <- replace(tools3$course, tools3$course %in% conditions.tools4.courses, replacements.tools4.courses)

library(ggplot2);
ggplot(tools4, aes(as.factor(gpa), reliance)) +
  geom_bar(stat = "summary", fun = "mean") + 
  labs(y = "Level of reliance", x = "GPA")
#Reliance is higher the lower the GPA

ggplot(tools4, aes(as.factor(course), grade.improved)) +
  geom_bar(stat = "summary", fun = "mean") + 
  labs(y = "Degree of Grade Improvement", x = "Course")
#They are all pretty much the same

write.csv(tools4, '/Users/markos98/Desktop/Data Analysis/tools4.csv')

read.csv('/Users/markos98/Desktop/Data Analysis/tools6.csv', header = TRUE)
tools6 <- read.csv('/Users/markos98/Desktop/Data Analysis/tools6.csv', header = TRUE)
glimpse(tools6)

quality.mean <- rowMeans(tools6[, c("quality.of.education",	"dup.quality",	
  "quality2",	"dup.quality.2", "quality.of.education3",	"dup.quality3",	
  "quality.of.education4",	"dup.quality4")], na.rm = TRUE)
tools6$quality.of.education <- quality.mean
tools6$quality.of.education

reliance.mean <- rowMeans(tools6[, c("reliance",	"reliance2",	"reliance3",	"reliance4")], na.rm = TRUE)
tools6$reliance <- reliance.mean
tools6$reliance

quality.copy.paste <- rowMeans(tools6[, c("copy.paste",	"copy.paste2",	"copy.paste3",	"copy.paste4")], na.rm = TRUE)
tools6$quality.of.education <- quality.mean
tools6$quality.of.education

stress.mean <- rowMeans(tools6[, c("stress",	"stress2",	"stress3",	"stress4")], na.rm = TRUE)
tools6$stress <- stress.mean
tools6$stress

info.recall.mean <- rowMeans(tools6[, c("info.recall",	"info.recall2",	"info.recall3",	"info.recall4")], na.rm = TRUE)
tools6$info.recall <- info.recall.mean
tools6$info.recall

chat.length.mean <- rowMeans(tools6[, c("chat.length",	"chat.length2",	"chat.length3",	"chat.length4")], na.rm = TRUE)
tools6$chat.length <- chat.length.mean
tools6$chat.length

false.info.mean <- rowMeans(tools6[, c("false.info",	"false.info2",	"false.info3",	"false.info4")], na.rm = TRUE)
tools6$false.info <- false.info.mean
tools6$false.info

false.timesaved <- rowMeans(tools6[, c("timesaved",	"timesaved2",	"timesaved3",	"timesaved4")], na.rm = TRUE)
tools6$timesaved <- false.timesaved
tools6$timesaved

grade.improved <- rowMeans(tools6[, c("grade.improved",	"grade.improved2",	"grade.improved3",	"grade.improved4")], na.rm = TRUE)
tools6$grade.improved <- grade.improved
tools6$grade.improved

library(ggplot2);
ggplot(tools6, aes(as.factor(gpa), reliance)) +
  geom_bar(stat = "summary", fun = "mean") + 
  labs(y = "Level of reliance", x = "GPA")
#Reliance is higher the lower the GPA

plot(stress.ind, grade.improved.dep,
     main = "Effect of Stress Alleviation on Grade Improvement",
     xlab = "Stress Level",
     ylab = "Grade Improvement")

tools6 <- select(tools6, -c("reliance2",	"quality2",	"copy.paste2",	"dup.quality.2",	"stress2",	"info.recall2",	"chat.length2",	"false.info2",	"timesaved2",	"grade.improved2",	"reliance3",	"quality.of.education3",	"copy.paste3",	"dup.quality3",	"stress3",	"info.recall3",	"chat.length3",	"false.info3",	"timesaved3",	"grade.improved3",	"reliance4",	"quality.of.education4",	"copy.paste4",	"dup.quality4",	"stress4",	"info.recall4",	"chat.length4",	"false.info4",	"timesaved4", "grade.improved4"))
write.csv(tools6, '/Users/markos98/Desktop/Data Analysis/tools_final.csv')


###############


linear_model_reliance_grades <- lm(grade.improved.dep ~ reliance.ind)
summary(linear_model_reliance_grades)
#good p-value but bad intercept
linear_model_grades_reliance <- lm(reliance.dep ~ grade.improved.ind)
summary(linear_model_grades_reliance)
#significant, grade improvement is affecting reliance

linear_model_reliance_time <- lm(timesaved.dep ~ reliance.ind)
summary(linear_model_reliance_time)
#significant, reliance is decreasing the time spent on hw
linear_model_reliance_time <- lm(reliance.ind ~ timesaved.dep)
summary(linear_model_reliance_time)
#the reverse is not true, RELIANCE is affecting

linear_model_reliance_quality<- lm(quality.of.education.dep ~ reliance.ind)
summary(linear_model_reliance_quality)
#significant, reliance is effecting
linear_model_quality_reliance<- lm(reliance.dep ~ quality.of.education.ind)
summary(linear_model_quality_reliance)
#interrelated

linear_model_stress_education <- lm(quality.of.education.dep ~ stress.ind)
summary(linear_model_stress_education)
#significant, stress is effecting perceived quality of education
linear_model_education_stress <- lm(stress.dep ~ quality.of.education.ind)
summary(linear_model_education_stress)
#the reverse is also true

linear_model_stress_education <- lm(quality.of.education.dep ~ timesaved.ind)
summary(linear_model_time_education)
#significant, time saved is affecting perceived quality of education
linear_model_timesaved_education <- lm(timesaved.dep ~ quality.of.education.ind)
summary(linear_model_timesaved_education)
#the reverse is also true

linear_model_stress_grades <- lm(grade.improved.dep ~ stress.ind)
summary(linear_model_stress_grades)
#p-value good but intercept is bad
linear_model_grades_stress <- lm(stress.dep ~ grade.improved.ind)
summary(linear_model_grades_stress)
#grade improvement is effecting time



linear_model_stress_reliance <- lm(reliance.dep ~ stress.ind)
summary(linear_model_stress_reliance)
#significant, stress is affecting
linear_model_reliance_stress <- lm(stress.dep ~ reliance.ind)
summary(linear_model_reliance_stress)
#interrelated, reliance is intertwined with stress

