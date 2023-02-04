#install packages
install.packages("tidyverse")
install.packages("dplyr")
install.packages("lmtest")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("car")
install.packages("plotly")
install.packages("plyr")

#load libraries
library(tidyverse)
library(dplyr)
library(lmtest)
library(ggplot2)
library(ggpubr)
library(car)
library(plotly)
library(plyr)

#set working director
setwd("C:\\Users\\swati\\OneDrive - Rutgers University\\2 Spring 2022\\App Multivar Methods\\Final Project")

#LOAD DATA
budget_ppcost_df <- read.csv("CSG1.csv")
class_salaries_df <- read.csv("CSG3.csv")
class_supplies_df <- read.csv("CSG4.csv")
class_services_df <- read.csv("CSG5.csv")
tot_support_df <- read.csv("CSG6.csv")
ops_maintain_df <- read.csv("CSG10.csv")
extracurricular_df <- read.csv("CSG13.csv")
tot_equipment_df <- read.csv("CSG15.csv")
StuTeachRatio_df <- read.csv("CSG16.csv")
StuSpecRatio_df <- read.csv("CSG17.csv")
enrollment_df <- read.csv("EnrollmentTrendsByStudentGroup.csv")
teachers_exp_df <- read.csv("TeachersExperience.csv")
gradratetrends_df <- read.csv("GraduatonRateTrendsProgress.csv")
dropoutrate_df <- read.csv("DropoutRateTrends.csv")
math_scores <- read.csv("MATHperformance16-17.csv")
ela_score <- read.csv("ELAperformance16-17.csv")

#remove scientific notation
options(scipen=999)

#only keep k12 districts
test <- budget_ppcost_df[!(budget_ppcost_df$GROUP=="A. K-6" | budget_ppcost_df$GROUP=="B. K-8 / 0 - 400" | budget_ppcost_df$GROUP=="C. K-8 / 401 - 750" | budget_ppcost_df$GROUP=="D. K-8 / 751 +" | budget_ppcost_df$GROUP=="H. 7-12 / 9-12" | budget_ppcost_df$GROUP=="I. CSSD" | budget_ppcost_df$GROUP=="J. VOC" | budget_ppcost_df$GROUP=="K. Charter" | budget_ppcost_df$CONAME=="Statewide"),]
budget_ppcost <- subset(test, select = -c(PP11, RK11, RK21, PP31, RK31, E11, E31, GROUP))
budget_ppcost$DIST <- as.numeric(budget_ppcost$DIST)

test1 <- class_salaries_df[!(class_salaries_df$GROUP=="A. K-6" | class_salaries_df$GROUP=="B. K-8/0-400" | class_salaries_df$GROUP=="C. K-8/401-750" | class_salaries_df$GROUP=="D. K-8/751+" | class_salaries_df$GROUP=="H. 7-12/9-12" | class_salaries_df$GROUP=="I. CSSD" | class_salaries_df$GROUP=="J. Voc" | class_salaries_df$GROUP=="K. Charter" | class_salaries_df$CONAME=="Statewide"),]
class_salaries <- subset(test1, select = -c(PP13, RK13, PCT13, SBA3, RK23, PP33, RK33, PCT33, SBC3, GROUP))
class_salaries$DIST <- as.numeric(class_salaries$DIST)

test2 <- class_services_df[-c(1:291, 524:705),]
test2 <- test2[!(test2$CONAME=="Statewide"),]
class_services <- subset(test2, select = -c(PP15,RK15,PCT15,RK25,PP35,RK35,PCT35,GROUP))
class_services$DIST <- as.numeric(class_services$DIST)

test3 <- class_supplies_df[-c(1:291, 524:705),]
test3 <- test3[!(test3$CONAME=="Statewide"),]
class_supplies <- subset(test3, select = -c(PP14,RK14,PCT14,RK24,PP34,RK34,PCT34,GROUP))
class_supplies$DIST <- as.numeric(class_supplies$DIST)

test4 <- tot_support_df[-c(1:291, 524:705),]
test4 <- test4[!(test4$CONAME=="Statewide"),]
tot_support <- subset(test4, select = -c(PP16,RK16,PCT16,RK26,PP36,RK36,PCT36,GROUP))
tot_support$DIST <- as.numeric(tot_support$DIST)

test5 <- ops_maintain_df[-c(1:291, 524:705),]
test5 <- test5[!(test5$CONAME=="Statewide"),]
ops_maintain <- subset(test5, select = -c(PP110,RK110,PCT110,RK210,PP310,RK310,PCT310,GROUP))
ops_maintain$DIST <- as.numeric(ops_maintain$DIST)

test6 <- extracurricular_df[-c(1:291, 524:705),]
test6 <- test6[!(test6$CONAME=="Statewide"),]
extracurricular <- subset(test6, select = -c(PP113,RK113,PCT113,RK213,PP313,RK313,PCT313,GROUP))
extracurricular$DIST <- as.numeric(extracurricular$DIST)

test7 <- tot_equipment_df[-c(1:291, 524:705),]
test7 <- test7[!(test7$CONAME=="Statewide"),]
tot_equipment <- subset(test7, select = -c(PP115,PP315,GROUP))
tot_equipment$DIST <- as.numeric(tot_equipment$DIST)

test8 <- StuTeachRatio_df[-c(1:291, 524:705),]
test8 <- test8[!(test8$CONAME=="Statewide"),]
StuTeachRatio <- subset(test8, select = -c(RK0016,RKSAL0016,STRAT0116,RK0116,SALT0116,RKSAL0116,GROUP))
StuTeachRatio$DIST <- as.numeric(StuTeachRatio$DIST)

test9 <- StuSpecRatio_df[-c(1:291, 524:705),]
test9 <- test9[!(test9$CONAME=="Statewide"),]
StuSpecRatio <- subset(test9, select = -c(RK0017,RKSAL0017,SSRAT0117,RK0117,SALS0117,RKSAL0117,GROUP))
StuSpecRatio$DIST <- as.numeric(StuSpecRatio$DIST)

enrollment_data <- enrollment_df[,-c(1:2, 4:6, 10:13)]
enrollment_data$DistrictCode <- as.numeric(enrollment_data$DistrictCode)

teachers_exp <- teachers_exp_df[,-c(1:2, 4:6, 8:12)]
teachers_exp$DistrictCode <- as.numeric(teachers_exp$DistrictCode)

test12 <- gradratetrends_df[,-c(1:2, 4, 8:10)]
test12 <- test12[!(test12$CohortYear == 2016),]
test12 <- test12[!(test12$CohortYear == 2017),]
test12 <- test12[!(test12$CohortYear == 2018),]
grad_rate <- test12
grad_rate$DistrictCode <- as.numeric(grad_rate$DistrictCode)

dropout_rate <- dropoutrate_df[,-c(1:2, 4, 6)]
dropout_rate$DistrictCode <- as.numeric(dropout_rate$DistrictCode)

#isolate and then merge test scores
ELA_grad10 <- ela_score[(ela_score$Grade_Subject == "Grade 10"),]
ELA_grad10$MetExcExpPerc <- as.numeric(ELA_grad10$MetExcExpPerc)
ELA_grad10$DistrictCode <- as.numeric(ELA_grad10$DistrictCode)
ELA_grad10 <- ELA_grad10[complete.cases(ELA_grad10),]
ELA_grad10.m <- ddply(ELA_grad10, .(DistrictCode), summarize, MetExcExpPerc.ELA=mean(MetExcExpPerc))

MATH_grad10 <- math_scores[!(math_scores$Grade_Subject == "Grade 3"),]
MATH_grad10 <- MATH_grad10[!(MATH_grad10$Grade_Subject == "Grade 4"),]
MATH_grad10 <- MATH_grad10[!(MATH_grad10$Grade_Subject == "Grade 5"),]
MATH_grad10 <- MATH_grad10[!(MATH_grad10$Grade_Subject == "Grade 6"),]
MATH_grad10 <- MATH_grad10[!(MATH_grad10$Grade_Subject == "Grade 7"),]
MATH_grad10 <- MATH_grad10[!(MATH_grad10$Grade_Subject == "Grade 8"),]
MATH_grad10 <- MATH_grad10[!(MATH_grad10$Grade_Subject == "Algebra I"),]
MATH_grad10$MetExcExpPerc <- as.numeric(MATH_grad10$MetExcExpPerc)
MATH_grad10 <- MATH_grad10[complete.cases(MATH_grad10),]
MATH_algebraII <- MATH_grad10[(MATH_grad10$Grade_Subject == "Algebra II"),]
MATH_algebraII.m <- ddply(MATH_algebraII, .(DistrictCode), summarize, MetExcExpPerc.AlgII=mean(MetExcExpPerc))
MATH_geometry <- MATH_grad10[(MATH_grad10$Grade_Subject == "Geometry"),]
MATH_geometry.m <- ddply(MATH_geometry, .(DistrictCode), summarize, MetExcExpPerc.Geo=mean(MetExcExpPerc))

math_combo <- merge(MATH_algebraII.m, MATH_geometry.m, by.x = "DistrictCode")
math_combo$MetExcExpPerc.MC <- (math_combo$MetExcExpPerc.AlgII+math_combo$MetExcExpPerc.Geo)/2

testscores_combo <- merge(math_combo, ELA_grad10.m, by.x="DistrictCode")
testscores_combo$MetExcExpPerc.Avg <- (testscores_combo$MetExcExpPerc.MC+testscores_combo$MetExcExpPerc.ELA)/2

#merge Taxpayers' Guide to Education Spending (TGES) dataframes
m <- merge(budget_ppcost, class_salaries, by.x = c("DIST", "CONAME", "DISTNAME"), by.y = c("DIST", "CONAME", "DISTNAME"))
m1 <- merge(m, class_supplies, by.x = c("DIST", "CONAME", "DISTNAME"), by.y = c("DIST", "CONAME", "DISTNAME"))
m2 <- merge(m1, class_services, by.x = c("DIST", "CONAME", "DISTNAME"), by.y = c("DIST", "CONAME", "DISTNAME"))
m3 <- merge(m2, tot_support, by.x = c("DIST", "CONAME", "DISTNAME"), by.y = c("DIST", "CONAME", "DISTNAME"))
m4 <- merge(m3, ops_maintain, by.x = c("DIST", "CONAME", "DISTNAME"), by.y = c("DIST", "CONAME", "DISTNAME"))
m5 <- merge(m4, extracurricular, by.x = c("DIST", "CONAME", "DISTNAME"), by.y = c("DIST", "CONAME", "DISTNAME"))
m6 <- merge(m5, tot_equipment, by.x = c("DIST", "CONAME", "DISTNAME"), by.y = c("DIST", "CONAME", "DISTNAME"))
m6$PP215 <- as.numeric(m6$PP215)
m6$PCT215 <- round(m6$PP215/m6$PP21, digits = 3)
m7 <- merge(m6, StuTeachRatio, by.x = c("DIST", "CONAME", "DISTNAME"), by.y = c("DIST", "CONAME", "DISTNAME"))
tges_df <- merge(m7, StuSpecRatio, by.x = c("DIST", "CONAME", "DISTNAME"), by.y = c("DIST", "CONAME", "DISTNAME"))

#merge New Jersey School Performance Reports (SPR) dataframes
m8 <- merge(enrollment_data, teachers_exp, by.x = "DistrictCode")
m9 <- merge(m8, grad_rate, by.x = "DistrictCode")
m10 <- merge(m9, testscores_combo, by.x = "DistrictCode")
spr_df <- merge(m10, dropout_rate, by.x = "DistrictCode")

#merge both datasets for final dataframe and create csv
data_df <- merge(tges_df, spr_df, by.x = "DIST", by.y = "DistrictCode")

colnames(data_df)[colnames(data_df) == "DIST"] <- "DistCode"
colnames(data_df)[colnames(data_df) == "CONAME"] <- "County"
colnames(data_df)[colnames(data_df) == "DISTNAME"] <- "District"
colnames(data_df)[colnames(data_df) == "PP21"] <- "PerPupTot"
colnames(data_df)[colnames(data_df) == "E21"] <- "EnrollmentTot"
colnames(data_df)[colnames(data_df) == "PP23"] <- "PerPupClassSal"
colnames(data_df)[colnames(data_df) == "PCT23"] <- "PercentTotClassSal"
colnames(data_df)[colnames(data_df) == "PP24"] <- "PerPupClassSup"
colnames(data_df)[colnames(data_df) == "PCT24"] <- "PercentTotClassSup"
colnames(data_df)[colnames(data_df) == "PP25"] <- "PerPupClassServ"
colnames(data_df)[colnames(data_df) == "PCT25"] <- "PercentTotClassServ"
colnames(data_df)[colnames(data_df) == "PP26"] <- "PerPupSupServ"
colnames(data_df)[colnames(data_df) == "PCT26"] <- "PercentTotSupServ"
colnames(data_df)[colnames(data_df) == "PP210"] <- "PerPupOps"
colnames(data_df)[colnames(data_df) == "PCT210"] <- "PercentTotOps"
colnames(data_df)[colnames(data_df) == "PP213"] <- "PerPupExtra"
colnames(data_df)[colnames(data_df) == "PCT213"] <- "PercentTotExtra"
colnames(data_df)[colnames(data_df) == "PP215"] <- "PerPupEquip"
colnames(data_df)[colnames(data_df) == "PCT215"] <- "PercentTotEquip"
colnames(data_df)[colnames(data_df) == "STRAT0016"] <- "StuTeachRat"
colnames(data_df)[colnames(data_df) == "SALT0016"] <- "MedTeachSal"
colnames(data_df)[colnames(data_df) == "SSRAT0017"] <- "StuSupPerRat"
colnames(data_df)[colnames(data_df) == "SALS0017"] <- "MedSupPerSal"
colnames(data_df)[colnames(data_df) == "Economically.Disadvantaged.Students"] <- "PercentFRL"
colnames(data_df)[colnames(data_df) == "Students.with.Disabilities"] <- "PercentDisabled"
colnames(data_df)[colnames(data_df) == "English.Learners"] <- "PercentELL"
colnames(data_df)[colnames(data_df) == "TeacherAvgYearsExp_District"] <- "TeachAvgYearsExp"
colnames(data_df)[colnames(data_df) == "Dropout_District"] <- "DropoutRate"

final_df <- subset(data_df, select = -c(SBB3, CohortYear, GraduationRateType, PerPupClassSal, PerPupClassSup,
                                        PerPupClassServ, PerPupSupServ, PerPupOps, PerPupExtra, PerPupEquip))

summary(final_df)

final_df$PercentTotClassSup <- as.numeric(final_df$PercentTotClassSup)
final_df$PercentTotClassServ <- as.numeric(final_df$PercentTotClassServ)
final_df$PercentTotSupServ <- as.numeric(final_df$PercentTotSupServ)
final_df$PercentTotOps <- as.numeric(final_df$PercentTotOps)
final_df$PercentTotExtra <- as.numeric(final_df$PercentTotExtra)
final_df$StuTeachRat <- as.numeric(final_df$StuTeachRat)
final_df$MedTeachSal <- as.numeric(final_df$MedTeachSal)
final_df$StuSupPerRat <- as.numeric(final_df$StuSupPerRat)
final_df$MedSupPerSal <- as.numeric(final_df$MedSupPerSal)
final_df$PercentFRL <- as.numeric(final_df$PercentFRL)
final_df$PercentDisabled <- as.numeric(final_df$PercentDisabled)
final_df$PercentELL <- as.numeric(final_df$PercentELL)
final_df$TeachAvgYearsExp <- as.numeric(final_df$TeachAvgYearsExp)
final_df$GraduationRate <- as.numeric(final_df$GraduationRate)
final_df$DropoutRate <- as.numeric(final_df$DropoutRate)

final_noNA <- final_df[complete.cases(final_df),]

write.csv(final_noNA, "C:\\Users\\swati\\OneDrive - Rutgers University\\2 Spring 2022\\App Multivar Methods\\Final Project\\finalprojectdata.csv", row.names = FALSE)

df <- read.csv("finalprojectdata.csv")

df$RankScore <- ((df$MetExcExpPerc.Avg*2/3)+(df$GraduationRate/3))
df$Rank <- rank(df$RankScore)

#check for linear regression assumptions
#1 variables are submitted by districts, shouldn't contain errors
hist(df$MetExcExpPerc.AlgII) #right skewed
hist(df$MetExcExpPerc.Geo) #right skewed
hist(df$MetExcExpPerc.MC) #very slight right skew
hist(df$MetExcExpPerc.ELA) #normal distribution
hist(df$MetExcExpPerc.Avg) #relatively normal
hist(df$GraduationRate) #left skewed, seems to have lots of outliers
hist(df$RankScore, main = "School Performance") #normal, use this as dependent

boxplot(df$MetExcExpPerc.AlgII) #a few top performing outliers
boxplot(df$MetExcExpPerc.Geo) #no outliers
boxplot(df$MetExcExpPerc.MC) #no outliers
boxplot(df$MetExcExpPerc.ELA) #no outliers
boxplot(df$MetExcExpPerc.Avg) #no outliers
boxplot(df$GraduationRate) #seems to have lots of outliers
boxplot(df$RankScore) #no outliers, use this as dependent

#2 model is correctly specified:
rank_all <- lm(df$RankScore~df$PerPupTot+df$PercentTotClassSal+df$PercentTotClassSup+
                 df$PercentTotClassServ+df$PercentTotSupServ+df$PercentTotOps+df$PercentTotExtra+
                 df$PercentTotEquip+df$EnrollmentTot+df$PercentFRL+df$PercentDisabled+df$PercentELL+
                 df$StuTeachRat+df$MedTeachSal+df$StuSupPerRat+df$MedSupPerSal+df$TeachAvgYearsExp)
summary(rank_all) #adjusted r2 = 0.6236
bptest(rank_all)

rank_part <- lm(df$RankScore~df$PercentTotClassSal+df$PercentTotClassSup+df$PercentTotClassServ+
                  df$PercentTotSupServ+df$PercentTotOps+df$PercentTotExtra+df$PercentTotEquip+
                  df$EnrollmentTot+df$PercentFRL+df$PercentDisabled+df$PercentELL+df$StuSupPerRat+
                  df$MedSupPerSal)
summary(rank_part) #adjusted r2 = 0.6136
bptest(rank_part)

rank_simple <- lm(df$RankScore~df$PerPupTot+df$StuTeachRat+df$MedTeachSal+df$TeachAvgYearsExp+df$PercentFRL)
summary(rank_simple) #adjusted r2 = 0.5798
bptest(rank_simple)

#3 check for linear relationship: < 0.1 negligible ;0.1-0.2 weak; 0.2-0.4 moderate; > 0.4 strong
plot(df$PerPupTot, df$MetExcExpPerc.Avg)
cor(df$PerPupTot, df$MetExcExpPerc.Avg) #0.169 weak postive relationship

plot(df$StuTeachRat, df$MetExcExpPerc.Avg)
cor(df$StuTeachRat, df$MetExcExpPerc.Avg) #0.035 no relationship

plot(df$TeachAvgYearsExp, df$MetExcExpPerc.Avg)
cor(df$TeachAvgYearsExp, df$MetExcExpPerc.Avg) #0.003 no relationship

plot(df$PercentFRL, df$MetExcExpPerc.Avg)
cor(df$PercentFRL, df$MetExcExpPerc.Avg) #-0.656 strong negative relationship

plot(df$GraduationRate, df$MetExcExpPerc.Avg)
cor(df$GraduationRate, df$MetExcExpPerc.Avg) #0.526 strong positive relationship

plot(df$PerPupTot, df$GraduationRate)
cor(df$PerPupTot, df$GraduationRate) #0.072 no relationship

plot(df$StuTeachRat, df$GraduationRate)
cor(df$StuTeachRat, df$GraduationRate) #-0.124 weak negative relationship

plot(df$TeachAvgYearsExp, df$GraduationRate)
cor(df$TeachAvgYearsExp, df$GraduationRate) #-0.087 no relationship

plot(df$PercentFRL, df$GraduationRate)
cor(df$PercentFRL, df$GraduationRate) #-0.785 strong negative relationship

plot(df$PerPupTot, df$RankScore)
cor(df$PerPupTot, df$RankScore) #0.165 weak positive relationship

plot(df$StuTeachRat, df$RankScore)
cor(df$StuTeachRat, df$RankScore) #0.014 no relationship

plot(df$TeachAvgYearsExp, df$RankScore)
cor(df$TeachAvgYearsExp, df$RankScore) #-0.01 no relationship

plot(df$PercentFRL, df$RankScore)
cor(df$PercentFRL, df$RankScore) #-0.716 strong negative relationship

#4 zero mean
#5 normality of error term - 176 observations no need to check because of CLT
df$resids <- residuals(rank_all)
hist(df$resids) #dependent = rank seems to be more normally distributed

#6 errors are homoscedastic 
df$predvals <- fitted(rank_all)
plot(df$predvals, df$resids)

bptest(rank_all)

#7 no autocorrelation
plot(df$PerPupTot, df$resids)
cor(df$PerPupTot, df$resids)

plot(df$PercentTotClassSal, df$resids)
cor(df$PercentTotClassSal, df$resids)

plot(df$PercentTotClassSup, df$resids)
cor(df$PercentTotClassSup, df$resids)

plot(df$PercentTotClassServ, df$resids)
cor(df$PercentTotClassServ, df$resids)

plot(df$PercentTotSupServ, df$resids)
cor(df$PercentTotSupServ, df$resids)

plot(df$PercentTotOps, df$resids)
cor(df$PercentTotOps, df$resids)

plot(df$PercentTotExtra, df$resids)
cor(df$PercentTotExtra, df$resids)

plot(df$PercentTotEquip, df$resids)
cor(df$PercentTotEquip, df$resids)

plot(df$EnrollmentTot, df$resids)
cor(df$EnrollmentTot, df$resids)

plot(df$PercentFRL, df$resids)
cor(df$PercentFRL, df$resids)

plot(df$PercentDisabled, df$resids)
cor(df$PercentDisabled, df$resids)

plot(df$PercentELL, df$resids)
cor(df$PercentELL, df$resids)

plot(df$StuTeachRat, df$resids)
cor(df$StuTeachRat, df$resids)

plot(df$MedTeachSal, df$resids)
cor(df$MedTeachSal, df$resids)

plot(df$StuSupPerRat, df$resids)
cor(df$StuSupPerRat, df$resids)

plot(df$MedSupPerSal, df$resids)
cor(df$MedSupPerSal, df$resids)

plot(df$TeachAvgYearsExp, df$resids)
cor(df$TeachAvgYearsExp, df$resids)

#8 no multicollinearity
var_only <- df[,c(4:29)]
cor(var_only)
#gradrate:frl = -0.782; gradrate:dropout = -0.825; math:ela = 0.874; math|ela:avg > 0.96

#goodness of fit test
anova(rank_simple, rank_part, test='F') #partial is better
anova(rank_part, rank_all, test='F') #all only slightly better

summary(rank_all)

#create graphs of perpuptot against graduation rate, change size for poverty
for (i in 1:176){
  if(df$PercentFRL[i] < 12.20){
    df$factorFRL[i] = "very low"
  } else if(df$PercentFRL[i] < 32.78) {
    df$factorFRL[i] = "low"
  } else if(df$PercentFRL[i] < 52.80) {
    df$factorFRL[i] = "moderate"
  } else if(df$PercentFRL[i] < 75) {
    df$factorFRL[i] = "high"
  } else {
    df$factorFRL[i] = "very high"
  }
}

plot1 <- df %>%
  ggplot(aes(StuTeachRat,Rank, color=factorFRL,
             text = paste("School:", District,
                          "<br>%FRL:", PercentFRL))) +
  geom_point(alpha=0.75, size=2) +
  scale_color_manual(values = c("very low" = "light blue",
                                "low" = "purple",
                                "moderate" = "green",
                                "high" = "orange",
                                "very high" = "red")) +
  labs(y="School Rank", x="Student Teacher Ratio")
ggplotly(plot1, tooltip = "text")

plot2 <- df %>%
  ggplot(aes(PerPupTot,Rank, color=factorFRL,
             text = paste("School:", District,
                          "<br>%FRL:", PercentFRL))) +
  geom_point(alpha=0.75, size=2) +
  scale_color_manual(values = c("very low" = "light blue",
                                "low" = "purple",
                                "moderate" = "green",
                                "high" = "orange",
                                "very high" = "red")) +
  labs(y="School Rank", x="Per Pupil Spending")
ggplotly(plot2, tooltip = "text")

plot3 <- df %>%
  ggplot(aes(MedTeachSal,Rank, color=factorFRL,
             text = paste("School:", District,
                          "<br>%FRL:", PercentFRL))) +
  geom_point(alpha=0.75, size=2) +
  scale_color_manual(values = c("very low" = "light blue",
                                "low" = "purple",
                                "moderate" = "green",
                                "high" = "orange",
                                "very high" = "red")) +
  labs(y="School Rank", x="Median Teacher Salary")
ggplotly(plot3, tooltip = "text")

plot4 <- df %>%
  ggplot(aes(TeachAvgYearsExp,Rank, color=factorFRL,
             text = paste("School:", District,
                          "<br>%FRL:", PercentFRL))) +
  geom_point(alpha=0.75, size=2) +
  scale_color_manual(values = c("very low" = "light blue",
                                "low" = "purple",
                                "moderate" = "green",
                                "high" = "orange",
                                "very high" = "red")) +
  labs(y="School Rank", x="Average Years of Teacher Experience")
ggplotly(plot4, tooltip = "text")

plot5 <- df %>%
  ggplot(aes(GraduationRate, PercentFRL, color=factorFRL,
             text = paste("School:", District,
                          "<br>%FRL:", PercentFRL))) +
  geom_point(alpha=0.75, size=2) +
  scale_color_manual(values = c("very low" = "light blue",
                                "low" = "purple",
                                "moderate" = "green",
                                "high" = "orange",
                                "very high" = "red")) +
  labs(x="Graduation Rate", y="% in Poverty")
ggplotly(plot5, tooltip = "text")

plot6 <- df %>%
  ggplot(aes(MetExcExpPerc.Avg, PercentFRL, color=factorFRL,
             text = paste("School:", District,
                          "<br>%FRL:", PercentFRL))) +
  geom_point(alpha=0.75, size=2) +
  scale_color_manual(values = c("very low" = "light blue",
                                "low" = "purple",
                                "moderate" = "green",
                                "high" = "orange",
                                "very high" = "red")) +
  labs(x="% Proficient on State Exams", y="% in Poverty")
ggplotly(plot6, tooltip = "text")

outlier.f <- function(x){
  low=as.numeric(quantile(x)[2] - IQR(x)*1.5)
  high=as.numeric(IQR(x)*1.5 + quantile(x)[4])
  list(lower.limit=low, upper.limit=high,
       lower=which(x<low), upper=which(x>high))
}

outlier.f(df$PerPupTot)
outlier.f(df$GraduationRate)


#some graphs for presentation
summary(df$PerPupTot)
boxplot(df$PerPupTot, main="Per Pupil Spending")

summary(df$MetExcExpPerc.Avg)
boxplot(df$MetExcExpPerc.Avg, main="% Proficient in Math and ELA")
summary(df$MetExcExpPerc.ELA)
boxplot(df$MetExcExpPerc.ELA, main="% Proficient in ELA")
summary(df$MetExcExpPerc.AlgII)
boxplot(df$MetExcExpPerc.AlgII, main="% Proficient in Algebra II")
summary(df$MetExcExpPerc.Geo)
boxplot(df$MetExcExpPerc.Geo, main="% Proficient in Geometry")

summary(df$GraduationRate)
boxplot(df$GraduationRate, main="Graduation Rate")