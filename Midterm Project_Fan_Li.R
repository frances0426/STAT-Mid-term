# Research Question 1: Do Intolerance of Uncertainty (IUS), Perceived Stress (PSS), or State-Trait Anxiety predict individual's level of cortisol?

if(!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}


# should change to just the name for reproducability
data1 <- read.csv("/Users/frances/Desktop/PSY1950/1950 Midterm Project/data_cortisol_scale.csv")
# standardize survey scores
scale(data1$IUS_all)
scale(data1$STAI_all)
scale(data1$PSS_all)
# check distribution
boxplot(data1$IUS_all,data1$STAI_all,data1$PSS_all, xlab = "Surveys", ylab = "Standardized Scores", main = "Distribution of Standardized Survey Scores")
axis(1, at = 1:3, labels = c("IUS","STAI","PSS"))
# scatrerplot matrices
pairs(data1[,c(5,89,90,93)], pch = 19)
# multiple regression
reg <- lm(data1$c1 ~ data1$IUS_all + data1$STAI_all + data1$PSS_all)
summary (reg)
print(reg)
## note: not significant, what should we do apart from reporting this result? Alternative explanations/ hypothesis?
# check residual and QQ plots
plot(reg)
# residual plot is not funnel-like, QQ plot appears normal
# elective: apply a sandwich estimator
reg_robust <- coeftest(reg, vcov = vcovHC(reg))

# Research Question 2: Does change of cortisol affect individuals gambling behaviors, namely, Attitude towards Risk and Tolerance of Ambiguity?
data2 <- read.csv("/Users/frances/Desktop/PSY1950/1950 Midterm Project/data_gamble_06-Sep-2019.csv")
# standardize level_risks and level_ambiguity
scale(data2$level_risk)
scale(data2$level_ambi)
# check distribution
boxplot(data2$level_risk, data2$level_ambi, xlab = "Gambling Behaviors", ylab = "Scores", main = "Distribution of Gambling Behaviors")
axis(1, at 1:2, labels = c("Attitude towards Risk","Tolerance of Ambiguity")) ## issues to be solved: adding labels to the x axis. Note to myself: try to make a violin plot for RQ2
# multiple regression
reg2 <- lm(data1$diff_c1_c2 ~ data2$level_risk + data2$level_ambi) ## issues to be solved: addressing the different length of variables
# check residuals and QQ plots
plot(reg2)

