#install.packages("janitor")
#install.packages("GGally")
library(janitor)
library(GGally)
library(dplyr)
df <- read.csv("Thyroid-Dataset.csv")
head(df)
summary(df)

#TO CONVERT ALL TRUE AND FALSE INTO 1 AND 0
df <- as.data.frame(lapply(df, function(col) {
  
  col_lower <- tolower(as.character(col))
  
  
  if (all(col_lower %in% c("true", "false", "t", "f"))) {
    
    
    return(as.numeric(col_lower %in% c("true", "t")))
    
  } else {
    return(col)  
  }
}))

#correct the column names using janitor
df<- clean_names(df)

#To remove Na values
df[df == "?" | df == "" | df == "na" | df == "NA"] <- NA
df <- na.omit(df)
df <- remove_empty(df, "rows")
df <- remove_empty(df, "cols")

#TO filter Age(0-100)
df$age <- as.numeric(df$age)
df <- df[df$age >= 0 & df$age <= 100, ]

write.csv(df, "Thyroid-Cleaned.csv", row.names = FALSE)

#Male vs Female ratio for thyroid 
# Count number of males and females in each thyroid class
sex_ratio <- table(df$sex, df$class)
sex_ratio

prop.table(sex_ratio, 1) * 100
barplot(sex_ratio,
        beside = TRUE,
        col = c("blue", "red"),
        main = "Male vs Female Ratio in Thyroid Conditions",
        xlab = "Thyroid Condition",
        ylab = "Count of Patients",
        legend = rownames(sex_ratio))


#1. Relationship: Age vs Thyroid Disease Risk
df$age_group <- cut(df$age,
                    breaks = c(0,20,40,60,80,100),
                    labels = c("0-20","21-40","41-60","61-80","81-100"))
table_age <- table(df$age_group, df$class)

barplot(t(table_age),
        beside = TRUE,
        main = "Age Group vs Thyroid Conditions",
        col = rainbow(8),
        las = 2)




#2. Relationship: Pregnancy vs Hormone Levels
boxplot(tsh ~ pregnant, data=df,
        main="TSH Levels in Pregnant vs Non-Pregnant",
        xlab="Pregnant", ylab="TSH")



#3. Relationship: TSH vs T3 vs TT4 (Thyroid Triangle)
ggplot(df, aes(x=tsh, y=t3, color=tt4)) +
  geom_point() +
  scale_color_gradient(low="ble", high="red") +
  labs(title="TSH vs T3 with TT4 Intensity")



#4. Relationship: Sex Differences in Thyroid Disorders
prop.table(table(df$sex, df$class), 1)




#5 Relationship: Medication Use vs Hormone Values
# Calculate average TSH for both groups
mean_tsh_no  <- mean(df$tsh[df$on_thyroxine == 0], na.rm = TRUE)
mean_tsh_yes <- mean(df$tsh[df$on_thyroxine == 1], na.rm = TRUE)

# Put into a vector
values <- c(mean_tsh_no, mean_tsh_yes)

# Create the bar plot
barplot(values,
        names.arg = c("Not on Thyroxine", "On Thyroxine"),
        col = c("skyblue", "salmon"),
        main = "Effect of Thyroxine Medication on TSH Levels",
        ylab = "Average TSH Level")




#6. Relationship: Symptom ‘Sick’ Flag vs Thyroid Function
boxplot(fti ~ sick, data=df,
        main="FTI Levels in Sick vs Non-Sick Patients")

#7 Relationship: Referral Source vs Disease Severity
barplot(table(df$referral_source, df$class),
        main="Referral Source vs Thyroid Condition",
        col=rainbow(7), las=2)

#8️. Relationship: TSH Abnormality Threshold Analysis
df$TSH_status <- ifelse(df$tsh > 4, "High",
                        ifelse(df$tsh < 0.4, "Low", "Normal"))

table(df$TSH_status, df$class)



#9. Relationship: Hormone fluctuations across thyroid classes
aggregate(cbind(tsh, t3, tt4, fti) ~ class, df, mean)




#10. Relationship: Thyroid Surgery Influence
# Remove NA values for surgery + TT4
df_clean <- df[!is.na(df$thyroid_surgery) & !is.na(df$tt4), ]

# Convert 0/1 → Factor
df_clean$thyroid_surgery <- factor(df_clean$thyroid_surgery,
                                   levels = c(0,1),
                                   labels = c("No Surgery", "Surgery"))

# Scatter plot using jitter to avoid overlapping
plot(jitter(as.numeric(df_clean$thyroid_surgery)),
     df_clean$tt4,
     xaxt = "n",
     xlab = "Thyroid Surgery Status",
     ylab = "TT4 Hormone Levels",
     main = "Scatter Plot: Effect of Thyroid Surgery on TT4 Levels",
     pch = 16, cex = 0.8)

# Add proper x-axis labels
axis(1, at = c(1,2), labels = c("No Surgery", "Surgery"))

