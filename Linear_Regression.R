library(tidyverse)
setwd("C:/Users/harsh/OneDrive/Documents/Healthcare/Lab3/")
file_path <- "C:\\Users\\harsh\\OneDrive\\Documents\\Healthcare\\Lab3\\Healthcare-Diabetes.csv"
data <- read_csv(file_path)

glimpse(data)

# Clean data
df_clean <- data %>%
  filter_all(all_vars(!is.na(.)))

missing_values <- is.na(df_clean)

missing_sum <- colSums(missing_values)

print(missing_sum)



# Task 1
# Dependent variable: Skin Thickness
# Independent Variable: Age

cor(df_clean$Age,df_clean$SkinThickness)

cor.test(df_clean$Age,df_clean$SkinThickness)

# A hypothesis test was performed to verify the existence of a correlation between Age and 
# Skin Thickness. The results of the test were the following: There is a negative, Weak
# correlation between Age and Skin Thickness (r=-0.11), and it is statistically
# significant (p<=0.05).

# Task 2

sp <- ggplot(data = df_clean) + geom_point(mapping = aes(x=df_clean$Age,y=df_clean$SkinThickness, color=(Age>50)))
sp <- sp + scale_color_manual("Age",values = c("#7B248F","#C89200"), labels = c("<50",">=50"))
sp <- sp + ggtitle("Relation between Age and Skin Thickness")+ylab("Skin Thickess")+xlab('Age') + theme_classic()
sp

# Task 3

# Simple Linear Regression
summary(lm( df_clean$SkinThickness ~ df_clean$Age ))

# Y=25.87 - 0.15X

sp <- ggplot(data = df_clean, aes(x=df_clean$Age,y=df_clean$SkinThickness))+geom_point()+ 
  geom_smooth(formula = y ~ x, method = "lm", se=FALSE, col="red")
sp <- sp + ggtitle("Linear Regression Best Fit-line: Age vs Skin Thickness")+ylab("Skin Thickess")+xlab('Age') + theme_classic()

sp

summary(lm(df_clean$SkinThickness ~ df_clean$Age + df_clean$Glucose 
           + df_clean$Insulin + df_clean$BloodPressure))
