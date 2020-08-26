library(readxl)
library(MatchIt)

data <- read.csv(here::here("dat/08_2020-2021.csv"))

#Make sure there are no duplicates#

data$time <- as.factor(data$time)
data$degree <- as.factor(data$degree)
data$language <- as.factor(data$language)
data$healthcare <- as.factor(data$healthcare)
data$V3 <- as.factor(data$V3)

row.names(data) <- data$V4

match <- matchit(V3 ~ healthcare + time + degree + language, data=data, method="optimal")
match_matrix <- match$match.matrix
write.csv(match_matrix, file="match_matrix.csv")


healthcare <- filter(data, data$healthcare=="Yes")
students <- filter(data, data$healthcare!="Yes")
                   
match_healthcare <- matchit(V3 ~ time + degree + language, data=healthcare, method="optimal")
match_healthcare_matrix <- match_healthcare$match.matrix
write.csv(match_healthcare_matrix, file="match_healthcare_matrix.csv")

match_students <- matchit(V3 ~ time + degree + language, data=students, method="optimal")
match_students_matrix <- match_studentse$match.matrix
write.csv(match_students_matrix, file="match_student_matrix.csv")


support_healthcare <- subset(support, support$Healthcare=="Yes")
buddy_healthcare <- subset(buddy, buddy$Healthcare=="Yes")
support_student <- subset(support, support$Healthcare=="No")
buddy_student <- subset(buddy, buddy$Healthcare=="No")

support_healthcare$match <- 1
buddy_healthcare$match <- 0
support_student$match <- 1
buddy_student$match <- 0

healthcare <- rbind(support_healthcare, buddy_healthcare)
healthcare <- cbind(healthcare$Name, healthcare$Degree, healthcare$Specialization, healthcare$match)
healthcare_name <- healthcare[,1]

student <- rbind(support_student, buddy_student)
student <- cbind(student$Name, student$Degree, student$Specialization, student$match)
student_name <- student[,1]

healthcare <- as.data.frame(healthcare)
student <- as.data.frame(student)
row.names(healthcare) <- healthcare_name
row.names(student) <- student_name

match_healthcare <- matchit(V4 ~ V2 + V3, data=healthcare, method="optimal")
match_healthcare_matrix <- match_healthcare$match.matrix
write.csv(match_healthcare_matrix, file="match_healthcare_matrix.csv")

match_student <- matchit(V4 ~ V2 + V3, data=student, method="optimal")
match_student_matrix <- match_student$match.matrix
write.csv(match_student_matrix, file="match_student_matrix.csv")
