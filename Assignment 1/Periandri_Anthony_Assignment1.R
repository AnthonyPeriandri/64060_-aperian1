read.csv("automobile.csv")
df<-automobile
summary(df[,c("wheel.base","length")])
table(df$engine.location)
table(df$fuel.type)
df<-df %>%
  mutate(log_Lengthofcar= log(length))
summary(df[,c("log_Lengthofcar", "length")])
hist(df$length,
     main = "Histogram of Car Length",
     xlab = "length",
     col = "red",
     breaks = 10)
plot(df$wheel.base,df$length,
     main = "Wheel Base VS Length",
     xlab = "Wheel Base",
     ylab = "Length",
     col = "blue",
     pch = 12)
     
