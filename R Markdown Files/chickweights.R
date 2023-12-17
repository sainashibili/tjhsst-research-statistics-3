#Saina Shibili
#Chick Weights

#Summarizing exercises:

#50 Chickens were involved in this experiment

#avg weight of chicken on day 18
mean(ChickWeight$weight[ChickWeight$Time %in% 18])

#table of chickens on each diet
table(ChickWeight$Diet)

#standard deviation of chicken weights on day 0
sd(ChickWeight$weight[ChickWeight$Time %in% 0])
#standard deviation of chicken weights on day 21
sd(ChickWeight$weight[ChickWeight$Time %in% 21])

#mean weight and standard deviation of all chickens on day 21 by diet
ChickWeight %>%
  dplyr::filter(Time==21) %>%
  dplyr::select(weight, Diet) %>%
  dplyr::group_by(Diet)%>% 
  summarize(mean_weight= mean(weight), std_dev= sd(weight))

#Visualization Exercises:

#dotplot of weight of chicken weights on day 14
wt_day14_diet3 <- ChickWeight$weight[ChickWeight$Time %in% 14 & ChickWeight$Diet %in% 3]
stripchart(wt_day14_diet3, method = "stack", 
           offset = .5, 
           at = 0, 
           pch = 19,
           col = "lightpink1", 
           main = "Weights of Chickens on Diet 3, on Day 14", 
           xlab = "Weight (grams)")

#histogram of weights of chickens on day0
hist(ChickWeight$weight[ChickWeight$Time %in% 0], 
     col= "skyblue", 
     main= "Chicken Weights on Day 0", 
     xlab= "Weight(grams)", 
     ylab="Number of Chickens")

#boxplot of chicken weights on day 21 grouped by diet

boxplot(weight~Diet,data=ChickWeight,
        col=c("pink", "green", "skyblue", "purple"),
        main= "Comparison of Chicken Diets",
        xlab= "Diet",
        ylab= "Weight(grams)",
        names= c("Diet1", "Diet2", "Diet3", "Diet4")
        )




