mcdonalds = list( c("Big Mac", 4.55, 530), c("Quarter Pounder with Cheese", 4.32, 520), c("Cheeseburger", 1.14, 290),
                  c("Double Quarter Pounder with Cheese", 5.46, 750), c("McDouble", 1.58, 380), c("McChicken", 1.47, 360),
                  c("BBQ Ranch Burger", 1.14, 340), c("Filet-O-Fish", 4.32, 390), c("Bacon Clubhouse Burger", 5.12, 720),
                  c("Southern Style Crispy Chicken Sandwich", 5.00, 430), c("Bacon Clubhouse Crispy Chicken Sandwich", 5.12, 750),
                  c("10 pc Chicken McNuggets", 5.12, 470), c("20 pc Chicken McNuggets", 5.70, 940),
                  c("Small French Fries", 1.58, 230), c("Medium French Fries", 2.04, 340), c("Large French Fries", 2.15, 510),
                  c("Small Soft Drink", 1.14, 160), c("Medium Soft Drink", 1.47, 230), c("Large Soft Drink", 1.70, 310),
                  c("Sausage McMuffin", 1.36, 370), c("Big Mac Meal", 6.83, 1190), c("Quarter Pounder with Cheese Meal", 6.60, 1180),
                  c("Double Quarter Pounder with Cheese Meal", 7.63, 1310), c("10 pc Chicken McNuggets Meal", 7.40, 1030),
                  c("Filet-O-Fish Meal", 6.60, 950), c("Southern Style Crispy Chicken Sandwich Meal", 7.28, 990))

solve_knapsack <- function(max_price, items){
  if (length(items) == 0){
    return(0)
  }
  count <- 100 * max_price + 1
  table <- rep(list(c()), length(items) + 1)
  table[[1]] <- rep(0, count)
  for (i in 1:length(items)){
    for (j in 1:count ){
      if (as.numeric(items[[i]][2]) * 100 > j - 1){
        table[[i + 1]][j] <- table[[i]][j]
      }
      else{
        table[[i + 1]][j] <- max(table[[i]][j], table[[i]][j - as.numeric(items[[i]][2]) * 100] + as.numeric(items[[i]][3]) )
      }
    }
  }
  return(table[[length(items) + 1]][count])
}
ratios <- c()
for (i in mcdonalds){
  ratios <- c(ratios, as.numeric(i[3])/as.numeric(i[2]))
}
foods <- c()
for (i in mcdonalds){
  foods <- c(foods, i[1])
}
food_ratios <- data.table(foods, ratios, key = "ratios")
y <- c()
for (i in seq(0, 40, 0.1)){
  temp <- solve_knapsack(i, mcdonalds)
  y <- c(y, temp[length(temp)])
}
y <- as.numeric(y)
x <- seq(0, 40, 0.1)
data <- data.frame(x,y)
ggplot(data, aes(x,y)) + geom_point(aes(color = 'RED')) + geom_smooth()  + theme_bw() + 
  labs(x = "Maximum Budget", y = "Total Calories", title = "Maximum Calories Across a Variety of Budgets")
derivative <- c()
for (i in 2:length(y)){
  derivative <- c(derivative, (y[i] - y[i-1]) / 0.1)
}
derivative <- c(y[1] /0.1, derivative)
data2 <- data.frame(x,derivative)
ggplot(data2, aes(x,derivative)) + geom_point(aes(color = 'RED')) + geom_smooth()  + theme_bw() +
  labs(x = "Maximum Budget", title = "Derivatives of Calories vs. Budget Constraints")