# GUIDE  ====
# Tables: 
#   - turo.df : original data
#   - tables with format variable_name.freq, e.g. car.city.freq : frequency distribution tables
#   - summary.cont.df : summary statistics of continuous variables
# ___________

#install.packages("e1071")
#install.packages("corrplot")
library(e1071)
library(corrplot)
# FUNCTIONS ====
# Function to calculate relative and percentage frequency
get.freq <- function(df) {
  df$Rel.Freq <- df$Freq / sum(df$Freq)
  df$Pct.Freq <- 100 * df$Rel.Freq
  return(df)
}

# Function to get frequency distribution for continuous variables
get.freq.cont <- function(df, var.name, bin.num = NULL) {
  df <- na.omit(df)
  bin.num <- ifelse(is.null(bin.num), nclass.FD(df[[var.name]]), bin.num)
  var.bin <- cut(df[[var.name]], bin.num)
  freq.df <- data.frame(table(var.bin))
  names(freq.df)[1] <- paste(var.name, ".bin", sep = "")
  # Relative and percent frequency distribution
  freq.df$Rel.Freq <- freq.df$Freq / sum(freq.df$Freq)
  freq.df$Pct.Freq <- 100.00 * freq.df$Rel.Freq
  # Cumulative (relative and percent) frequency distribution
  freq.df$Cum.Freq <- cumsum(freq.df$Freq)
  freq.df$Cum.Rel.Freq <- cumsum(freq.df$Rel.Freq)
  freq.df$Cum.Pct.Freq <- cumsum(freq.df$Pct.Freq)
  return (freq.df)
}

# Function to get top n freq
get.top.n.freq <- function(df,n) {
  return (head(df[order(df$Freq, decreasing = TRUE), ], n))
}

# Function to calculate minimum, first quartile, median, mean, third quartile, maximum, standard deviation, and skewness of continuous variables
get.summary.cont <- function(col) {
  return (list(
    Minimum = min(col, na.rm = TRUE),
    First_Quartile = quantile(col, 0.25, type = 6, na.rm = TRUE),
    Median = median(col, na.rm = TRUE),
    Mean = mean(col, na.rm = TRUE),
    Third_Quartile = quantile(col, 0.75, type = 6, na.rm = TRUE),
    Maximum = max(col, na.rm = TRUE),
    Standard_Deviation = sd(col, na.rm = TRUE),
    Skewness = skewness(col, na.rm = TRUE)
  ))
}

# Print summary statistics of continuous variable
print.summary.cont <- function(summary.df, var.name) {
  summary = summary.df[summary.df$Variable == var.name, ]
  print(paste("Summary Statistics for variable", var.name, ":"))
  print(paste("Minimum:", summary$Minimum))
  print(paste("First quartile:", summary$First_Quartile))
  print(paste("Median:", summary$Median))
  print(paste("Mean:", summary$Mean))
  print(paste("Third quartile:", summary$Third_Quartile))
  print(paste("Maximum:", summary$Maximum))
  print(paste("Standard deviation:", summary$Standard_Deviation))
  print(paste("Skewness:", summary$Skewness))
}

# Function to draw pie chart of freq table
draw.pie.freq <- function(df, chart.name) {
  par(mar = c(1,2,4,2))
  pie(df$Freq, main = paste("Pie Chart of", chart.name, sep = " "),
      labels = paste(df[[1]], " (", round(df$Pct.Freq, digit=2), "%)", sep = ""))
}

# Function to draw bar chart of freq table
draw.bar.freq <- function(df, chart.name, xlab = "") {
  par(mar = c(5,5,4,2))
  barplot(df$Freq, names.arg = df[[1]],
          xlab = xlab, ylab = "Frequency", main = paste("Bar Chart of", chart.name, sep = " "), las = 2)
}

# Function to draw bar chart of top n in freq table
draw.bar.top.n.freq <- function(df, n, chart.name, horiz = FALSE, short.names.arg = FALSE, bottom = 8, left = 8) {
  # Get top n freq from df
  top.n.freq = get.top.n.freq(df,n)
  if (short.names.arg == FALSE) {
    names.arg = top.n.freq[[1]]
  } else {
    names.arg = gsub("\\s*\\([^()]*\\)", "", top.n.freq[[1]])
  }
  # graph padding
  par(mar = c(bottom,left,4,2))
  
  if (horiz == FALSE) {
    barplot(top.n.freq$Freq, names.arg = names.arg,
            xlab = "", ylab = "Frequency", main = paste("Bar Chart of", chart.name, sep = " "), las = 2, cex.axis=0.8, cex.names=0.8, horiz = horiz)
  } else {
    barplot(top.n.freq$Freq, names.arg = names.arg,
            xlab = "Frequency", ylab = "", main = paste("Bar Chart of", chart.name, sep = " "), las = 2, cex.axis = 0.8, cex.names=0.8, horiz = horiz)
  }
}

draw.hist <- function(col, chart.name, xlab="") {
  par(mar = c(5,5,4,2))
  hist(col, breaks = "FD", xlab = xlab,
       main = paste("Histogram of", chart.name, sep = " "))
}

# 1. DATA EXPLORATION ====
# 1A - Examination of each variable ====
turo.df <- readRDS("turo.data.5140")
# Get variable names 
variable.names <- colnames(turo.df)

categorical.variable.names <- c(
  "car.city", "car.deliver.to.you.num", "car.doors", 
  "car.extra.beach.gear", "car.extra.child.safety.seat", 
  "car.extra.cooler", "car.extra.one.way.trip", 
  "car.extra.pet.fee", "car.extra.phone.mount", 
  "car.extra.portable.gps", "car.extra.post.trip.cleaning", 
  "car.extra.prepaid.ev.recharge", "car.extra.prepaid.refuel", 
  "car.extra.stroller", "car.extra.unlimited.mileage", 
  "car.instant.book", "car.insurance", "car.make", 
  "car.model", "car.photo.verified", "car.power", 
  "car.rental.type", "car.self.pickup.num", "car.state", 
  "car.transmission", "car.turo.go", "host.all.star", 
  "host.location.available", "host.verified.approved.to.drive", 
  "host.verified.email", "host.verified.fb", "host.verified.phone"
)

continuous.variable.names <- setdiff(variable.names, categorical.variable.names)

# Create variable frequency names
categorical.variable.freq.names <- lapply(categorical.variable.names, function(x){paste(x, ".freq", sep = "")})

continuous.variable.freq.names <- lapply(continuous.variable.names, function(x){paste(x, ".freq", sep = "")})

# *CATEGORICAL variables ====
# **Get Summary Statistics ====
for (i in 1:length(categorical.variable.freq.names)) {
  variable.freq.name = categorical.variable.freq.names[[i]]
  col.name = categorical.variable.names[[i]]
  # assign each variable the corresponding freq df 
  assign(variable.freq.name, data.frame(table(turo.df[[col.name]])))
  
  freq.df = get(variable.freq.name)
  # get relative and percentage frequency
  freq.df <- get.freq(freq.df)
  names(freq.df)[1] <- col.name
  assign(variable.freq.name, freq.df)
}

# **Plots ====

# __car.city ====
# Get top 20 most frequent cities
top20.freq.cities = head(car.city.freq[order(car.city.freq$Freq, decreasing = TRUE), ], 20)

# Bar chart of car.city
# graph padding
par(mar = c(8,5,4,2))
barplot(car.city.freq$Freq,
        xlab = "Cities", ylab = "Frequency", main = "Bar Chart of Car Location (City)")

# Bar chart of top 20 most frequent cities
barplot(top20.freq.cities$Freq, names.arg = top20.freq.cities[[1]],
        xlab = "", ylab = "Frequency", main = "Bar Chart of Top 20 Most Frequent Cities", las = 2)

# Bar chart of top 20 most frequent cities (%)
barplot(top20.freq.cities$Pct.Freq, names.arg = top20.freq.cities[[1]],
        xlab = "", ylab = "Percentage Frequency (%)", main = "Bar Chart of Top 20 Most Frequent Cities", las = 2)

# __car.deliver.to.you.num ====
# Pie chart of car.deliver.to.you.num
draw.pie.freq(car.deliver.to.you.num.freq, chart.name = "Car Delivery Availability (0: No, 1: Yes)")

# __car.doors ====
draw.bar.freq(car.doors.freq, chart.name = "Number of Car Doors", xlab = "Number of Doors")

# __car.extra.beach.gear ====
draw.pie.freq(car.extra.beach.gear.freq, chart.name = "Beach Gear Availability")

# __car.extra.child.safety.seat ====
draw.pie.freq(car.extra.child.safety.seat.freq, chart.name = "Child Safety Seat Availability")

# __car.extra.cooler ====
draw.pie.freq(car.extra.cooler.freq, chart.name = "Cooler Availability")

# __car.extra.one.way.trip ====
draw.pie.freq(car.extra.one.way.trip.freq, chart.name = "One Way Trip Availability")

# __car.extra.pet.fee ====
draw.pie.freq(car.extra.pet.fee.freq, chart.name = "Pet Allowance Availability for a Fee")

# __car.extra.phone.mount ====
draw.pie.freq(car.extra.phone.mount.freq, chart.name = "Phone Mount Availability")

# __car.extra.portable.gps ====
draw.pie.freq(car.extra.portable.gps.freq, chart.name = "Portable GPS Availability")

# __car.extra.post.trip.cleaning ====
draw.pie.freq(car.extra.post.trip.cleaning.freq, chart.name = "Post Trip Cleaning Availability")

# __car.extra.prepaid.ev.recharge ====
draw.pie.freq(car.extra.prepaid.ev.recharge.freq, chart.name = "Prepaid EV Recharge Availability")

# __car.extra.prepaid.refuel ====
draw.pie.freq(car.extra.prepaid.refuel.freq, chart.name = "Prepaid Refuel Availability")

# __car.extra.stroller ====
draw.pie.freq(car.extra.stroller.freq, chart.name = "Stroller Availability")

# __car.extra.unlimited.mileage ====
draw.pie.freq(car.extra.unlimited.mileage.freq, chart.name = "Unlimited Mileage Availability")

# __car.instant.book ====
draw.pie.freq(car.instant.book.freq, chart.name = "Instant Booking Availability")

# __car.insurance ====
ins.df <- data.frame(
  Company = c("Liberty Mutual", "Others"),
  Freq = c(car.insurance.freq$Freq[car.insurance.freq$car.insurance == "Liberty Mutual"], sum(car.insurance.freq$Freq[car.insurance.freq$car.insurance != "Liberty Mutual"]))
)
ins.df$Pct.Freq = 100 * ins.df$Freq / sum(ins.df$Freq)

draw.pie.freq(ins.df, "Car Insurances")

# __car.make ====
draw.bar.top.n.freq(car.make.freq, 20, "Top 20 Most Frequent Car Makes")

# __car.model ====
draw.bar.top.n.freq(car.model.freq, 20, "Top 20 Most Frequent Car Models")

# __car.photo.verified ====
draw.pie.freq(car.photo.verified.freq, chart.name = "Car Photos Verification Status")

# __car.power ====
par(mar = c(8,5,4,2))
barplot(car.power.freq$Freq, names.arg = car.power.freq[[1]],
        xlab = "", ylab = "Frequency", main = paste("Bar Chart of Car Power Types"), las = 2, cex.names = 0.8, cex.axis = 0.8)

# __car.rental.type ====
par(mar = c(8,5,4,2))
barplot(car.rental.type.freq$Freq, names.arg = car.rental.type.freq[[1]],
        xlab = "", ylab = "Frequency", main = paste("Bar Chart of Car Rental Types"), las = 2, cex.names = 0.8, cex.axis = 0.8)
draw.pie.freq(car.rental.type.freq, "Car Rental Types")

# __car.self.pickup.num ====
draw.pie.freq(car.self.pickup.num.freq, "Self Pickup Availability (0: No, 1: Yes)")

# __car.state ====
draw.bar.freq(car.state.freq, "Car States")

# __car.transmission ====
draw.pie.freq(car.transmission.freq, "Car Transmission Types")

# __car.turo.go ====
draw.pie.freq(car.turo.go.freq, "Turo Go App Unlock Capability")

# __host.all.star ====
draw.pie.freq(host.all.star.freq, "All-Star Hosts")

# __host.location.available ====
draw.pie.freq(host.location.available.freq, "Host Location Availability")

# __host.verified.approved.to.drive ====
draw.pie.freq(host.verified.approved.to.drive.freq, "Host Approval to Drive")

# __host.verified.email ====
draw.pie.freq(host.verified.email.freq, "Host Email Verification Status")

# __host.verified.fb ====
draw.pie.freq(host.verified.fb.freq, "Host Facebook Verification Status")

# __host.verified.phone ====
draw.pie.freq(host.verified.phone.freq, "Host Phone Number Verification Status")

# *CONTINUOUS variables ====

# **Get Summary Statistics ====
# Summary statistics includes minimum, first quartile, median, mean, third quartile, maximum, standard deviation, and skewness
summary.cont.df <- data.frame(Variable = continuous.variable.names)
# For each continuous variable, create and add the summary statistics of the variable to summary.cont.df
for (i in 1:length(continuous.variable.names)) {
  var.name = continuous.variable.names[[i]]
  summary.cont = get.summary.cont(turo.df[[var.name]])
  for (measure in names(summary.cont)) {
    summary.cont.df[i,measure] <- summary.cont[[measure]]
  }
}

# To print summary statistics of an individual variable, use function print.summary.cont(), for example, to print summary statistics of car.deliver.airport.num, use:
# print.summary.cont(summary.cont.df,"car.deliver.airport.num")

# *** Handle variable car.miles.included which contains Inf value (unlimited miles) ====
# Get freq table of Unlimited Car Miles
unlimited.miles.sum = sum(is.infinite(turo.df$car.miles.included))
car.unlimited.miles.included.freq <- data.frame(Is_Unlimited_Miles_Included = c(TRUE, FALSE), Freq = c(unlimited.miles.sum, nrow(na.omit(turo.df))-unlimited.miles.sum))
car.unlimited.miles.included.freq <- get.freq(car.unlimited.miles.included.freq)
# Draw pie chart
draw.pie.freq(car.unlimited.miles.included.freq, "Unlimited Car Miles Eligibility")

# Create a table of Limited Car Miles
car.limited.miles.included.list <- turo.df$car.miles.included[!(is.infinite(turo.df$car.miles.included) | is.na(turo.df$car.miles.included))]
car.limited.miles.included.df = data.frame(car.miles.included = car.limited.miles.included.list)

# Get summary statistics for car.limited.miles.included
car.limited.miles.included.summary <- get.summary.cont(car.limited.miles.included.df$car.miles.included)
summary.cont.df[nrow(summary.cont.df)+1,] <- c(Variable = "car.limited.miles.included", car.limited.miles.included.summary)
print.summary.cont(summary.cont.df, "car.limited.miles.included")

# **Plots ====

# __car.deliver.airport.num ====
# Histogram
draw.hist(turo.df$car.deliver.airport.num, chart.name = "Number of Airports the Car can be Delivered to", xlab = "Number of Airports")

# Box plot
boxplot(turo.df$car.deliver.airport.num ~ turo.df$car.state,
        xlab = "State", ylab = "Number of Airports",
        main = "Box Plot of Number of Airports the Car can be Delivered to by State")

# __car.deliver.hotel.num ====
# Histogram
draw.hist(turo.df$car.deliver.hotel.num, chart.name = "Number of Hotels the Car can be Delivered to")

# Box plot
boxplot(turo.df$car.deliver.hotel.num ~ turo.df$car.state,
        xlab = "State", ylab = "Number of Hotels",
        main = "Box Plot of Number of Hotels the Car can be Delivered to by State")

# __car.deliver.train.station.num ====
# Histogram
draw.hist(turo.df$car.deliver.train.station.num, chart.name = "Number of Train Stations the Car can be Delivered to")

# Box plot
boxplot(turo.df$car.deliver.train.station.num ~ turo.df$car.state,
        xlab = "State", ylab = "Number of Train Stations",
        main = "Box Plot of Number of Train Stations the Car can be Delivered to by State")

# __car.displayed.turo.review.num ====
# Histogram
draw.hist(turo.df$car.displayed.turo.review.num, chart.name = "Number of Turo Reviews (trip cancelled by host) in Total")

# __car.displayed.turo.review.num.past.12m ====
# Histogram
draw.hist(turo.df$car.displayed.turo.review.num.past.12m, chart.name = "Number of Turo Reviews in Past 12 Months")

# __car.displayed.turo.review.num.past.18m ====
# Histogram
draw.hist(turo.df$car.displayed.turo.review.num.past.18m, chart.name = "Number of Turo Reviews in Past 18 Months")

# __car.displayed.turo.review.num.past.6m ====
# Histogram
draw.hist(turo.df$car.displayed.turo.review.num.past.6m, chart.name = "Number of Turo Reviews in Past 6 Months")

# __car.displayed.user.review.num ====
# Histogram
draw.hist(turo.df$car.displayed.user.review.num, chart.name = "Number of Guest Reviews in Total")

# __car.displayed.user.review.num.past.12m ====
draw.hist(turo.df$car.displayed.user.review.num.past.12m, chart.name = "Number of Guest Reviews in Past 12 Months")

# __car.displayed.user.review.num.past.18m ====
draw.hist(turo.df$car.displayed.user.review.num.past.18m, chart.name = "Number of Guest Reviews in Past 18 Months")

# __car.displayed.user.review.num.past.6m ====
draw.hist(turo.df$car.displayed.user.review.num.past.6m, chart.name = "Number of Guest Reviews in Past 6 Months")

# __car.extra.mile.fee ====
draw.hist(turo.df$car.extra.mile.fee, chart.name = "Fee per Additional Mile Beyond Miles Included")

# __car.extra.num ====
draw.hist(turo.df$car.extra.num, chart.name = "Number of Extra Services Available")

# __car.faq.num ====
draw.hist(turo.df$car.faq.num, chart.name = "Number of FAQ")

# __car.miles.included ====
draw.hist(turo.df$car.miles.included, chart.name = "Miles Included")

# __car.photo.num ====
draw.hist(turo.df$car.photo.num, chart.name = "Number of Car Photos")

# __car.self.pickup.avg.price ====
draw.hist(turo.df$car.self.pickup.avg.price, chart.name = "Self Pick Up Price")

# __car.trip.price ====
draw.hist(turo.df$car.trip.price, chart.name = "Car Trip Price")

# __car.year ====
draw.hist(turo.df$car.year, chart.name = "Car Model Year")

# __host.car.num ====
draw.hist(turo.df$host.car.num, chart.name = "Total Number of Cars The Host Own")

# __host.tenure.in.weeks ====
draw.hist(turo.df$host.tenure.in.weeks, chart.name = "Host Tenure on Turo (in Weeks)")

# 1B Examine the correlation between dependent variable (car.trip.price) and other variables ====

# *Correlation of non-factor variables ====
# Get only non-factor columns
non.factor.turo.df <- na.omit(turo.df)[sapply(turo.df, function(x) !is.factor(x))]
non.factor.turo.df <- subset(non.factor.turo.df, select = -c(car.self.pickup.avg.price,car.self.pickup.num))

# Calculate the correlation matrix
cor.matrix <- cor(non.factor.turo.df)
cor.df <- data.frame(cor.matrix)

# Create correlation matrix graph
png("correlation_matrix.png", width = 1200, height = 800)
corrplot(cor.matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, tl.cex = 0.7)
dev.off()

# Correlation of car.trip.price vs other variables
car.trip.price.cor.df <- data.frame(t(cor.df["car.trip.price", ]))
colnames(car.trip.price.cor.df)[[1]] <- "correlation"
# Create a column of absolute value so we can sort by correlation strength
car.trip.price.cor.df$correlation.absolute <- abs(car.trip.price.cor.df$correlation)


# 2. DATA CLEANING ====
# Remove NA
print(paste("Rows before removing missing values:", nrow(turo.df)))
turo.df <- na.omit(turo.df)
print(paste("Rows after removing missing values:", nrow(turo.df)))

remove.outliers <- function(df, var.name) {
  Q1 <- quantile(df[[var.name]], 0.25, type = 7)
  Q3 <- quantile(df[[var.name]], 0.75, type = 7)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR

  print(paste(var.name, ".Q1: ", Q1, sep=""))
  print(paste(var.name, ".Q3: ", Q3, sep=""))
  print(paste(var.name, ".IQR: ", IQR, sep=""))
  print(paste(var.name, ".IQ.lower: ", lower_bound, sep=""))
  print(paste(var.name, ".IQ.upper: ", upper_bound, sep=""))
  
  new.df <- df[df[[var.name]] >= lower_bound & df[[var.name]] <= upper_bound, ]
  print(paste("Rows after removing outliers of", var.name, ":", nrow(new.df)))
 
   return(new.df) 
}

print(paste("Rows before removing outliers:", nrow(turo.df)))

#car.deliver.airport.num
turo.df <- remove.outliers(turo.df, "car.deliver.airport.num")

#car.deliver.hotel.num
turo.df <- remove.outliers(turo.df, "car.deliver.hotel.num")

#car.deliver.train.station.num
turo.df <- remove.outliers(turo.df, "car.deliver.train.station.num")

#car.displayed.turo.review.num
turo.df <- remove.outliers(turo.df, "car.displayed.turo.review.num")

#car.displayed.turo.review.num.past.12m
turo.df <- remove.outliers(turo.df, "car.displayed.turo.review.num.past.12m")

#car.displayed.turo.review.num.past.18m
turo.df <- remove.outliers(turo.df, "car.displayed.turo.review.num.past.18m")

#car.displayed.turo.review.num.past.6m
turo.df <- remove.outliers(turo.df, "car.displayed.turo.review.num.past.6m")

#car.displayed.user.review.num
turo.df <- remove.outliers(turo.df, "car.displayed.user.review.num")

#car.displayed.user.review.num.past.12m
turo.df <- remove.outliers(turo.df, "car.displayed.user.review.num.past.12m")

#car.displayed.user.review.num.past.18m
turo.df <- remove.outliers(turo.df, "car.displayed.user.review.num.past.18m")

#car.displayed.user.review.num.past.6m
turo.df <- remove.outliers(turo.df, "car.displayed.user.review.num.past.6m")

#car.extra.mile.fee
turo.df <- remove.outliers(turo.df, "car.extra.mile.fee")

#car.extra.num
turo.df <- remove.outliers(turo.df, "car.extra.num")

#car.faq.num
turo.df <- remove.outliers(turo.df, "car.faq.num")

#car.miles.included
turo.df <- remove.outliers(turo.df, "car.miles.included")

#car.photo.num
turo.df <- remove.outliers(turo.df, "car.photo.num")

#car.self.pickup.avg.price
turo.df <- remove.outliers(turo.df, "car.self.pickup.avg.price")

#car.trip.price
turo.df <- remove.outliers(turo.df, "car.trip.price")

#car.year
turo.df <- remove.outliers(turo.df, "car.year")

#host.car.num
turo.df <- remove.outliers(turo.df, "host.car.num")

#host.tenure.in.weeks
turo.df <- remove.outliers(turo.df, "host.tenure.in.weeks")

# DATA ANALYSIS (Modeling and Explanation) ====
# Check whether there is NA, Inf, or NaN in the variable
summary(turo.df)

# Create the several models
#Consider age and mileage related variables
model1 <- lm(car.trip.price ~ car.year + car.extra.mile.fee + car.extra.num +
               car.photo.num + host.tenure.in.weeks, data = turo.df)
#Age and review related variables were considered
model2 <- lm(car.trip.price ~ car.year + car.extra.mile.fee + car.extra.num +
               car.displayed.turo.review.num + car.photo.num, data = turo.df)

#Consider mileage charges and airport transfer related variables
model3 <- lm(car.trip.price ~ car.extra.mile.fee + car.extra.num +
               car.photo.num + car.deliver.airport.num + host.tenure.in.weeks, data = turo.df)

#Also consider age, reviews and airport transfers
model4 <- lm(car.trip.price ~ car.year + car.extra.mile.fee +
               car.photo.num + car.deliver.airport.num + car.displayed.user.review.num, data = turo.df)


# Compare model performance
summary(model1)
summary(model2)
summary(model3)
summary(model4)
#Extract the adjusted R^2 value
adj_r2_model1 <- summary(model1)$adj.r.squared
adj_r2_model2 <- summary(model2)$adj.r.squared
adj_r2_model3 <- summary(model3)$adj.r.squared
adj_r2_model4 <- summary(model4)$adj.r.squared
#Print the adjusted R^2 value
cat("Adjusted R-squared for Model 1:", adj_r2_model1, "")
cat("Adjusted R-squared for Model 2:", adj_r2_model2, "")
cat("Adjusted R-squared for Model 3:", adj_r2_model3, "")
cat("Adjusted R-squared for Model 4:", adj_r2_model4, "")
#Comparative AIC value
AIC(model1, model2, model3, model4)
#Run VIF checks on candidate models
library(car)

print("VIF for model1:")
vif(model1)

print("VIF for model2:")
vif(model2)

print("VIF for model3:")
vif(model3)

print("VIF for model4:")
vif(model4)

# Analysis Charts---------------------------------------------------------------
# Residuals vs Fitted
#Model1
plot(model1$fitted.values, residuals(model1), 
     main = "Residuals vs Fitted1", 
     xlab = "Fitted Values", ylab = "Residuals", pch = 19, col = "blue")
abline(h = 0, col = "red", lty = 2)
#Model2
plot(model2$fitted.values, residuals(model2), 
     main = "Residuals vs Fitted2", 
     xlab = "Fitted Values", ylab = "Residuals", pch = 19, col = "blue")
abline(h = 0, col = "red", lty = 2)
#Model3
plot(model3$fitted.values, residuals(model3), 
     main = "Residuals vs Fitted3", 
     xlab = "Fitted Values", ylab = "Residuals", pch = 19, col = "blue")
abline(h = 0, col = "red", lty = 2)
#Model4
plot(model4$fitted.values, residuals(model4), 
     main = "Residuals vs Fitted4", 
     xlab = "Fitted Values", ylab = "Residuals", pch = 19, col = "blue")
abline(h = 0, col = "red", lty = 2)

#modified the model1 that make sure is precise
model1_refined <- lm(car.trip.price ~ car.year + car.extra.mile.fee + car.extra.num + 
                       host.tenure.in.weeks, data = turo.df)
summary(model1_refined)

AIC(model1, model1_refined)

#Normal Q-Q plot
qqnorm(residuals(model1), main = "Normal Q-Q")
qqline(residuals(model1), col = "blue")

#Actual vs predicted value graph
predicted_values <- predict(model1, newdata = turo.df)
plot(turo.df$car.trip.price, predicted_values, 
     main = "Actual vs Predicted", 
     xlab = "Actual Values", ylab = "Predicted Values", pch = 19, col = "blue")
abline(0, 1, col = "red", lty = 2) 

#Diagram of relationship between variable and dependent variable----------------
# The relationship between car.year and car trip price
plot(turo.df$car.year, turo.df$car.trip.price, 
     main = "car.year vs Trip Price", 
     xlab = "car.year", ylab = "Trip Price", pch = 19, col = "blue")
#The relationship between car.extra num, and car trip price
boxplot(car.trip.price ~ car.extra.num, data = turo.df,
        main = "Trip Price by extra num",
        xlab = "car extra num ", ylab = "Trip Price", col = "lightblue")
#The relationship between car.photo.num and car trip price
plot(turo.df$car.photo.num, turo.df$car.trip.price, 
     main = "Car photo num vs Trip Price", 
     xlab = "Car photo num ", ylab = "Trip Price", pch = 19, col = "blue")
#The relationship between car.extra.mile.fee and car trip price
plot(turo.df$car.extra.mile.fee, turo.df$car.trip.price, 
     main = "car.extra.mile.fee vs Trip Price", 
     xlab = "car.extra.mile.fee", ylab = "Trip Price", pch = 19, col = "blue")
#The relationship between host.tenure.in.weeks and car trip price
plot(turo.df$host.tenure.in.weeks, turo.df$car.trip.price, 
     main = "host.tenure.in.weeks vs Trip Price", 
     xlab = "host.tenure.in.weeks", ylab = "Trip Price", pch = 19, col = "blue")

#Importance diagram of variables
coef_values <- abs(coef(model1))  # The absolute value of regression coefficient was extracted
barplot(coef_values[-1],  # Removes the intercept item
        main = "Variable Importance", 
        xlab = "Variables", ylab = "Absolute Coefficients", col = "skyblue", las = 2)


# Build the multiple linear regression model
# Convert categorical variable to factor
turo.df$car.transmission <- as.factor(turo.df$car.transmission)
summary(turo.df)

# Build the multiple linear regression model
model <- lm(car.trip.price ~ car.year + car.miles.included + car.transmission + car.extra.num + host.tenure.in.weeks, 
            data = turo.df)
# Summarize the model to view results
summary(model)

# Extract coefficients from the model
coef_values <- coef(model)  
coef_values <- coef_values[-1]  

# Create a data frame for better visualization
coef_df <- data.frame(
  Variable = names(coef_values),
  Coefficient = coef_values
)
summary(model)$coefficients

# Plotting the coefficients
library(ggplot2)

ggplot(coef_df, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip the axes for better readability
  labs(title = "Coefficient Magnitudes for Predictors of Car Trip Price",
       x = "Predictor Variables",
       y = "Coefficient Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Evaluating model performance 
adj_r2 <- summary(model)$adj.r.squared
cat("Adjusted R-squared for the model:", adj_r2, "")
AIC(model)


#Model Diagnostics

# Residuals vs Fitted plot
plot(model$fitted.values, residuals(model), main = "Residuals vs Fitted", 
     xlab = "Fitted Values", ylab = "Residuals", pch = 19, col = "blue")
abline(h = 0, col = "red", lty = 2)

# Normal Q-Q plot
qqnorm(residuals(model), main = "Normal Q-Q Plot")
qqline(residuals(model), col = "blue")

#Model Selection (Comparing Multiple Models)
# Fit multiple models
model1 <- lm(car.trip.price ~ car.year + car.miles.included + car.transmission + car.extra.num + host.tenure.in.weeks, data = turo.df)
model2 <- lm(car.trip.price ~ car.year + car.miles.included + car.extra.num + car.deliver.airport.num + car.photo.num, data = turo.df)
model3 <- lm(car.trip.price ~ car.year + car.extra.mile.fee + car.transmission + car.extra.num + host.car.num, data = turo.df)
model4 <- lm(car.trip.price ~ car.year + car.transmission + car.miles.included + car.extra.num + car.photo.num, data = turo.df)

# Calculate AIC for each model
aic_values <- c(AIC(model1), AIC(model2), AIC(model3), AIC(model4))

# Create a data frame with model names and their AIC values
aic_df <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4"),
  AIC = aic_values
)

# Plotting the AIC values using ggplot2
library(ggplot2)

ggplot(aic_df, aes(x = Model, y = AIC, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "AIC Comparison of Different Models",
       x = "Model", y = "AIC") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")
vif(model)
# Calculate VIFs for the model
library(car)
vif_values <- vif(model)

# Create a data frame for the VIF values
vif_df <- data.frame(
  Variable = names(vif_values),
  VIF = vif_values
)

# Plotting the VIF values using ggplot2
library(ggplot2)

ggplot(vif_df, aes(x = reorder(Variable, VIF), y = VIF)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +  # Flip the plot for better readability
  labs(title = "Variance Inflation Factors (VIF) for Predictors",
       x = "Predictor Variables",
       y = "VIF Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(vif_values)

#Different Models to chose the best one
# Model 1: Basic car features + host tenure
model1 <- lm(car.trip.price ~ car.year + car.miles.included + car.transmission + car.extra.num + host.tenure.in.weeks, data = turo.df)

# Model 2: Host-related features (including number of photos and delivery options)
model2 <- lm(car.trip.price ~ car.year + car.miles.included + car.extra.num + car.deliver.airport.num + car.photo.num, data = turo.df)

# Model 3: Pricing-related features with host variables
model3 <- lm(car.trip.price ~ car.year + car.extra.mile.fee + car.transmission + car.extra.num + host.car.num, data = turo.df)

# Model 4: Combination of car attributes, transmission type, and photos
model4 <- lm(car.trip.price ~ car.year + car.transmission + car.miles.included + car.extra.num + car.photo.num, data = turo.df)

# Evaluate models using Adjusted R-squared and AIC
model_metrics <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3", "Model 4"),
  Adjusted_R2 = c(summary(model1)$adj.r.squared, 
                  summary(model2)$adj.r.squared, 
                  summary(model3)$adj.r.squared, 
                  summary(model4)$adj.r.squared),
  AIC = c(AIC(model1), AIC(model2), AIC(model3), AIC(model4))
)

# Print the model evaluation metrics
print(model_metrics)

# Plot Model Evaluation (AIC and Adjusted R-squared)

# Load ggplot2 library
library(ggplot2)



# Plot Adjusted R-squared comparison
ggplot(model_metrics, aes(x = Model, y = Adjusted_R2, fill = Model)) +
  geom_bar(stat = "identity") +
  labs(title = "Adjusted R-squared Comparison of Different Models",
       x = "Model", y = "Adjusted R-squared") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")


# Plot for Model 1
plot(model1$fitted.values, residuals(model1), main = "Residuals vs Fitted (Model 1)", xlab = "Fitted Values", ylab = "Residuals", pch = 19, col = "blue")
abline(h = 0, col = "red", lty = 2)
#Model Interpretation
summary(model1)

# Plot for Model 2
plot(model2$fitted.values, residuals(model2), main = "Residuals vs Fitted (Model 2)", xlab = "Fitted Values", ylab = "Residuals", pch = 19, col = "blue")
abline(h = 0, col = "red", lty = 2)
#Model Interpretation
summary(model2)

# Plot for Model 3
plot(model3$fitted.values, residuals(model3), main = "Residuals vs Fitted (Model 3)", xlab = "Fitted Values", ylab = "Residuals", pch = 19, col = "blue")
abline(h = 0, col = "red", lty = 2)
#Model Interpretation
summary(model3)

# Plot for Model 4
plot(model4$fitted.values, residuals(model4), main = "Residuals vs Fitted (Model 4)", xlab = "Fitted Values", ylab = "Residuals", pch = 19, col = "blue")
abline(h = 0, col = "red", lty = 2)
#Model Interpretation
summary(model4)






















