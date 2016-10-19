require(ggplot2)
data(diamonds)
str(diamonds)

ggplot(aes(x = x, y = price), data = diamonds) +
  geom_point()

# It appears that price tends to grow exponentially as the characteristic of x increases.There are some odd values whose price is non-zero, yet their x characteristic is zero.

cor.test(diamonds$price, diamonds$x, method = "pearson")$estimate

cor.test(diamonds$price, diamonds$y, method = "pearson")$estimate

cor.test(diamonds$price, diamonds$z, method = "pearson")$estimate

ggplot(aes(x = depth, y = price), data = diamonds) +
  geom_point()

ggplot(aes(x = depth, y = price), data = diamonds) +
  geom_point(alpha = 1/100) +
  scale_x_continuous(breaks = seq(min(diamonds$depth),max(diamonds$depth),2))

# Most diamonds are of a depth between 59 and 64.

cor.test(diamonds$depth, diamonds$price, method = "pearson")$estimate

# The correlation between depth of a diamond and price is almost zero, meaning that if we know a diamond's depth, it will not be strongly positively or negatively correlated with price.

ggplot(aes(x = carat, y = price), data = subset(diamonds, quantile(diamonds$carat, 0.99) & quantile(diamonds$price, 0.99))) +
  geom_point()

diamonds$volume <- (diamonds$x * diamonds$y * diamonds$z)

ggplot(aes(x = volume, y = price), data = diamonds) +
  geom_point()

table(diamonds$volume == 0)

# There was one extremely high outlier for volume. The graph of price by volume seems to generally be an exponential distribution, with some outliers of high price but very low volume.

clipped_volumes <- subset(diamonds, volume > 0 & volume <= 800)
cor.test(clipped_volumes$volume, clipped_volumes$price)$estimate

ggplot(aes(x = volume, y = price), data =clipped_volumes) +
  geom_point(alpha = 1/100) +
  stat_smooth(method = "lm") +
  scale_y_continuous(limits = c(0,20000))

# A linear model is marginally helpful in predicting price with volume, but an exponential model would be better suited.

library(dplyr)
library(gridExtra)

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))


p1 <- ggplot(aes(x = clarity, y = mean_price), data = diamonds_mp_by_clarity) +
  geom_bar(stat = "identity")

p2 <- ggplot(aes(x = color, y = mean_price), data = diamonds_mp_by_color) +
  geom_bar(stat = "identity")

grid.arrange(p1, p2, ncol = 1)

#The mean price seems to be increasing as color moves from D to J. The mean price also seems to be generally decreasing as clarity moves from I1 to IF, with a peak mean price at SI2. These go against out intuition, since D is higher quality than J, and IF is higher quality than I1. Maybe there is another feature that is causing this trend for which we need to control. Maybe we shold look at the means across cut?

setwd("~/MOOCs/Udacity/R Data Science")
require(xlsx)
require(dplyr)
require(tidyr)

healthspending <- read.xlsx(file = "healthcarespendingperperson.xlsx", sheetIndex = "Data")
summary(healthspending)
tidyhs <- healthspending %>% gather(year, spending, X1995:X2010)
rm(healthspending)
names(tidyhs)[1] <- "country"

percentgovspendinghc <- read.xlsx(file = "percentgovspendinghealthcare.xlsx", sheetIndex = "Data")
tidypercenths <- percentgovspendinghc %>% gather(year, spendingperperson, X1995:X2010)
rm(percentgovspendinghc)
names(tidypercenths)[1] <- "country"

total <- merge(tidyhs, tidypercenths ,by=c("country","year"))
names(total)[3:4] <- c("spendingperperson","percent_spending")

total$year <- as.numeric(substring(total$year, 2, 5))

require(ggplot2)
ggplot(aes(x = spendingperperson, y = percent_spending), data = subset(total, !is.na(spendingperperson))) +
        geom_point(alpha = 1/10)

cor.test(total$spending, total$percent_spending, method = "pearson")$estimate

## Military Spending

militaryspending <- read.xlsx(file = "militaryspendingpercentgdp.xlsx", sheetIndex = "Data")
tidyms <- militaryspending %>% gather(year, spending, X1988:X2011)
rm(militaryspending)
names(tidyms)[1] <- "country"

militarylabor <- read.xlsx(file = "militarypercentoflabor.xlsx", sheetIndex = "Data")
tidyml <- militarylabor %>% gather(year, militarylabor, X1985:X2010)
rm(militarylabor)
names(tidyml)[1] <- "country"

military <- merge(tidyms, tidyml, by=c("country","year"))
names(military)[3:4] <- c("militarypercentgdp","militarypercentoflabor")
rm(tidyml, tidyms, tidyhs, tidypercenths)

require(ggplot2)
ggplot(aes(x = militarypercentgdp, y = militarypercentoflabor), data = military) +
        geom_point(alpha = 1/10) +
        scale_x_continuous(breaks = seq(0,100,5)) +
        coord_cartesian(xlim = c(0,30), ylim = c(0,20))

military$year <- as.numeric(substring(military$year, 2, 5))

cor.test(military$militarypercentgdp, military$militarypercentoflabor)

ggplot(aes(x = year, y = militarypercentgdp, color = country), data = military) +
        geom_line(show.legend = FALSE) +
        scale_y_continuous(breaks = seq(0,120,5))

ggplot(aes(x = year, y = militarypercentoflabor, color = country), data = military) +
        geom_line(show.legend = FALSE) +
        scale_y_continuous(breaks = seq(0,120,5))
