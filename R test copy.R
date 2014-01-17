## clear workspace
rm(list = ls (all = TRUE))

install.packages ('foreign')
install.packages ('MASS')
install.packages ('xtable')
install.packages ('apsrtable')
install.packages ('arm')


load( )


summary (data)
table (data$Industry)
table (data$Country)
truehist (data$interlocks)
lines (density (data$interlocks, na.rm = T))
rug (data$interlocks)


plot(data$assets, data$interlocks)
lines (loess.smooth(data$Assets, data$interlocks)), 
col= "red")

boxplot (data$interlocks ~ sata$Industry)
boxplot (data$interlocks ~ data$country)

pairs (interlocks ~ assets + as.factor (Industry) + as.factor (Country) data = data)

### MODEL

# regular linear model

m1<- glm (Interlocks ~ Assets + as.factor (Industry) + as.factor (country), 
         data = data, family = gaussian (link = "Identity"))

library(foreign)
library(MASS)
library ()
library (arm)
librart


library(ggplot2)
library(reshape2)
library(plyr)
library(stargazer)

#------------
# Read data
#------------
medals <- read.csv("http://andhs.co/olympics")

#----------------
# Medal winners
#----------------
# Age of medal winners




#--------------
# Gold medals
#--------------
# Top gold-winning countries
top.gold <- ddply(medals, ~ Country, summarize, Gold=sum(Gold))

# Sort countries by number of gold medals
top.gold <- top.gold[order(top.gold$Gold, decreasing=TRUE), ]
top.gold$Country <- factor(top.gold$Country, levels=top.gold$Country, ordered=TRUE)

# Plot
fig2 <- ggplot(top.gold[1:10,], aes(x=Country, y=Gold))
fig2 <- fig2 + geom_bar(stat="identity") + labs(x=NULL, y="Gold Medals", title="Gold Medals (2000-2012)\n")
ggsave(plot=fig2, filename="figure_2.pdf", width=7, height=5, units="in")
