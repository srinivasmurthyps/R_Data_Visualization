library(ggplot2)
library(data.table)
library(ggthemes)

df <- fread('Economist_Assignment_Data.csv', drop=1)
pl <- ggplot(df, aes(x=CPI, y=HDI, color=Region)) + geom_point(size=6, pch=1) + geom_smooth(method=lm, formula = y ~ log(x), se = FALSE, color='red', aes(group=1))
pl2 = pl + geom_text(aes(label=Country))
pointsToLabel <- c('India', 'United States', 'Canada', 'China', 'Pakistan', 'Srilanka', 'Mexico', 'Finland', 'Germany', 'France', 'Belgium', 'Australia')
pl3 <- pl2 + geom_text(aes(label = Country), color = "gray20", 
                       data = subset(df, Country %in% pointsToLabel),check_overlap = TRUE)

pl4 <- pl3 + theme_bw() 
pl5 <- pl4 + scale_x_continuous(name='Corruption Perceptions Index', limits=c(0.9, 10.5), breaks = 1:10) 
pl6 <- pl5 + scale_y_continuous(name='Corruption Perceptions Index', limits=c(0.2, 1.0), breaks = 1:10) 
pl7 <- pl6 + ggtitle("Corruption and Human Development")
print(pl7)