
batting <- read.csv('C:\\Users\\srini\\Documents\\batting.csv')
str(batting)
print(head(batting['AB']))
print(head(batting['X2B']))

# Calculate batting Average 
# batting Average = Hits / At Bats
batting$BA <- batting$H / batting$AB
print(tail(batting$BA, 5))


# Calculate Singles (1B)

batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

# Calculate On Base Percentage.
# OBP = (Hits + Bases on Balls + Hit by Pitch)/(At Bat + Bases on Balls + Hit By Pitch + Sacrifice by fly)
# OBP = (H + BB + HBP) / (AB + BB + HBP + SF)
batting$OBP <- (batting$H + batting$BB + batting$HBP)/(batting$AB + batting$BB + batting$HBP + batting$SF)

print(head(batting$OBP))
# Calculate Slugging Percentage
# SLG = (1B) + (2 * 2B) + (3 * 3B) + (4 * HR)/AB
batting$SLG <- (batting$X1B + (2* batting$X2B) + (3 * batting$X3B) + (4 * batting$HR))/batting$AB

# Merging Salary Data with Batting Data
sal <- read.csv('C:\\Users\\srini\\Documents\\Salaries.csv')
batting <- subset(batting,yearID >= 1985)
bat_sal_combined <- merge(batting,sal,by=c('playerID','yearID'))
summary(bat_sal_combined)

# Build the data frame for Lost Players. All lost players were in 2001 off season. Reduce the data frame to reflect the baseball stats
lost_players <- subset(bat_sal_combined,playerID %in% c('giambja01','damonjo01','saenzol01'))
lost_players <- subset(lost_players,yearID == 2001)
lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]
summary(lost_players)
# Replacement Players
# Find Replacement Players for the key three players we lost! However, there are three constraints:
  
# 1. The total combined salary of the three players can not exceed 15 million dollars.
# 2. Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
# 3. Their mean OBP had to equal to or greater than the mean OBP of the lost players

Total_AB_LP <- sum(lost_players$AB)
mean_OBP_LP <- mean(lost_players$OBP)
avail_players <- subset(bat_sal_combined, yearID==2001)
print(head(avail_players))

avail_players <- filter(avail_players, salary<=5000000, OBP > 0, AB >= 500)
print(avail_players)

possible <- arrange(avail_players, desc(OBP))
possible <- possible[,c('playerID','OBP','AB','salary')]
possible <- subset(possible, playerID != c('giambja01','damonjo01','saenzol01'))
New_Players <- possible [1:3, ] 
print(New_Players)
