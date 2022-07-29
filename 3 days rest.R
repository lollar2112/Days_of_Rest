install.packages('tidyverse')
library('tidyverse')

colnames(X1995_2021_Three_Days_Rest)
X1995_2021_Three_Days_Rest <- mutate(X95_21_Three_Days_Rest, New_IP = as.numeric(New_IP))

#Find out the averge amount of innings thrown by each team
mean(X95_21_Three_Days_Rest$IP)

#Average strikeouts per 9 innings
mean(X95_21_Three_Days_Rest$`SO/9`)
mean(X95_21_Four_Days_Rest$`SO/9`)
mean(X95_21_Five_Days_Rest$`SO/9`)
# 95-01 There are more strikeouts per nine innings for five days of rest than the others, but not by a large margin
mean(X20_41_Five_days_rest$`SO/9`)
mean(X20_41_Four_Days_Rest$`SO/9`)
mean(X20_41_Three_Days_Rest$`SO/9`)

mean(X1995_2021_Three_Days_Rest$ER)

#We do math to find the totals for Innings and earned runs for 3, 4 and 5 days of rest
a <- sum(X95_21_Three_Days_Rest$New_IP)
b <- sum(X95_21_Three_Days_Rest$ER)
a4 <- sum(X95_21_Four_Days_Rest$New_IP)
b4 <- sum(X95_21_Four_Days_Rest$ER)
a5 <- sum(X95_21_Five_Days_Rest$New_IP)
b5 <- sum(X95_21_Five_Days_Rest$ER)
a6 <- sum(X95_21_Six_Days_Rest$New_IP)
b6 <- sum(X95_21_Six_Days_Rest$ER)

X3d <- sum(X20_41_Three_Days_Rest$New_IP)
x4d <- sum(X20_41_Four_Days_Rest$New_IP)
x5d <- sum(X20_41_Five_days_rest$New_IP)
x3der <- sum(X20_41_Three_Days_Rest$ER)
X4der <- sum(X20_41_Four_Days_Rest$ER)
x5der <- sum(X20_41_Five_days_rest$ER)

# Now calculating the overall ERA for the Three days
c <- b * 9 
New_ERA <- c/a

c4 <- b4 *9
New_ERA_4 <- c4/a4

c5 <- b5 *9
New_ERA_5 <- c5/a5

c6 <- b6 *9
New_ERA_6 <- c6/a6

# 95-21 The more days of rest the lower the ERA, though the difference in four or five days isn't significant

x3d_ert <- x3der * 9
x3dera <- x3d_ert/X3d

x4d_ert <- X4der * 9
x4dera <- x4d_ert/x4d

x5d_ert <- x5der * 9
x5dera <- x5d_ert/x5d

# Find the total number of starts
starts_tot <- sum(X95_21_Three_Days_Rest$G)
starts_tot_4 <- sum(X95_21_Four_Days_Rest$G)
starts_tot_5 <- sum(X95_21_Five_Days_Rest$G)
starts_tot_6 <- sum(X95_21_Six_Days_Rest$G)
# 1995-2021 The vast majority of the starts occur with a pitcher on Four days of rest
x3dgs <- sum(X20_41_Three_Days_Rest$G)
x4dgs <- sum(X20_41_Four_Days_Rest$G)
x5dgs <- sum(X20_41_Five_days_rest$G)
# 1920-1941 Most starts were made on Three Days rest, followed closely by Four and Farther by Five


# Average innings per start
avg_ip <- a/starts_tot
avg_ip_4 <- a4/starts_tot_4
avg_ip_5 <- a5/starts_tot_5
avg_ip_6 <- a6/starts_tot_6
# 1995-2021 The average innings pitched per start drops almost a full inning when on Three Days Rest.  Six was second shortest
x3d_avgip <- X3d/x3dgs
x4d_avgip <- x4d/x4dgs
x5d_avgip <- x5d/x5dgs
# 1920-1941 There was not much difference in average innings per start.  Five days very slightly lower


# Average strikeouts per 9
so_4d <- sum(X95_21_Four_Days_Rest$SO)*9
so_5d <- sum(X95_21_Five_Days_Rest$SO)*9
so_3d <- sum(X95_21_Three_Days_Rest $SO)*9
so_6d <- sum(X95_21_Six_Days_Rest$SO)*9

so_per_3 <- so_3d/a
so_per_4 <- so_4d/a4
so_per_5 <- so_5d/a5
so_per_6 <- so_6d/a6

#1995-2021 Fewer strikouts per 9 innings on three days rest, though the numbers aren't significant across the board
x3d_ks <- sum(X20_41_Three_Days_Rest$SO)*9
x4d_ks <- sum(X20_41_Four_Days_Rest$SO)*9
x5d_ks <- sum(X20_41_Five_days_rest$SO)*9

x3d_so_per_3 <- x3d_ks/X3d
x4d_so_per_4 <- x4d_ks/x4d
x5d_so_per_5 <- x5d_ks/x5d


#Looking at BB per 9
bb_3d <- sum(X95_21_Three_Days_Rest$BB...18)*9
bb_4d <- sum(X95_21_Four_Days_Rest$BB...18)*9
bb_5d <- sum(X95_21_Five_Days_Rest$BB...17)*9
bb_6d <- sum(X95_21_Six_Days_Rest$BB...17)*9

bb_per_3 <- bb_3d/a
bb_per_4 <- bb_4d/a4
bb_per_5 <- bb_5d/a5
bb_per_6 <- bb_6d/a6

x3d_tbb <- sum(X20_41_Three_Days_Rest$BB...17)*9
x4d_tbb <- sum(X20_41_Four_Days_Rest$BB...17)*9
x5d_tbb <- sum(X20_41_Five_days_rest$BB...17)*9

x3d_bb_per9 <- x3d_tbb/X3d
x4d_bb_per9 <- x4d_tbb/x4d
x5d_bb_per9 <- x5d_tbb/x5d

#95-21 Many more walks per nine innings on three days rest.  Four and Five almost no difference.  Six is higher
# 20-41 Not much difference.  Very, Very slightly higher walks per nine innings for starters on five days rest

ggplot(data = X1995_2021_Three_Days_Rest) + 
  geom_bar(mapping = aes(x = ERA, y = Team))

ggplot(data = X95_21_Three_Days_Rest, aes(x = ERA, y = Team)) + geom_bar(stat = 'identity')
ggplot(data = Modern, aes(x = era, y = ip)) + geom_bar(stat = 'identity')+labs(x="ERA", y="Average IP per start",
                                                                               title= "ERA and Innings Per Start
                                                                               1995-2021")

#I'm putting all the values into one dataframe
data <- data.frame(a, a4, a5, a6, avg_ip, avg_ip_4, avg_ip_5, avg_ip_6, avgip, b, b4, b5, b6, bb_3d, bb_4d, bb_5d,
                   bb_6d, bb_per_3, bb_per_4, bb_per_5, bb_per_6, c, c4, c5, c6, New_ERA, New_ERA_4, New_ERA_5,
                   New_ERA_6, so_3d, so_4d, so_5d, so_6d, so_per_3, so_per_4, so_per_5, so_per_6, starts_tot,
                   starts_tot_4, starts_tot_5, starts_tot_6, X3d, x3d_avgip, x3d_bb_per9, x3d_ert, x3d_ks,
                   x3d_so_per_3, x3d_tbb, x3der,x3dera, x4dera, x3dgs, x4d, x4d, avgip, x4d_bb_per9, x4d_ert,
                   x4d_ks, x4d_so_per_4, x4d_tbb, X4der, x4dera, x4dgs, x5d, x5d_avgip, x5d_bb_per9,
                   x5d_ert, x5d_ks, x5d_so_per_5, x5d_tbb, x5der, x5dgs, x5dera, era_20_41, era_95_21)

Modern_3_Days <- data.frame(a, avg_ip, b, bb_3d, bb_per_3, c, New_ERA, so_per_3, starts_tot)
Modern_4_Days <- data.frame(a4, avg_ip_4, b4, bb_per_4, New_ERA_4, so_per_4, starts_tot_4)
Modern_5_Days <- data.frame(a5, avg_ip_5, b5, bb_per_5, New_ERA_5, so_per_5, starts_tot_5)
Modern_6_Days <- data.frame(a6, avg_ip_6, b6, bb_per_6, New_ERA_6, so_per_6, starts_tot_6)

Modern <- data.frame(era =c(New_ERA, New_ERA_4, New_ERA_5, New_ERA_6),
                     so =c(so_per_3, so_per_4, so_per_5, so_per_6),
                     bb =c(bb_per_3, bb_per_4, bb_per_5, bb_per_6),
                     ip =c(avg_ip, avg_ip_4, avg_ip_5, avg_ip_6))

Old <- data.frame(era =c(x3dera, x4dera, x5dera),
                  so =c(x3d_so_per_3,x4d_so_per_4,x5d_so_per_5),
                  bb =c(x3d_bb_per9, x4d_bb_per9,x5d_bb_per9),
                  ip =c(x3d_avgip, x4d_avgip, x5d_avgip))

Old_3_Days <- data.frame(x3d_avgip,x3d_bb_per9,x3dera,x3d_so_per_3)
Old_4_Days <- data.frame(x4d_avgip, x4d_bb_per9, x4d_so_per_4,x4dera)
Old_5_Days <- data.frame(x5d_avgip, x5d_bb_per9, x5d_so_per_5, x5dera)


era_95_21 <- data.frame(ERA_3_Days, ERA_4_Days, ERA_5_Days, ERA_6_Days)
era_20_41 <- data.frame(ERA_Old_3_days, ERA_Old_4_days, ERA_Old_5_days)

ERA_Old_3_days <- x3dera
ERA_Old_4_days <- x4dera
ERA_Old_5_days <- x5dera

ERA_3_Days <- New_ERA
ERA_4_Days <- New_ERA_4
ERA_5_Days <- New_ERA_5
ERA_6_Days <- New_ERA_6

IP_per_New <- data.frame(New_3_IP, New_4_IP, New_5_IP, New_6_IP)
  
New_3_IP <- avg_ip
New_4_IP <- avg_ip_4
New_5_IP <- avg_ip_5
New_6_IP <- avg_ip_6

data %>%
  ggplot(mapping = aes(x = era_95_21))+
  geom_bar(fill = "blue", "red", "green", "orange")+
  theme_bw()+
  labs(x = "Days of Rest",
       y = NULL,
       title = "ERA by Days of Rest From 1995-2021")


traceback()




