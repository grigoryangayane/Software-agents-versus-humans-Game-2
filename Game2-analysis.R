# Install and call required packages
install.packages(c("tidyverse", "ggplot2", "scales", "psych", "patchwork"))
library(ggplot2)
library(tidyverse)
library(scales)
library(psych)
library(patchwork) # for ggplot to combine the plots

# Game 5 output
game5Data <- subset(read.csv(file = 'EXPERIMENT-Game-5-output.csv'), ID != 0)
newdataG5 <- subset(game5Data, players_turn == 2 & decision_consistency == 2 & coalition_suggestion == 2,
                    select = c(ID, round_num))

################### Histograms
p1 <- ggplot(newdataG5, aes(x = round_num)) + 
  geom_histogram(color="lightblue", fill="#293352")+
  scale_x_continuous(limits = c(0, 100)) +
  #scale_x_continuous(trans = 'log2') + 
  xlab("Number of rounds") +
  ylab("Count")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill= NA))

p2 <- ggplot(newdataG5, aes(x = round_num)) + 
  geom_histogram(color="lightblue", fill="#293352")+
  scale_x_continuous(trans = 'log2') + 
  xlab("Number of rounds") +
  ylab("Count")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill= NA))

p1 + p2 # combine the figures

# Comparing CDF of CP with in_cp
CP <- game5Data %>%
  group_by(ID) %>%
  filter(CP == -1) %>%
  summarize(count = n())
#summarize(round = min(round_num))
CP$type <- "Coalition partition" 
qqnorm(CP$count)
qqline(CP$count, lty =2)

in_cp <- game5Data %>%
  group_by(ID) %>%
  filter(in_cp == -1) %>%
  summarize(count = n())
in_cp$type <- "Core coalition" 
qqnorm(in_cp$count)
qqline(in_cp$count, lty =2)

dfF <- rbind(CP, in_cp)

ggplot(dfF, aes(x = count, color = type)) + 
  stat_ecdf() + 
  xlab("Round numbers") +
  ylab("Cumulative probability")+
  #labs(fill = "Coalition type")+
  scale_x_continuous(trans = 'log2', limits = c(1, 100)) +
  #scale_x_continuous(limits = c(1, 100)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = c("#AE4031", "#4631AE")) +
  #theme_classic(panel.border(colour = "black", fill =NA, size = 5))
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill= NA),
        legend.position = c(0.8, 0.15),
        legend.title = element_blank())


####### continue from here
bar_data5 <- read.csv(file = 'G2_cum_percent.csv')
View(bar_data5)
write.table(bar_data5, file = "chisq_Game5.csv", sep = ",", quote = FALSE, row.names = F) 
df <- gather(bar_data5, key = "category", value = "value", c("ï..Agents", "Humans"))
df$category[df$category=="ï..Agents"] <- "Cummulative % agents"
df$category[df$category=="Humans"] <- "Cummulative % humans"
df$id = c(1, 2, 3, 4, 1, 2, 3, 4)
View(df)

# Comparing agents with bots for Game2
ggplot(df, aes(x = id, y = value, fill = category)) +
  geom_bar(stat = "identity", position = 'dodge', width = 0.35) +
  #geom_line()
  ylab("Cumulative percentages") +
  xlab("Number of rounds") +
  geom_hline(aes(yintercept = -Inf), color = "black") +
  scale_x_continuous(breaks = round(seq(min(df$id), max(df$id), by = 1), 1)) +
  scale_y_continuous(breaks = round(seq(0, 90, by = 10))) +
  scale_fill_manual(values = c("#293352", "#00AFBB"))+ 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", size=1, fill= NA),
        legend.position = c(0.15, 0.87),
        legend.text = element_text(size = 8),
        legend.title = element_blank())


# statistical test of significance
test5 <- chisq.test(table(bar_data5$Humans, bar_data5$ï..Agents))
test5

# Monte-Carlo simulation runs
chisq.test(cbind(bar_data5$Humans, bar_data5$ï..Agents),sim=TRUE,B=20000)
?chisq.test


# Extra analysis
# The data that contains all numbers except for -1
newdataG5less1 <- subset(game5Data, in_cp != -1,
                         select = c(ID, round_num, CP, in_cp))
newdataG5 <- newdataG5 %>% arrange(round_num)



