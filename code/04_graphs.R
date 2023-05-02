#Load the package 'tidyverse'
library(tidyverse)

#Define a party-vector used for the position of the party in the plots
Parteien <- c("Grüne","SP","GLP","BD","CVP","FDP","SVP")

#########################################################################
#########################################################################

######RQ1: Positive or negative - National Council###### 

#Load the corresponding data
load(file = 'data/data_processed/speeches_NR_rq1.Rdata')

#Restructure and rename the data 
speeches_NR_rq1 <- speeches_NR_rq1 %>% 
  select(ParlGroupName,SpeakerParlGroupName, npvalue) %>% 
  rename(MotionParlGroupName = ParlGroupName) %>%
  filter(is.na(MotionParlGroupName) == FALSE) %>%   
  mutate(MotionParlGroupName = ifelse(MotionParlGroupName == "Die Mitte-Fraktion. Die Mitte. EVP.", "CVP", 
                                      ifelse(MotionParlGroupName == "Grünliberale Fraktion", "GLP", 
                                             ifelse(MotionParlGroupName == "Grüne Fraktion", "Grüne",      
                                                    ifelse(MotionParlGroupName == "Fraktion der Schweizerischen Volkspartei", "SVP",
                                                           ifelse(MotionParlGroupName == "FDP-Liberale Fraktion", "FDP",
                                                                  ifelse(MotionParlGroupName == "Sozialdemokratische Fraktion", "SP",
                                                                         ifelse(MotionParlGroupName == "Fraktion BD", "BD",
                                                                                MotionParlGroupName)))))))
  ) %>% 
  mutate(SpeakerParlGroupName = ifelse(SpeakerParlGroupName == "CVP-Fraktion", "CVP", 
                                       ifelse(SpeakerParlGroupName == "Grünliberale Fraktion", "GLP", 
                                              ifelse(SpeakerParlGroupName == "Grüne Fraktion", "Grüne",      
                                                     ifelse(SpeakerParlGroupName == "Fraktion der Schweizerischen Volkspartei", "SVP",
                                                            ifelse(SpeakerParlGroupName == "FDP-Liberale Fraktion", "FDP",
                                                                   ifelse(SpeakerParlGroupName == "Sozialdemokratische Fraktion", "SP",
                                                                          ifelse(SpeakerParlGroupName == "Fraktion BD", "BD",
                                                                                 SpeakerParlGroupName)))))))
  ) 

#Create an empty matrix to be filled with the mean values in the next step
speeches_NR_rq1_2 <- data.frame(matrix(ncol = length(Parteien), nrow = length(Parteien)))
colnames(speeches_NR_rq1_2) <- Parteien
rownames(speeches_NR_rq1_2) <- Parteien

#Calculate the mean for every combination
for (MotionParlGroupName in Parteien){
  for (SpeakerParlGroupName in Parteien){
    speeches_NR_rq1_2[SpeakerParlGroupName,MotionParlGroupName] <- mean(speeches_NR_rq1$npvalue[speeches_NR_rq1$MotionParlGroupName==MotionParlGroupName&speeches_NR_rq1$SpeakerParlGroupName==SpeakerParlGroupName])
  }
}

#Restructure the data
speeches_NR_rq1_3 <- speeches_NR_rq1_2 %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname) %>%
  rename(MotionParlGroupName = colname) %>%
  rename(SpeakerParlGroupName = rowname) %>%
  rename(npvalue = value) 

#Sort and arrange the data such that the order in the plot corresponds to its political position 
speeches_NR_rq1_3_sorted <- speeches_NR_rq1_3 %>%
  mutate(MotionParlGroupName = factor(MotionParlGroupName, levels = Parteien),
         SpeakerParlGroupName = factor(SpeakerParlGroupName, levels = rev(Parteien))) %>%
  arrange(SpeakerParlGroupName, MotionParlGroupName)

#Generate a heatmap-plot
speeches_NR_rq1_plot <- ggplot(speeches_NR_rq1_3_sorted, aes(x = MotionParlGroupName, y = SpeakerParlGroupName, fill = npvalue)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0, limits = c(-1, 1)) +
  labs(title = "A Heatmap of Parlamentarian Speeches", subtitle = "(National Council, n=1615)") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  xlab("parliamentary group of the initiator") +
  ylab("parliamentary group of the speaker")

#Save the plot
ggsave('output/plots/speeches_NR_rq1.png', speeches_NR_rq1_plot, width = 10, height = 8)

#########################################################################

######RQ1: Positive or negative - Council of States ###### 

#Load the corresponding data
load(file = 'data/data_processed/speeches_SR_rq1.Rdata')

#Restructure and rename the data 
speeches_SR_rq1 <- speeches_SR_rq1 %>% 
  select(ParlGroupName,SpeakerParlGroupName, npvalue) %>% 
  rename(MotionParlGroupName = ParlGroupName) %>%
  filter(is.na(MotionParlGroupName) == FALSE) %>%   
  mutate(MotionParlGroupName = ifelse(MotionParlGroupName == "Die Mitte-Fraktion. Die Mitte. EVP.", "CVP", 
                                      ifelse(MotionParlGroupName == "Grünliberale Fraktion", "GLP", 
                                             ifelse(MotionParlGroupName == "Grüne Fraktion", "Grüne",      
                                                    ifelse(MotionParlGroupName == "Fraktion der Schweizerischen Volkspartei", "SVP",
                                                           ifelse(MotionParlGroupName == "FDP-Liberale Fraktion", "FDP",
                                                                  ifelse(MotionParlGroupName == "Sozialdemokratische Fraktion", "SP",
                                                                         ifelse(MotionParlGroupName == "Fraktion BD", "BD",
                                                                                MotionParlGroupName)))))))
  ) %>% 
  mutate(SpeakerParlGroupName = ifelse(SpeakerParlGroupName == "CVP-Fraktion", "CVP", 
                                       ifelse(SpeakerParlGroupName == "Grünliberale Fraktion", "GLP", 
                                              ifelse(SpeakerParlGroupName == "Grüne Fraktion", "Grüne",      
                                                     ifelse(SpeakerParlGroupName == "Fraktion der Schweizerischen Volkspartei", "SVP",
                                                            ifelse(SpeakerParlGroupName == "FDP-Liberale Fraktion", "FDP",
                                                                   ifelse(SpeakerParlGroupName == "Sozialdemokratische Fraktion", "SP",
                                                                          ifelse(SpeakerParlGroupName == "Fraktion BD", "BD",
                                                                                 SpeakerParlGroupName)))))))
  ) 

#Create an empty matrix to be filled with the mean values in the next step
speeches_SR_rq1_2 <- data.frame(matrix(ncol = length(Parteien), nrow = length(Parteien)))
colnames(speeches_SR_rq1_2) <- Parteien
rownames(speeches_SR_rq1_2) <- Parteien

#Calculate the mean for every combination
for (MotionParlGroupName in Parteien){
  for (SpeakerParlGroupName in Parteien){
    speeches_SR_rq1_2[SpeakerParlGroupName,MotionParlGroupName] <- mean(speeches_SR_rq1$npvalue[speeches_SR_rq1$MotionParlGroupName==MotionParlGroupName&speeches_SR_rq1$SpeakerParlGroupName==SpeakerParlGroupName])
  }
}

#Restructure the data
speeches_SR_rq1_3 <- speeches_SR_rq1_2 %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname) %>%
  rename(MotionParlGroupName = colname) %>%
  rename(SpeakerParlGroupName = rowname) %>%
  rename(npvalue = value) 

#Sort and arrange the data such that the order in the plot corresponds to its political position 
speeches_SR_rq1_3_sorted <- speeches_SR_rq1_3 %>%
  mutate(MotionParlGroupName = factor(MotionParlGroupName, levels = Parteien),
         SpeakerParlGroupName = factor(SpeakerParlGroupName, levels = rev(Parteien))) %>%
  arrange(SpeakerParlGroupName, MotionParlGroupName)

#Generate a heatmap-plot
speeches_SR_rq1_plot <- ggplot(speeches_SR_rq1_3_sorted, aes(x = MotionParlGroupName, y = SpeakerParlGroupName, fill = npvalue)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0, limits = c(-1, 1)) +
  labs(title = "A Heatmap of Parlamentarian Speeches", subtitle = "(Council of States, n=1252)") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  xlab("parliamentary group of the initiator") +
  ylab("parliamentary group of the speaker")

#Save the plot
ggsave('output/plots/speeches_SR_rq1.png', speeches_SR_rq1_plot, width = 10, height = 8)

#########################################################################
#########################################################################

######RQ2: Left or right and conservative or liberal - National Council###### 

#Load the corresponding data
load(file = 'data/data_processed/speeches_NR_rq2.Rdata')

#Restructure and rename the data 
speeches_NR_rq2 <- speeches_NR_rq2 %>% 
  select(ParlGroupName,SpeakerParlGroupName, lrvalue, clvalue) %>% 
  rename(MotionParlGroupName = ParlGroupName) %>%
  filter(is.na(MotionParlGroupName) == FALSE) %>%   
  mutate(MotionParlGroupName = ifelse(MotionParlGroupName == "Die Mitte-Fraktion. Die Mitte. EVP.", "CVP", 
                                      ifelse(MotionParlGroupName == "Grünliberale Fraktion", "GLP", 
                                             ifelse(MotionParlGroupName == "Grüne Fraktion", "Grüne",      
                                                    ifelse(MotionParlGroupName == "Fraktion der Schweizerischen Volkspartei", "SVP",
                                                           ifelse(MotionParlGroupName == "FDP-Liberale Fraktion", "FDP",
                                                                  ifelse(MotionParlGroupName == "Sozialdemokratische Fraktion", "SP",
                                                                         ifelse(MotionParlGroupName == "Fraktion BD", "BD",
                                                                                MotionParlGroupName)))))))
  ) %>% 
  mutate(SpeakerParlGroupName = ifelse(SpeakerParlGroupName == "CVP-Fraktion", "CVP", 
                                       ifelse(SpeakerParlGroupName == "Grünliberale Fraktion", "GLP", 
                                              ifelse(SpeakerParlGroupName == "Grüne Fraktion", "Grüne",      
                                                     ifelse(SpeakerParlGroupName == "Fraktion der Schweizerischen Volkspartei", "SVP",
                                                            ifelse(SpeakerParlGroupName == "FDP-Liberale Fraktion", "FDP",
                                                                   ifelse(SpeakerParlGroupName == "Sozialdemokratische Fraktion", "SP",
                                                                          ifelse(SpeakerParlGroupName == "Fraktion BD", "BD",
                                                                                 SpeakerParlGroupName)))))))
  ) 

#Sort and arrange the data  
speeches_NR_rq2_sorted <- speeches_NR_rq2 %>%
  mutate(SpeakerParlGroupName = factor(SpeakerParlGroupName, levels = Parteien)) %>%
  arrange(SpeakerParlGroupName)

#Generate a political map
speeches_NR_rq2_plot <- ggplot(speeches_NR_rq2_sorted, aes(lrvalue, clvalue, colour = SpeakerParlGroupName)) + 
  geom_point(position = position_jitter(width = 0.1, height = 0)) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(title = "A political Map of Parlamentarian Speeches (National Council, n=1615)", subtitle = "liberal", x = "conservative") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),  #remove y axis ticks
        legend.title = element_blank()
  ) +
  scale_y_continuous(name = "left",sec.axis = sec_axis( trans=~.*1, name="right")) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_color_manual(values = c("green", "red", "lightgreen", "yellow" ,"orange", "blue","darkgreen")) 

#Save the plot
ggsave('output/plots/speeches_NR_rq2.png', speeches_NR_rq2_plot, width = 10, height = 8)

#########################################################################

######RQ2: Positive or negative - Council of States ###### 

#Load the corresponding data
load(file = 'data/data_processed/speeches_SR_rq2.Rdata')

#Restructure and rename the data
speeches_SR_rq2 <- speeches_SR_rq2 %>% 
  select(ParlGroupName,SpeakerParlGroupName, lrvalue, clvalue) %>% 
  rename(MotionParlGroupName = ParlGroupName) %>%
  filter(is.na(MotionParlGroupName) == FALSE) %>%   
  mutate(MotionParlGroupName = ifelse(MotionParlGroupName == "Die Mitte-Fraktion. Die Mitte. EVP.", "CVP", 
                                      ifelse(MotionParlGroupName == "Grünliberale Fraktion", "GLP", 
                                             ifelse(MotionParlGroupName == "Grüne Fraktion", "Grüne",      
                                                    ifelse(MotionParlGroupName == "Fraktion der Schweizerischen Volkspartei", "SVP",
                                                           ifelse(MotionParlGroupName == "FDP-Liberale Fraktion", "FDP",
                                                                  ifelse(MotionParlGroupName == "Sozialdemokratische Fraktion", "SP",
                                                                         ifelse(MotionParlGroupName == "Fraktion BD", "BD",
                                                                                MotionParlGroupName)))))))
  ) %>% 
  mutate(SpeakerParlGroupName = ifelse(SpeakerParlGroupName == "CVP-Fraktion", "CVP", 
                                       ifelse(SpeakerParlGroupName == "Grünliberale Fraktion", "GLP", 
                                              ifelse(SpeakerParlGroupName == "Grüne Fraktion", "Grüne",      
                                                     ifelse(SpeakerParlGroupName == "Fraktion der Schweizerischen Volkspartei", "SVP",
                                                            ifelse(SpeakerParlGroupName == "FDP-Liberale Fraktion", "FDP",
                                                                   ifelse(SpeakerParlGroupName == "Sozialdemokratische Fraktion", "SP",
                                                                          ifelse(SpeakerParlGroupName == "Fraktion BD", "BD",
                                                                                 SpeakerParlGroupName)))))))
  ) 

#Sort and arrange the data 
speeches_SR_rq2_sorted <- speeches_SR_rq2 %>%
  mutate(SpeakerParlGroupName = factor(SpeakerParlGroupName, levels = Parteien)) %>%
  arrange(SpeakerParlGroupName)

#Generate a political map
speeches_SR_rq2_plot <- ggplot(speeches_SR_rq2_sorted, aes(lrvalue, clvalue, colour = SpeakerParlGroupName)) + 
  geom_point(position = position_jitter(width = 0.1, height = 0)) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(title = "A political Map of Parlamentarian Speeches (Council of States, n=1252)", subtitle = "liberal", x = "conservative") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5),
        axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),  #remove y axis ticks
        legend.title = element_blank()
  ) +
  scale_y_continuous(name = "left",sec.axis = sec_axis( trans=~.*1, name="right")) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_color_manual(values = c("green", "red", "yellow" ,"orange", "blue","darkgreen"))  

#Save the plot
ggsave('output/plots/speeches_SR_rq2.png', speeches_SR_rq2_plot, width = 10, height = 8)

#########################################################################
#########################################################################

######RQ3: Complexity and formality - National Council ######

#Load the corresponding data
load(file = 'data/data_processed/speeches_NR_rq3.Rdata')

#Restructure and rename the data
speeches_NR_rq3 <- speeches_NR_rq3 %>% 
  filter(is.na(ParlGroupName) == FALSE) %>%  
  select(SpeakerParlGroupName, cvalue, fvalue) %>% 
  mutate(SpeakerParlGroupName = ifelse(SpeakerParlGroupName == "CVP-Fraktion", "CVP", 
                                       ifelse(SpeakerParlGroupName == "Grünliberale Fraktion", "GLP", 
                                              ifelse(SpeakerParlGroupName == "Grüne Fraktion", "Grüne",      
                                                     ifelse(SpeakerParlGroupName == "Fraktion der Schweizerischen Volkspartei", "SVP",
                                                            ifelse(SpeakerParlGroupName == "FDP-Liberale Fraktion", "FDP",
                                                                   ifelse(SpeakerParlGroupName == "Sozialdemokratische Fraktion", "SP",
                                                                          ifelse(SpeakerParlGroupName == "Fraktion BD", "BD",
                                                                                 SpeakerParlGroupName)))))))
  ) 

#Calculate the mean for every combination
speeches_NR_rq3_2 <- aggregate(speeches_NR_rq3[,2:3], by=list(SpeakerParlGroupName=speeches_NR_rq3$SpeakerParlGroupName), mean)

#Reshape the data
speeches_NR_rq3_2_long <- pivot_longer(speeches_NR_rq3_2, cols = c("cvalue", "fvalue"), names_to = "variable", values_to = "value") %>%
  mutate(SpeakerParlGroupName = factor(SpeakerParlGroupName, levels = Parteien)) %>%
  arrange(SpeakerParlGroupName)

#Generate a barplot
speeches_NR_rq3_plot <-  ggplot(speeches_NR_rq3_2_long, aes(x = SpeakerParlGroupName, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("mean value") +
  xlab("")+
  scale_fill_manual(values = c("lightblue", "pink"), name = "Variable",
                    labels = c("complexity", "formality")) +
  labs(title = "Complexity and Formality of Parliamentarien Speeches by Party", subtitle = "(National Council, n=1615)") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  ylim(0,10)

#Save the plot
ggsave('output/plots/speeches_NR_rq3.png', speeches_NR_rq3_plot, width = 10, height = 8)

#########################################################################

######RQ3: Complexity and formality - Council of States ###### 

#Load the corresponding data
load(file = 'data/data_processed/speeches_SR_rq3.Rdata')

#Restructure and rename the data
speeches_SR_rq3 <- speeches_SR_rq3 %>% 
  filter(is.na(ParlGroupName) == FALSE) %>%  
  select(SpeakerParlGroupName, cvalue, fvalue) %>% 
  mutate(SpeakerParlGroupName = ifelse(SpeakerParlGroupName == "CVP-Fraktion", "CVP", 
                                       ifelse(SpeakerParlGroupName == "Grünliberale Fraktion", "GLP", 
                                              ifelse(SpeakerParlGroupName == "Grüne Fraktion", "Grüne",      
                                                     ifelse(SpeakerParlGroupName == "Fraktion der Schweizerischen Volkspartei", "SVP",
                                                            ifelse(SpeakerParlGroupName == "FDP-Liberale Fraktion", "FDP",
                                                                   ifelse(SpeakerParlGroupName == "Sozialdemokratische Fraktion", "SP",
                                                                          ifelse(SpeakerParlGroupName == "Fraktion BD", "BD",
                                                                                 SpeakerParlGroupName)))))))
  ) 

#Calculate the mean for every combination
speeches_SR_rq3_2 <- aggregate(speeches_SR_rq3[,2:3], by=list(SpeakerParlGroupName=speeches_SR_rq3$SpeakerParlGroupName), mean)

#Reshape the data
speeches_SR_rq3_2_long <- pivot_longer(speeches_SR_rq3_2, cols = c("cvalue", "fvalue"), names_to = "variable", values_to = "value") %>%
  mutate(SpeakerParlGroupName = factor(SpeakerParlGroupName, levels = Parteien)) %>%
  arrange(SpeakerParlGroupName)

#Generate a barplot
speeches_SR_rq3_plot <-  ggplot(speeches_SR_rq3_2_long, aes(x = SpeakerParlGroupName, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylab("mean value") +
  xlab("")+
  scale_fill_manual(values = c("lightblue", "pink"), name = "Variable",
                    labels = c("complexity", "formality")) +
  labs(title = "Complexity and Formality of Parliamentarien Speeches by Party", subtitle = "(Council of States, n=1250)") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  ylim(0,10)

#Save the plot
ggsave('output/plots/speeches_SR_rq3.png', speeches_SR_rq3_plot, width = 10, height = 8)
