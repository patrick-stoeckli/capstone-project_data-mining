library(tidyverse)
Parteien <- c("Grüne","SP","GLP","BD","CVP","FDP","SVP")

#########################################################################
#########################################################################

######RQ1: Positive or negative - National Council###### 

load(file = 'data/data_processed/speeches_NR_rq1.Rdata')

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

speeches_NR_rq1_2 <- data.frame(matrix(ncol = length(Parteien), nrow = length(Parteien)))
colnames(speeches_NR_rq1_2) <- Parteien
rownames(speeches_NR_rq1_2) <- Parteien

for (MotionParlGroupName in Parteien){
  for (SpeakerParlGroupName in Parteien){
    speeches_NR_rq1_2[SpeakerParlGroupName,MotionParlGroupName] <- mean(speeches_NR_rq1$npvalue[speeches_NR_rq1$MotionParlGroupName==MotionParlGroupName&speeches_NR_rq1$SpeakerParlGroupName==SpeakerParlGroupName])
  }
}

speeches_NR_rq1_3 <- speeches_NR_rq1_2 %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname) %>%
  rename(MotionParlGroupName = colname) %>%
  rename(SpeakerParlGroupName = rowname) %>%
  rename(npvalue = value) 

speeches_NR_rq1_3_sorted <- speeches_NR_rq1_3 %>%
  mutate(MotionParlGroupName = factor(MotionParlGroupName, levels = Parteien),
         SpeakerParlGroupName = factor(SpeakerParlGroupName, levels = rev(Parteien))) %>%
  arrange(SpeakerParlGroupName, MotionParlGroupName)

speeches_NR_rq1_plot <- ggplot(speeches_NR_rq1_3_sorted, aes(x = MotionParlGroupName, y = SpeakerParlGroupName, fill = npvalue)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0, limits = c(-1, 1)) +
  labs(title = "A Heatmap of Parlamentarian Speeches", subtitle = "(National Council, n=1615)") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  xlab("parliamentary group of the initiator") +
  ylab("parliamentary group of the speaker")

ggsave('output/plots/speeches_NR_rq1.png', speeches_NR_rq1_plot, width = 10, height = 8)

#########################################################################

######RQ1: Positive or negative - Council of States ###### 

load(file = 'data/data_processed/speeches_SR_rq1.Rdata')

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

speeches_SR_rq1_2 <- data.frame(matrix(ncol = length(Parteien), nrow = length(Parteien)))
colnames(speeches_SR_rq1_2) <- Parteien
rownames(speeches_SR_rq1_2) <- Parteien

for (MotionParlGroupName in Parteien){
  for (SpeakerParlGroupName in Parteien){
    speeches_SR_rq1_2[SpeakerParlGroupName,MotionParlGroupName] <- mean(speeches_SR_rq1$npvalue[speeches_SR_rq1$MotionParlGroupName==MotionParlGroupName&speeches_SR_rq1$SpeakerParlGroupName==SpeakerParlGroupName])
  }
}

speeches_SR_rq1_3 <- speeches_SR_rq1_2 %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname) %>%
  rename(MotionParlGroupName = colname) %>%
  rename(SpeakerParlGroupName = rowname) %>%
  rename(npvalue = value) 

speeches_SR_rq1_3_sorted <- speeches_SR_rq1_3 %>%
  mutate(MotionParlGroupName = factor(MotionParlGroupName, levels = Parteien),
         SpeakerParlGroupName = factor(SpeakerParlGroupName, levels = rev(Parteien))) %>%
  arrange(SpeakerParlGroupName, MotionParlGroupName)

speeches_SR_rq1_plot <- ggplot(speeches_SR_rq1_3_sorted, aes(x = MotionParlGroupName, y = SpeakerParlGroupName, fill = npvalue)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0, limits = c(-1, 1)) +
  labs(title = "A Heatmap of Parlamentarian Speeches", subtitle = "(Council of States, n=1252)") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  theme(legend.title = element_blank()) +
  xlab("parliamentary group of the initiator") +
  ylab("parliamentary group of the speaker")

ggsave('output/plots/speeches_SR_rq1.png', speeches_SR_rq1_plot, width = 10, height = 8)

#########################################################################
#########################################################################

######RQ2: Left or right and conservative or liberal - National Council###### 

load(file = 'data/data_processed/speeches_NR_rq2.Rdata')

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

speeches_NR_rq2_sorted <- speeches_NR_rq2 %>%
  mutate(SpeakerParlGroupName = factor(SpeakerParlGroupName, levels = Parteien)) %>%
  arrange(SpeakerParlGroupName)

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

ggsave('output/plots/speeches_NR_rq2.png', speeches_NR_rq2_plot, width = 10, height = 8)

#########################################################################

######RQ2: Positive or negative - Council of States ###### 

load(file = 'data/data_processed/speeches_SR_rq2.Rdata')

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

speeches_SR_rq2_sorted <- speeches_SR_rq2 %>%
  mutate(SpeakerParlGroupName = factor(SpeakerParlGroupName, levels = Parteien)) %>%
  arrange(SpeakerParlGroupName)

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

ggsave('output/plots/speeches_SR_rq2.png', speeches_SR_rq2_plot, width = 10, height = 8)