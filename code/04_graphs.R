df <- data.frame(matrix(ncol = 3, nrow = 500))
colnames(df) <- c("ParteiMotion", "ParteiVotum", "Wert")

Parteien <- c("Grüne","SP","GLP","CVP","FDP","SVP")

for (i in 1:500) {
  df$ParteiMotion[i] <- sample(Parteien,1)
  df$ParteiVotum[i] <- sample(Parteien,1)
  df$Wert[i] <- runif(1, -1, 1)
}

df2 <- data.frame(matrix(ncol = length(Parteien), nrow = length(Parteien)))
colnames(df2) <- Parteien
rownames(df2) <- Parteien

for (ParteiMotion in Parteien){
  for (ParteiVotum in Parteien){
  df2[ParteiVotum,ParteiMotion] <- mean(df$Wert[df$ParteiMotion==ParteiMotion&df$ParteiVotum==ParteiVotum])
  }
}

df3 <- df2 %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname) %>%
  rename(ParteiMotion = colname) %>%
  rename(ParteiVotum = rowname) %>%
  rename(Wert = value) 

# sort the data set by color in the defined order, then by fruit in the defined order
data_sorted <- df3 %>%
  mutate(ParteiMotion = factor(ParteiMotion, levels = Parteien),
         ParteiVotum = factor(ParteiVotum, levels = Parteien)) %>%
  arrange(ParteiVotum, ParteiMotion)

# print the sorted data set
print(data_sorted)

ggplot(data_sorted, aes(x = ParteiMotion, y = ParteiVotum, fill = Wert)) +
  geom_tile()

# add another plot example plot for the bias analysis


for (i in 1:500) {
  df$LeftRightValue[i] <- runif(1, -1, 1)
  df$ConservativeLiberalValue[i] <- runif(1, -1, 1)
}

df <- df %>%
  mutate(ParteiVotum = factor(ParteiVotum, levels = Parteien)) %>%
  arrange(ParteiVotum)

ggplot(df, aes(LeftRightValue, ConservativeLiberalValue, colour = ParteiVotum)) + 
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  labs(title = "A political map of parlamentarian speeches", subtitle = "liberal") +
  theme(plot.title = element_text(hjust = 0.5, face="bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()  #remove y axis ticks
        ) +
  xlab("conservative") +
  scale_y_continuous(name = "left",sec.axis = sec_axis( trans=~.*1, name="right")) +
  #ylab("left") +
  scale_color_manual(values = c("green", "red", "lightgreen", "orange", "blue","darkgreen")) +
  geom_point()

#load(file = 'data/data_raw/first_try.Rdata')
load(file = 'data/data_processed/speeches_NR_final.Rdata')

Parteien <- c("Grüne","SP","GLP","BD","CVP","FDP","SVP")

speeches_NR_final <- speeches_NR_final[1:1200,] 

speeches_NR_final <- speeches_NR_final %>% 
  #mutate(value = as.numeric(value)) %>% 
  mutate(value = str_replace_all(value, "[^0-9.-]", ""),
         value = as.numeric(value)) %>%
  select(ParlGroupName,SpeakerParlGroupName, value) %>% 
  filter(is.na(ParlGroupName) == FALSE) %>%   
  mutate(ParlGroupName = ifelse(ParlGroupName == "Die Mitte-Fraktion. Die Mitte. EVP.", "CVP", 
                          ifelse(ParlGroupName == "Grünliberale Fraktion", "GLP", 
                            ifelse(ParlGroupName == "Grüne Fraktion", "Grüne",      
                              ifelse(ParlGroupName == "Fraktion der Schweizerischen Volkspartei", "SVP",
                                ifelse(ParlGroupName == "FDP-Liberale Fraktion", "FDP",
                                  ifelse(ParlGroupName == "Sozialdemokratische Fraktion", "SP",
                                    ifelse(ParlGroupName == "Fraktion BD", "BD",
                                      ParlGroupName)))))))
         ) %>% 
  mutate(SpeakerParlGroupName = ifelse(SpeakerParlGroupName == "CVP-Fraktion", "CVP", 
                                  ifelse(SpeakerParlGroupName == "Grünliberale Fraktion", "GLP", 
                                    ifelse(SpeakerParlGroupName == "Grüne Fraktion", "Grüne",      
                                      ifelse(SpeakerParlGroupName == "Fraktion der Schweizerischen Volkspartei", "SVP",
                                        ifelse(SpeakerParlGroupName == "FDP-Liberale Fraktion", "FDP",
                                          ifelse(SpeakerParlGroupName == "Sozialdemokratische Fraktion", "SP",
                                            ifelse(SpeakerParlGroupName == "Fraktion BD", "BD",
                                              SpeakerParlGroupName)))))))
         ) %>%
  mutate(ParlGroupName = factor(ParlGroupName, levels = Parteien),
         SpeakerParlGroupName = factor(SpeakerParlGroupName, levels = Parteien)) %>%
  arrange(ParlGroupName, SpeakerParlGroupName)

df2 <- data.frame(matrix(ncol = length(Parteien), nrow = length(Parteien)))
colnames(df2) <- Parteien
rownames(df2) <- Parteien

for (ParlGroupName in Parteien){
  for (SpeakerParlGroupName in Parteien){
    df2[SpeakerParlGroupName,ParlGroupName] <- mean(speeches_NR_final$value[speeches_NR_final$ParlGroupName==ParlGroupName&speeches_NR_final$SpeakerParlGroupName==SpeakerParlGroupName])
  }
}




ggplot(speeches_NR_final, aes(x = ParlGroupName, y = SpeakerParlGroupName, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0, limits = c(-1, 1))

