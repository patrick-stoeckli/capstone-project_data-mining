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
