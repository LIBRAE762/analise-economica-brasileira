library(tidyverse)
library(writexl)

dados <- read.csv("indicadores_economicos.csv")

colnames(dados) <- c("Ano", "PIB", "IPCA", "Desemprego", "Selic")

write_xlsx(dados, path = "indicadores_economicos_exportado.xlsx")

print(ggplot(dados, aes(x = Ano)) +
  geom_line(aes(y = PIB, color = "PIB"), linewidth = 1.5) +
  geom_point(aes(y = PIB, color = "PIB"), size = 3) +

  geom_line(aes(y = IPCA * (max(PIB) / max(IPCA)), color = "IPCA"), linewidth = 1.5) +
  geom_point(aes(y = IPCA * (max(PIB) / max(IPCA)), color = "IPCA"), size = 3) +

  geom_line(aes(y = Desemprego * (max(PIB) / max(Desemprego)), color = "Desemprego"), linewidth = 1.5) +
  geom_point(aes(y = Desemprego * (max(PIB) / max(Desemprego)), color = "Desemprego"), size = 3) +

  geom_line(aes(y = Selic * (max(PIB) / max(Selic)), color = "Selic"), linewidth = 1.5) +
  geom_point(aes(y = Selic * (max(PIB) / max(Selic)), color = "Selic"), size = 3) +

  scale_y_continuous(
    name = "PIB (trilhões de reais)",
    sec.axis = sec_axis(~ . / (max(dados$PIB) / max(dados$IPCA)), name = "dados percentuais (%)")
  ) +

  scale_color_manual(values = c(
     "PIB" = "red",
     "IPCA" = "green",
     "Desemprego" = "blue",
     "Selic" = "orange")) +
  labs(title = "Indicadores Econômicos do Brasil (PIB, Selic, Desemprego e IPCA)",
       x = "Ano", y = "Valor",
       color = "Indicador") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12))
)