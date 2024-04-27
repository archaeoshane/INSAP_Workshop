rm(list = ls())
options(scipen=999)

install.packages("tidyverse")
library(tidyverse)


data<-read.csv("https://raw.githubusercontent.com/archaeoshane/INSAP_Workshop/main/Bizmoune_Points.csv")


data$X<-data$UTM_Eastin
data$Y<-data$UTM_Northi


plot(data$Y~data$X)


data<-data%>%
  filter(X>444000)

plot(data$UTM_Northi~data$UTM_Eastin)




data.shape<-st_as_sf(data, 
                     coords = c("X", "Y"))


figure<-ggplot(data.shape) +
  geom_sf(aes(size = Lithic), color = "black", lwd = 0.15) +
  labs(caption = "Bizmoune Survey - 2022 & 2023") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right") +
  scale_size_continuous(name = "Lithics")

figure

ggsave(figure, file="figure.png", dpi = 300)




summary_data <- data %>%
  summarize(
    Pottery = sum(Pottery, na.rm = TRUE),
    Lithic = sum(Lithic, na.rm = TRUE),
    Slag = sum(Slag, na.rm = TRUE),
    MSA = sum(MSA, na.rm = TRUE)
  )

summary_data

summary_data_long <- tidyr::gather(summary_data, key = "Category", value = "Total")

summary_data_long


barplot<-ggplot(summary_data_long, aes(x = Category, y = Total, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Sum by Category", x = "Category", y = "Total") +
  theme_minimal()

barplot

ggsave(barplot, file="barplot.png", dpi = 300)

