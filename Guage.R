# Paste or type your script code here:
library(tidyverse)

# Dummy data
dataset <- data.frame(Year = c(2017:2020),
                      Max = c(rep(1,4)),
                      Min = c(rep(0,4)),
                      Target = c(0.25,0.15,0.75,0.5),
                      #Benchmark = c(0.25,0.15,0.75,0.5),
                      Actual = c(0.15,0.25,0.65,0.6)
                      )

# Building chart
ggplot(dataset, aes(fill = "#FFCD11", ymax = 1, ymin = 0, xmax = 2, xmin = 1)) +
  geom_rect(aes(ymax = 1, ymin = 0, xmax = 2, xmin = 1), fill = "#E6E6E6") +
  #geom_rect(aes(ymax = `2020 LOS` / `2023 Commitment`, ymin = 0, xmax = 2, xmin = 1), colour = "blue", linetype = "dashed", size = 2) +
  geom_rect(aes(ymax = Actual, ymin = 0, xmax = 2, xmin = 1), fill = "#FFCD11") +
  geom_rect(aes(ymax = Target, ymin = 0, xmax = 2, xmin = 1), colour = "black", size = 1, linetype = "dashed") + 
  geom_rect() + 
  coord_polar(theta = "y",start=-pi/2) + xlim(c(0, 2)) + ylim(c(0,2)) +
  geom_text(aes(x = .6, y = 0.5, label = paste(Actual*100, "%", sep = '')), size = 8, color = "#FFCD11") + 
  #geom_text(aes(x=0.35, y=0.5, label = paste("$",`2020 LOS`,sep = '')), size = 10, color = "blue") + 
  geom_text(aes(x = 0, y = 2, label = paste(round(Target * 100,0), "%", sep = '')), size = 8, color = "black") + 
  geom_text(aes(x = 1.5, y = 1.05, label = paste(round(Max  * 100,0), "%", sep = '')), size = 8, color = "gray") + 
  geom_text(aes(x = 1.5, y = 1.95), label = "0%", size = 8, color = "gray") + 
  scale_fill_manual(values = c("red"="#C9146C", "orange"="#DA9112", "green"="#129188")) +
  facet_wrap(. ~ Year, nrow = 1) +
  theme_void() +
  theme(strip.text.x = element_text(size = 18, color = "black")) +
  guides(fill = FALSE, colour = FALSE)