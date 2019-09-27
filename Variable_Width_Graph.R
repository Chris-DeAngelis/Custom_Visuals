## Created by Chris DeAngelis, CFA

# Load packages
library(ggplot2, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)

# Creating data for chart
data <- data.frame(Company = c(LETTERS[1:8]),
                   EBIT = c(0.1,-0.19,0.2,-0.15,-.08,.15,.21,.09),
                   Revenue = c(244,265,2533,4234,1132,2111,234,432))

# Ranking data by revenue
data$rank <- rank(-data$`EBIT`, ties.method = "first")

# Summarizes data by Company (not necessary for this dataset)
data <- data %>%
  select(Company, `Revenue`, EBIT, rank) %>%
  group_by(Company) %>%
  summarize(`Revenue` = sum(`Revenue`, na.rm = TRUE), EBIT = mean(EBIT, na.rm = TRUE), rank = mean(rank, na.rm = TRUE)) %>%
  arrange(rank) %>%
  as.data.frame()

# Use revenue weights to determine industry average
data$rev_weight <- data$Revenue / sum(data$Revenue)

# Specify chart boundaries with a cushion for text labels that drift off the plot
ymin <- ifelse(min(data$EBIT, na.rm = TRUE) > 0, 0, min(data$EBIT, na.rm = TRUE) - .02)
ymax <- ifelse(max(data$EBIT, na.rm = TRUE) < 0 | is.na(max(data$EBIT, na.rm = TRUE)), 0.1, max(data$EBIT, na.rm = TRUE) + .02)
ylab <- paste(unique(data$Metric), "EBIT (%)", sep = "")
xmin <- 0
xmax <- sum(data$`Revenue`, na.rm = TRUE) + sum(data$`Revenue`, na.rm = TRUE)*.05 
xlab <- "Revenue ($M)"

# Build chart
bp <- barplot(data$EBIT*100, abs(data$`Revenue`), space = 0, ylim = range(pretty(c(ymin * 100 + 1, ymax * 100 + 1))), xlim = range(pretty(c(xmin, xmax))), 
              las = 2, xlab = xlab, yaxt = 'n', ylab = ylab, xaxt = 'n', col = ifelse(data$Company=="C", "gold", "light gray"))
abline(h = sum(data$EBIT * data$rev_weight) *100, lty = 2, col = "blue")
abline(h = 0, lty = 1)
text(bp, y = data$EBIT*100, labels = data$Company, cex = 1, pos = 3)
text(y = ymax * 100 - 2, x = xmax - xmax *.2, label = paste("Industry Average = ", round(sum(data$EBIT * data$rev_weight) *100, digits = 0), "%",sep = "") , cex = .95, col = "blue", font = 3) 
axis(1, axTicks(1), paste("$",axTicks(1), sep = ''), pos = ymin*1.05 * 100 , labels = formatC(axTicks(1), big.mark = ',', format = 'd'))
axis(2, axTicks(2), paste(round(axTicks(2), digits = 0), "%", sep = ''), las = 1, pos = 0)
title(main = "EBIT Margin and Revenue by Company")
