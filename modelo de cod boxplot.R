getwd()

install.packages("ggpubr")

library(ggpubr)
library(ggplot2)
library(tidyverse)

data <- readxl::read_excel("Cópia de DADOS_BRUTOS_COVID_LONGA_SEQUENCIAMENTO.xlsx")

data <- na.omit(data)

data

BPA_CONC <- data %>% 
  select(GROUP, BPA_CONC_POINTS, BPA_DIVID_POINTS, BPA_ALTERN_POINTS)
data$GROUP <- factor(data$GROUP, levels = c("CONTROL","CASE"))

# Perform t-test for later reference
res_test <- compare_means(BPA_CONC_POINTS ~ GROUP, data = BPA_CONC, method = "t.test",  size = 8)

# Create the plot
p1 <- ggplot(BPA_CONC, aes(x = GROUP, y = BPA_CONC_POINTS, fill = GROUP)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3, width = 0.7, lwd = 0.4) + 
  geom_jitter(aes(color = GROUP), size = 4.5, width = 0.15, alpha = 0.7) +
  
  # Enhanced color scheme
  scale_color_manual(values = c("#0072B2", "tomato")) +   
  scale_fill_manual(values = c("#0072B2", "tomato")) +  
  
  # Improved labels and title
  ylab("Total points") +
  ggtitle("BPA Sustained Attention") +
  
  # Customized theme with better readability
  theme_classic() +   
  theme(
    axis.text = element_text(size = 18, color = "black"),
    axis.title = element_text(size = 20, color = "black"),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.title.x = element_blank()) +
  
  # Add p-value from the t-test
  annotate("text", x = 1.5, y = max(BPA_CONC$BPA_CONC_POINTS) * 1.05, 
           label = paste0("p-value = ", format(res_test$p.format, digits = 3), size = 12))
# Show the plot
p1

ggsave("BPA Concentration.pdf", width = 7, height = 6, units = "in")
ggsave("BPA Concentration.jpeg", width = 7, height = 6, units = "in")