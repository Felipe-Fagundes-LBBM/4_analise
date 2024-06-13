library(ggplot2)
library(dplyr)


library(readxl)
library(dplyr)
library(ggplot2)
library(vcd)


###143 amostras

# Carregar dados do Excel
data <- read_excel("DADOS_BRUTOS_COVID_LONGA_PREENCHENDO (1).xlsx")

# Calcular média, mediana e desvio padrão por grupo
summary_stats <- data %>%
  group_by(GROUP) %>%
  summarize(
    mean_age = mean(AGE, na.rm = TRUE),
    median_age = median(AGE, na.rm = TRUE),
    sd_age = sd(AGE, na.rm = TRUE))

summary_stats

# Calcular frequências absolutas para gênero
gender_freq <- table(data$GENDER)
print(gender_freq)


# Calcular frequências relativas para gênero por grupo
gender_freq_by_group <- data %>%
  group_by(GROUP, GENDER) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

print(gender_freq_by_group)

# Calcular frequências relativas para nível educacional por grupo
edu_freq_by_group <- data %>%
  group_by(GROUP, EDU_LEVEL) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

edu_freq_by_group

# Realizar teste qui-quadrado para gênero
gender_table <- table(data$GROUP, data$GENDER)
chi_gender <- chisq.test(gender_table)
chi_gender

# Realizar teste qui-quadrado para nível de educação
edu_table <- table(data$GROUP, data$EDU_LEVEL)
chi_edu <- chisq.test(edu_table)
chi_edu

# Realizar o teste t para idade pelos grupos
t_test_result <- t.test(AGE ~ GROUP, data = data)

t_test_result


###120 amostras

# Carregar dados do Excel
data <- read_excel("Cópia de DADOS_BRUTOS_COVID_LONGA_SEQUENCIAMENTO (1).xlsx")

# Calcular média, mediana e desvio padrão por grupo
summary_stats <- data %>%
  group_by(GROUP) %>%
  summarize(
    mean_age = mean(AGE, na.rm = TRUE),
    median_age = median(AGE, na.rm = TRUE),
    sd_age = sd(AGE, na.rm = TRUE))

summary_stats

# Calcular frequências absolutas para gênero
gender_freq <- table(data$GENDER)
gender_freq


# Calcular frequências relativas para gênero por grupo
gender_freq_by_group <- data %>%
  group_by(GROUP, GENDER) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

gender_freq_by_group

# Calcular frequências relativas para nível educacional por grupo
edu_freq_by_group <- data %>%
  group_by(GROUP, EDU_LEVEL) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)

edu_freq_by_group

# Realizar teste qui-quadrado para gênero
gender_table <- table(data$GROUP, data$GENDER)
chi_gender <- chisq.test(gender_table)
chi_gender

# Realizar teste qui-quadrado para nível de educação
edu_table <- table(data$GROUP, data$EDU_LEVEL)
chi_edu <- chisq.test(edu_table)
chi_edu

# Realizar o teste t para idade pelos grupos
t_test_result <- t.test(AGE ~ GROUP, data = data)

t_test_result
