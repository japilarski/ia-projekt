library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(psych)
library(scales) 
library(patchwork)

install.packages("kableExtra")

euro_pln <- read_excel("eurpln_d.xlsx")
mci_pln <- read_excel("mci_d.xlsx")
pkn_pln <- read_excel("pkn_d.xlsx")
xau_pln <- read_excel("xaupln_d.xlsx")

euro_pln <- euro_pln[0:107, ] %>%
  mutate(
    log_return = log(Zamkniecie / lag(Zamkniecie)),
    pct_change = (Zamkniecie - lag(Zamkniecie)) / lag(Zamkniecie) * 100
  )

mci_pln <- mci_pln[0:107, ] %>%
  mutate(
    log_return = log(Zamkniecie / lag(Zamkniecie)),
    pct_change = (Zamkniecie - lag(Zamkniecie)) / lag(Zamkniecie) * 100
  )

pkn_pln <- pkn_pln[0:107, ] %>%
  mutate(
    log_return = log(Zamkniecie / lag(Zamkniecie)),
    pct_change = (Zamkniecie - lag(Zamkniecie)) / lag(Zamkniecie) * 100
  )

xau_pln <- xau_pln[0:107, ] %>%
  mutate(
    log_return = log(Zamkniecie / lag(Zamkniecie)),
    pct_change = (Zamkniecie - lag(Zamkniecie)) / lag(Zamkniecie) * 100
  )

write.csv(euro_pln, file = "euro_pln.csv", row.names = TRUE)
write.csv(mci_pln, file = "mci_pln.csv", row.names = TRUE)
write.csv(pkn_pln, file = "pkn_pln.csv", row.names = TRUE)
write.csv(xau_pln, file = "xau_pln.csv", row.names = TRUE)

df1 <- euro_pln[2:107, c("pct_change"), drop = FALSE]
df2 <- mci_pln[2:107, c("pct_change"), drop = FALSE]
df3 <- pkn_pln[2:107, c("pct_change"), drop = FALSE]
df4 <- xau_pln[2:107, c("pct_change"), drop = FALSE]

combined_df <- rbind(df1, df2, df3, df4)

# # Plot the data
# ggplot(
#   df1,
#   aes(
#     x = 'EUR/PLN',
#     y = pct_change,
#   )
#   ) +
#   geom_line(size = 1) +
#   geom_point(size = 2) +
#   labs(title = "Percentage Rate Comparison",
#        x = "X-Axis Label",
#        y = "Percentage Rate",
#        color = "Data Source") +
#   theme_minimal()

# When performing descriptive statistics for return rates, the choice between percentage change and logarithmic rate of return depends on your goals and the properties of the data. Both measures have their advantages and are used in different contexts.

# Key Differences Between Percentage Change and Logarithmic Return
# Aspect	Percentage Change	Logarithmic Rate of Return
# Interpretation	Easy to interpret as a percentage (e.g., 5%).	Natural logarithmic changes; harder to interpret directly.
# Additivity Over Time	Not additive; multi-period returns require compounding.	Additive for multi-period returns (logarithms sum up).
# Handling Extreme Values	Sensitive to large price changes.	Better handles extreme values due to log transformation.
# Symmetry	Not symmetric for gains and losses.	Symmetric for gains and losses.
# Use in Statistical Models	May violate assumptions of normality.	Log returns often approximate a normal distribution better.

#Which to Use for Descriptive Statistics?
# 1. If Interpretability is Important (e.g., Communicating Results):
# Use percentage changes because they are intuitive and easy to explain.
# 2. If Analyzing Statistical Properties (e.g., Volatility, Risk Models):
# Use logarithmic returns because they are:
# Symmetric for gains and losses.
# Additive over time, making them suitable for multi-period analyses.
# Better suited for statistical models (e.g., normality assumptions).

# statystyki opisowe
euro_pln_stat <- describe(euro_pln[, c("log_return", "pct_change")])
euro_pln_stat

# box and whiskers graph

# euro_pln_clean <- euro_pln$pct_change[is.finite(euro_pln$pct_change)]
# mci_pln_clean <- mci_pln$pct_change[is.finite(mci_pln$pct_change)]
# pkn_pln_clean <- pkn_pln$pct_change[is.finite(pkn_pln$pct_change)]
# xau_pln_clean <- xau_pln$pct_change[is.finite(xau_pln$pct_change)]

# Find the global min and max values across all the pct_change columns
# x_range <- range(c(euro_pln_clean, mci_pln_clean, pkn_pln_clean, xau_pln_clean))

# svg("boxplots.svg") # Specify the file name and size (in inches)

# par(mfrow = c(4, 1))  # 2 rows, 2 columns

# boxplot(
#   euro_pln$pct_change,
#   main = "Wykres pudełkowy dla procentowej stopy zwrotu dla EUR/PLN",
#   col = "lightblue",
#   border = "brown",
#   horizontal = TRUE,
#   outline = TRUE,
#   ylim = x_range
# )
# boxplot(
#   mci_pln$pct_change,
#   main = "Wykres pudełkowy dla procentowej stopy zwrotu dla MCI",
#   col = "lightblue",
#   border = "brown",
#   horizontal = TRUE,
#   outline = TRUE,
#   ylim = x_range
# )
# boxplot(
#   pkn_pln$pct_change,
#   main = "Wykres pudełkowy dla procentowej stopy zwrotu dla PKN",
#   col = "lightblue",
#   border = "brown",
#   horizontal = TRUE,
#   outline = TRUE,
#   ylim = x_range
# )
# boxplot(
#   xau_pln$pct_change,
#   main = "Wykres pudełkowy dla procentowej stopy zwrotu dla XAU/PLN",
#   col = "lightblue",
#   border = "brown",
#   horizontal = TRUE,
#   outline = TRUE,
#   ylim = x_range
# )

# dev.off()

svg("boxplots.svg") # Specify the file name and size (in inches)

p1 <- ggplot(df1, aes(x = '', y = pct_change)) +
  geom_boxplot() +
  coord_flip() +
  ggtitle("EUR/PLN") +
  ylab("") +
  scale_y_continuous(labels = label_percent(scale = 1))
p2 <- ggplot(df2, aes(x = '', y = pct_change)) +
  geom_boxplot() +
  coord_flip() +
  ggtitle("MCI") +
  ylab("") +
  scale_y_continuous(labels = label_percent(scale = 1))
p3 <- ggplot(df3, aes(x = '', y = pct_change)) +
  geom_boxplot() +
  coord_flip() +
  ggtitle("PKN") +
  ylab("") +
  scale_y_continuous(labels = label_percent(scale = 1))
p4 <- ggplot(df4, aes(x = '', y = pct_change)) +
  geom_boxplot() +
  coord_flip() +
  ggtitle("XAU/PLN") +
  ylab("") +
  scale_y_continuous(labels = label_percent(scale = 1))

p1 / p2 / p3 / p4

dev.off()
