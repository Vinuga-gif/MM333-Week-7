# clear environment
rm(list = ls())

# turn off scientific notation
options(scipen = 999)

#set wd
setwd("C:/Users/vinug/OneDrive - University of Reading/mm333/Week 7")

#libraries
library(RKaggle)
library(ARTool)
library(car)
library(rstatix)
library(effectsize)
library(emmeans)
library(tidyverse)

# Download and read the "fast-food-marketing-campaign-ab-test" dataset from Kaggle
raw <- RKaggle::get_dataset("chebotinaa/fast-food-marketing-campaign-ab-test")

# Check the data
raw

# Activity 1 ----------------------------------------------------------------

# Structure and descriptive statistics
glimpse(raw)
summary(raw)

# How many stores, and how many rows per store?
n_distinct(raw$LocationID)
raw %>% count(LocationID) %>% count(n, name = "locations")

# Missing values and duplicate rows
colSums(is.na(raw))
sum(duplicated(raw))

# One row per store: total, early and late sales
stores <- raw %>%
  group_by(LocationID, MarketID, MarketSize, AgeOfStore, Promotion) %>%
  summarise(total_sales = sum(SalesInThousands),
            early_sales = sum(SalesInThousands[week <= 2]),
            late_sales  = sum(SalesInThousands[week >= 3]),
            .groups = "drop") %>%
  # Set up the categorical variables
  mutate(promotion   = factor(Promotion, labels = c("Promo 1", "Promo 2", "Promo 3")),
         market_size = factor(MarketSize, levels = c("Small", "Medium", "Large")))

# Number of stores, and stores per promotion
nrow(stores)
table(stores$promotion)

# Activity 2 ----------------------------------------------------------------

# Number of stores, mean and standard deviation of sales by promotion
stores %>%
  group_by(promotion) %>%
  summarise(n = n(), mean = mean(total_sales), sd = sd(total_sales))

# Boxplot of total sales by promotion, with the mean shown as a diamond
ggplot(stores, aes(x = promotion, y = total_sales)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 4) +
  labs(x = "Promotion", y = "Total sales over 4 weeks (thousands)") +
  theme_minimal()

# Fit the one-way ANOVA
model_one <- aov(total_sales ~ promotion, data = stores)
summary(model_one)

# Which promotions differ?
TukeyHSD(model_one)

# Effect size
eta_squared(model_one)

# Activity 3 ----------------------------------------------------------------

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model_one)
par(mfrow = c(1, 1))

# Similar spread across groups
leveneTest(total_sales ~ promotion, data = stores)

# Normality of residuals
shapiro.test(residuals(model_one))

# Median sales by promotion
stores %>%
  group_by(promotion) %>%
  summarise(median = median(total_sales))

# Kruskal-Wallis test
kruskal.test(total_sales ~ promotion, data = stores)

# Dunn's test, with the Holm adjustment for multiple comparisons
dunn_test(stores, total_sales ~ promotion, p.adjust.method = "holm")

# Effect size
kruskal_effsize(stores, total_sales ~ promotion)

# Histogram of total sales, coloured by market size
ggplot(stores, aes(x = total_sales, fill = market_size)) +
  geom_histogram(bins = 25, alpha = 0.7, position = "identity") +
  labs(x = "Total sales over 4 weeks (thousands)", y = "Number of stores",
       fill = "Market size") +
  theme_minimal()

# Activity 4 ----------------------------------------------------------------

# How many stores in each combination?
table(stores$promotion, stores$market_size)

# Interaction plot of the group means
ggplot(stores, aes(x = market_size, y = total_sales,
                   colour = promotion, group = promotion)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line") +
  labs(x = "Market size", y = "Mean total sales (thousands)",
       colour = "Promotion") +
  theme_minimal()

# Fit the two-way ANOVA with the interaction
model_two <- aov(total_sales ~ promotion * market_size, data = stores)
summary(model_two)

# Partial eta squared for each effect
eta_squared(model_two, partial = TRUE)

# Which promotions differ, now that market size is in the model?
TukeyHSD(model_two, which = "promotion")

# Assumption checks
leveneTest(total_sales ~ promotion * market_size, data = stores)
shapiro.test(residuals(model_two))

# Fit the aligned rank transform model
model_art <- art(total_sales ~ promotion * market_size, data = stores)

# Check that the alignment worked
summary(model_art)

# ANOVA on the aligned ranks
anova(model_art)

# Post hoc comparisons between promotions
art.con(model_art, "promotion", adjust = "holm")

# Split store age into two groups
stores <- stores %>%
  mutate(store_age = if_else(AgeOfStore <= 5, "New", "Established"),
         store_age = factor(store_age, levels = c("New", "Established")))

# How many stores in each of the 18 groups?
ftable(table(stores$promotion, stores$market_size, stores$store_age))

# Fit the three-way ANOVA
model_three <- aov(total_sales ~ promotion * market_size * store_age, data = stores)
summary(model_three)

# Activity 5 ----------------------------------------------------------------

# Is store age related to sales?
cor(stores$AgeOfStore, stores$total_sales)

# Is store age similar across the three promotions?
summary(aov(AgeOfStore ~ promotion, data = stores))

# Sales against store age, with one line per promotion
ggplot(stores, aes(x = AgeOfStore, y = total_sales, colour = promotion)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Age of store (years)", y = "Total sales (thousands)",
       colour = "Promotion") +
  theme_minimal()

# Homogeneity of regression slopes: the interaction should not be significant
model_slopes <- aov(total_sales ~ AgeOfStore * promotion, data = stores)
summary(model_slopes)

# Fit the ANCOVA, with the covariate entered first
model_ancova <- aov(total_sales ~ AgeOfStore + promotion, data = stores)
summary(model_ancova)
eta_squared(model_ancova, partial = TRUE)

# Adjusted means for each promotion
emmeans(model_ancova, ~ promotion)

# Activity 6 ----------------------------------------------------------------

# How strongly are the two outcomes related?
cor(stores$early_sales, stores$late_sales)

# Fit the MANOVA
model_manova <- manova(cbind(early_sales, late_sales) ~ promotion, data = stores)
summary(model_manova)

# Follow-up: one ANOVA per outcome
summary.aov(model_manova)

# MANCOVA: adding store age as a covariate
model_mancova <- manova(cbind(early_sales, late_sales) ~ AgeOfStore + promotion,
                        data = stores)
summary(model_mancova)

# Activity 7 ----------------------------------------------------------------

# Business assumptions: change these to see how the decision changes
margin <- 0.30
promo_cost <- tibble(promotion = c("Promo 1", "Promo 2", "Promo 3"),
                     cost = c(25, 10, 15))

# Net return per store for each promotion
decision <- stores %>%
  group_by(promotion) %>%
  summarise(mean_sales = mean(total_sales)) %>%
  left_join(promo_cost, by = "promotion") %>%
  mutate(profit = mean_sales * margin,
         net_return = profit - cost) %>%
  arrange(desc(net_return))
decision

# How does the ranking change with the profit margin?
sensitivity <- expand_grid(margin = c(0.2, 0.3, 0.4, 0.5),
                           decision %>% select(promotion, mean_sales, cost)) %>%
  mutate(net_return = mean_sales * margin - cost) %>%
  select(margin, promotion, net_return) %>%
  pivot_wider(names_from = promotion, values_from = net_return)
sensitivity
