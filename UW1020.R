library(dplyr)
library(tidymodels)
library(infer)
library(googlesheets4)

############ tests ###############
# Version A hyp test - self identification
VA <- tibble(
  aAsian = c(0.7143, 1, 1, 0.5714, 0.8571, 0.1429, 0.2857, 0.7143, 1, 0.8571), 
  ind = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
)

rownames(VA) <- c("ind1", "ind2", "ind3", "ind4", "ind5", "ind6", "ind7", "ind8", "ind9", "ind10")


null_dist <- VA |>
  specify(response = aAsian) %>% 
  hypothesize(null = "point", mu = 0.50) |>
  generate(reps = 2000, type = "bootstrap") |> 
  calculate(stat = "mean")

null_dist |>
  summarize(mean = mean(stat))


mnA <- mean(0.7143, 1, 1, 0.5714, 0.8571, 0.1429, 0.2857, 0.7143, 1, 0.8571)
mnA

pValA <- null_dist |>
  get_p_value(obs_stat = mnA, direction = "two-sided")


# Version B hyp test - behaviors

VB <- tibble(
  bAsian = c(0.3333, 0.5, 0.8333, 0.1667, 1, 0.8333, 0.5, 0.1667, 0.8333, 0.5),
  ind = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
)
rownames(VB) <- c("ind1", "ind2", "ind3", "ind4", "ind5", "ind6", "ind7", "ind8", "ind9", "ind10")


null_dist <- VB |>
  specify(response = bAsian) %>% 
  hypothesize(null = "point", mu = 0.50) |>
  generate(reps = 2000, type = "bootstrap") |> 
  calculate(stat = "mean")

null_dist |>
  summarize(mean = mean(stat))


mnB <- mean(0.3333, 0.5, 0.8333, 0.1667, 1, 0.8333, 0.5, 0.1667, 0.8333)
mnB

pValB <- null_dist |>
  get_p_value(obs_stat = mnB, direction = "two-sided")



# Version C hyp test - appearance

VC <- tibble(
  cWhite = c(0.50, 0.17, 0.17, 1.00, 0.67, 0.33, 0.00, 1.00, 0.83, 1.00),
  ind = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  )

VC <- VC %>%
  mutate(
    cAsian = 1 - cWhite) %>% 
  select(ind, cAsian)

rownames(VC) <- c("ind1", "ind2", "ind3", "ind4", "ind5", "ind6", "ind7", "ind8", "ind9", "ind10")

null_dist <- VC |>
  specify(response = cAsian) %>% 
  hypothesize(null = "point", mu = 0.50) |>
  generate(reps = 2000, type = "bootstrap") |> 
  calculate(stat = "mean")

null_dist |>
  summarize(mean = mean(stat))

mnC <- mean(0.50, 0.17, 0.17, 1.00, 0.67, 0.33, 0.00, 1.00, 0.83, 1.00)
mnC

pValC <- null_dist |>
  get_p_value(obs_stat = mnC, direction = "two-sided")
pValC

########### visualizations ########### 

tot <- VC %>% 
  left_join(VA, by = "ind") %>% 
  left_join(VB, by = "ind") %>% 
  rename(
    "c" = cAsian, 
    "b" = bAsian, 
    "a" = aAsian
  )

tot
totL <- tot %>%
  pivot_longer(
    cols = c(a, b, c),
    names_to = "version",
    values_to = "propAsian"
  )
totL

plot1 <- ggplot(totL, aes(x = ind, y = propAsian, fill = version))+
  geom_bar(position='dodge', stat='identity') + 
  labs(
    x = "Celebrity Identifier", 
    y = "Proportion who identified them as Asian", 
    title = "Comparison of racial identification across celebrities by indicator", 
    fill = "Survey Version"
  ) + 
  scale_fill_discrete(labels = c("Self-identification", "Behaviors", "Appearance")) + 
  scale_x_continuous(breaks = seq(1, 10, by = 1))

plot1 





################### stuff I didn't use ################### 
# means <- tibble(
#   aMean = mean(VA$aAsian), 
#   bMean = mean(VB$bAsian), 
#   cMean = mean(VC$cAsian)
#   )

# 
# #chi squared test for homogeneity
# AB <- tibble(
#   VA,
#   VB
# )
# 
# chisq.test(AB, B = 2000)
# 
# BC <- tibble(
#   VB,
#   VC
# )
# 
# chisq.test(BC, B = 2000)
# 
# AC <- tibble(
#   VA,
#   VC
# )
# 
# chisq.test(AC, B = 2000)
# 
# #homogeneity tests against individuals
# chiTest <- function(dat1, dat2, dat3, n){
#   v1 <- dat1[n, , drop = TRUE]
#   v2 <- dat2[n, , drop = TRUE]
#   v3 <- dat3[n, , drop = TRUE]
#   v1 <- v1*100
#   v2 <- v2*100
#   v3 <- v3*100
#   tib <- tibble(
#     v1, v2, v3
#   )
# 
#   colnames(tib) <- c("A", "B", "C")
#   tib <- tib %>% 
#     add_row(
#       A = 100 - v1,
#       B = 100 - v2,
#       C = 100 - v3
#     )
#   longTib <- tib %>% 
#     pivot_longer(
#       cols = everything(), 
#       names_to = "vGroup", 
#       values_to = "counts"
#     )
#     # ) %>% 
#     # mutate(counts = factor(counts))
#     
#   tab <- xtabs (data = longTib, ~ counts + vGroup)
#   
#   chisq.test(tab, p = c(0.5, 0.5))
# }
# 
# 
# n1 <- chiTest(VA, VB, VC, 1)
# n2 <- chiTest(VA, VB, VC, 2)
# n3 <- chiTest(VA, VB, VC, 3)
# n4 <- chiTest(VA, VB, VC, 4)
# n5 <- chiTest(VA, VB, VC, 5)
# n6 <- chiTest(VA, VB, VC, 6)
# n7 <- chiTest(VA, VB, VC, 7)
# n8 <- chiTest(VA, VB, VC, 8)
# n9 <- chiTest(VA, VB, VC, 9)
# n10 <- chiTest(VA, VB, VC, 10)
# 


WP = c(.5, .83, .83, .66, 1)

AP = c(0, .33, 0, 0, .17)

chisq.test(WP, AP, p = c(0.5,0.5))