# **Michael L. Thompson, July 2, 2020 ====
# [me on LinkedIn](www.linkedin.com/in/mlthomps)

# INTRODUCTION ====
# Inspired by Andrew Gelman's July 2, 2020 blog post
# ([here](https://statmodeling.stat.columbia.edu/2020/07/02/no-i-dont-believe-that-claim-based-on-regression-discontinuity-analysis-that/))
# and the responses by Erik Gahner Larsen, including the code he posted on Github at
# ["erikgahner/Code to Paolo Inglese"](https://gist.github.com/erikgahner/39212cf46422e3871ba99873ccba45ac).

#
# Packages ====
library(magrittr)
library(tidyverse)
library(brms)
library(tidybayes)

# Data ====
# Data downloaded originally from here,
# CITATION:
# Barfort, Sebastian; Klemmensen, Robert; Larsen, Erik Gahner, 2019,
# "Replication Data for: Longevity Returns to Political Office",
# https://doi.org/10.7910/DVN/IBKYRX, Harvard Dataverse, V1,
# UNF:6:r2RQxcUp75qcOP1j4GqNHw== [fileUNF]
#
pathnm <- ""
df_rdd <- read_csv(paste0(pathnm,"longevity.csv"))

# Filter the data down to post-1944, and the close elections.
# Only grab 1 election per candidate.
df_rdd2 <- df_rdd %>% 
  filter(
    !is.na(death_date_imp),
    year >= 1945, 
    abs(margin_pct_1)<5,
    living_day_imp_post>0,
    living_day_imp_pre>0
  ) %>% 
  select(
    starts_with("cand"),
    death_date_imp,
    margin_pct_1,
    living_day_imp_post,
    living_day_imp_pre
  ) %>% 
  group_by(across(starts_with("cand"))) %>% 
  summarize(across(everything(),head,1),.groups = "drop") %>%
  rename(margin = "margin_pct_1") %>%
  mutate( win = as.integer(margin >= 0))


# EXPLORATORY DATA ANALYSIS with VISUALIZATIONS ====

# ***Margin as proxy for negative "win" ====
# By definition, we see high correlation between margin & win: 
# Why include both in the same regression?  
# Seems margin should be sufficient.
df_rdd2 %$% cor(win,margin)
lm(margin ~ win, data = df_rdd2) %>% summary()

df_rdd2 %>% 
  {
    ggplot(.,aes(x=win,y=margin)) + 
      geom_point() + 
      geom_smooth(method = lm, formula = y ~ x) +
      labs(
        title = "Margin vs. 'Win'",
        subtitle = "Obviously, by construction, margin and win are highly correlated."
      )
  } %>%
  print()

df_rdd2  %>% 
  {
    ggplot(.,aes(x=living_day_imp_pre/365.25,y=living_day_imp_post/365.25)) + 
    geom_point(alpha=0.3,size=3) + 
      geom_smooth(method=lm,formula=y~x)+
      labs(
        title = "Years lived after election vs. Years lived before election",
        subtitle = "Years lived before has some predictive power of years lived after.",
        x = "Years lived before election",
        y = "Years lived after election"
      )
  } %>%
  print()


df_rdd2 %$% cor(margin,living_day_imp_post)
df_rdd2  %>% 
  {
    ggplot(.,aes(x=margin,y=living_day_imp_post/365.25)) + 
      geom_point(alpha=0.3,size=3) + 
      geom_smooth(method=loess,formula=y~x) +
      labs(
        title = "Margin vs. Years lived after election",
        subtitle = "No apparent linear correlation exists, so threw LOESS at it."
      )
  } %>%
  print()

df_rdd2  %>% 
  {
    ggplot(.,aes(x=margin,y=living_day_imp_post/365.25)) + 
      geom_point(alpha=0.3,size=3) + 
      geom_smooth(method=lm,formula=y~poly(x,3))+
      labs(
        title = "Margin vs. Years lived after election",
        subtitle = "No apparent linear correlation exists, so threw a cubic at it."
      )
  } %>%
  print()

# MODEL BUILDING ====

# ****Model 1: win + margin + years_before ====
lm(living_day_imp_post ~ win + margin + living_day_imp_pre, data = df_rdd2 ) %>% 
  summary()

# ****Model 2: margin + years_before ====
lm(living_day_imp_post ~ margin + living_day_imp_pre, data = df_rdd2 ) %>% 
  summary()

# ****Bayesian Model 1: win + margin + years_before, w/horseshoe priors ====
get_prior(
  formula = years_after ~ win + margin + years_before,
  family  = gaussian,
  data    = df_rdd2 %>% 
    mutate(
      years_after = living_day_imp_post/365.25,
      years_before= living_day_imp_pre/365.25
    )
)
priors <- c(
  set_prior(prior = "horseshoe(par_ratio = 2,scale_slab=3,df_global=3)",class="b"),
  set_prior(prior = "lognormal(3.4,0.4)", class = "Intercept")
)

# Prior simulation: Note the centering of the "living_day_imp_pre" term so
# as to eliminate strong correlation between its coefficient and the Intercept.
brm_sim1 <- brm(
  formula = years_after ~ win + margin + years_before,
  family  = gaussian,
  prior   = priors, 
  data    = df_rdd2 %>% 
    mutate(
      years_after = living_day_imp_post/365.25,
      years_before= (living_day_imp_pre - median(living_day_imp_pre))/365.25
    ) ,
  sample_prior = "only",
  chains = 1,
  cores = 1,
  iter = 3000L
)
# ****Prior predictive checking ====
pp_check(brm_sim1, type = "hist")
pp_check(brm_sim1, type = "stat", stat = "mean")
pp_check(brm_sim1, type = "stat", stat = "max")
pp_check(brm_sim1, type = "stat", stat = "min")

# ****Model Estimation: Posterior sampling ====
brm_fit1 <- update(
  brm_sim1,
  sample_prior = "no",
  chains = 4,
  cores  = 4,
  iter  = 2000
)

summary(brm_fit1)
brm_fit1 <- add_criterion(brm_fit1,"loo")
# Pairs plot of posterior shows high correlation among parameters
pairs(brm_fit1)

# Investigate Total Contribution of win and margin =====

# MLT: I suggest that if the question is whether "win" has an effect
# upon y="living_day_imp_post", then we must consider the total
# contribution of win in predicting y, including that of all terms
# highly correlated with win, and see if this contribution to y is
# significantly different when win=1 from when win=0.  Checking the
# significance of the simple coefficient upon win is insufficient.

# Get the posterior sample of the coefficients.
fit1_coef <- fixef(brm_fit1,summary = FALSE) %>% as_tibble()

# Total contribution of the correlated terms win & margin.
win_marg_contrib <- df_rdd2 %>% 
  select(win,margin) %>% 
  as.matrix() %>% 
  { . %*% t(fit1_coef[,c("win","margin")])} %>% 
  set_colnames(sprintf("mcmc_%04d",seq_len(ncol(.)))) %>% 
  as_tibble() %>% 
  mutate(win = df_rdd2$win) %>% 
  select(win,everything())

win_marg_contrib %>% 
  pivot_longer(
    cols=starts_with("mcmc"),
    names_to="iter",
    values_to="contribution"
  ) %>%
  mutate(win=factor(win)) %>% 
  {
    ggplot(.,aes(x=win,y=contribution,fill=win)) + 
      geom_boxplot(alpha=0.5,varwidth = TRUE) +
      labs(
        title = "Distributions of Total Contributions to Model by `margin` & `win`",
        subtitle = "No significant difference between `win`=0 and `win`=1."
      )
  } %>%
  print()

# ****Posterior Predictive Checking =====
pp_check(brm_fit1, type = "hist")
pp_check(brm_fit1, type = "stat", stat = "mean")
pp_check(brm_fit1, type = "stat", stat = "max")
pp_check(brm_fit1, type = "stat", stat = "min")

# ****Bayesian Model 2: margin + years_before, w/horseshoe priors ====
brm_fit2 <- update( brm_fit1, formula. = ~ . - win )

summary(brm_fit2)
brm_fit2 <- add_criterion(brm_fit2,"loo")
# Pairs plot of posterior shows high correlation amongst parameters
pairs(brm_fit2)
# Posterior Predictive Checking 
pp_check(brm_fit2, type = "hist")
pp_check(brm_fit2, type = "stat", stat = "mean")
pp_check(brm_fit2, type = "stat", stat = "max")
pp_check(brm_fit2, type = "stat", stat = "min")

# ****Bayesian Model 3: win + years_before, w/horseshoe priors ====
brm_fit3 <- update( brm_fit1, formula. = ~ . - margin )

summary(brm_fit3)
brm_fit3 <- add_criterion(brm_fit3,"loo")
# Pairs plot of posterior shows high correlation amongst parameters
pairs(brm_fit3)
# Posterior Predictive Checking 
pp_check(brm_fit3, type = "hist")
pp_check(brm_fit3, type = "stat", stat = "mean")
pp_check(brm_fit3, type = "stat", stat = "max")
pp_check(brm_fit3, type = "stat", stat = "min")

# MODEL SELECTION ====
# **Model Comparison using loo ====
loo_compare(brm_fit1,brm_fit2,brm_fit3)
# No significant difference between the 3 models.


# More models ====
brm_fit4 <- update(
  brm_fit1, 
  newdata = df_rdd2 %>% 
    mutate(
      win  = sign(margin),
      years_after = living_day_imp_post/365.25,
      years_before= (living_day_imp_pre - median(living_day_imp_pre))/365.25
    )
)

brm_fit5 <- update( brm_fit1, formula. = ~ . - margin + poly(margin,2) )

