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
    year,
    female,
    death_date_imp,
    margin_pct_1,
    living_day_imp_post,
    living_day_imp_pre
  ) %>% 
  group_by(across(starts_with("cand"))) %>% 
  summarize(across(everything(),head,1),.groups = "drop") %>%
  rename(margin = "margin_pct_1") %>%
  mutate( 
    win = as.integer(margin >= 0),
    years_after = living_day_imp_post/365.25,
    years_before= (living_day_imp_pre - median(living_day_imp_pre))/365.25
  )


# EXPLORATORY DATA ANALYSIS with VISUALIZATIONS ====

# ***Margin as proxy for "win" ====
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

# KEY FIGURES ====
df_rdd2  %>% mutate(win=factor(win),abs_margin=factor(abs(round(margin)))) %>%
  {
    ggplot(.,aes(x=living_day_imp_pre/365.25,y=living_day_imp_post/365.25,group=win)) + 
      geom_point(aes(color=abs_margin,shape=win),size=3) + 
      geom_smooth(
        aes(fill=win,linetype=win),
        method=loess,formula=y~x,color="black",size=1
      ) +
      scale_color_brewer(palette = "YlOrRd", direction = -1) +
      labs(
        title = "Years lived after election vs. Years lived before election by win",
        subtitle = "Years lived before has some predictive power of years lived after.",
        x = "Years lived before election",
        y = "Years lived after election",
        caption = paste(
          "It is not apparent that win or margin help predict longevity.",
          "But, it does seem that younger candidates have more close victories!",
          sep = "\n"
        )
      )
  } %>%
  print()

df_rdd2  %>% 
  mutate(
    win=factor(win),
    abs_margin= factor(abs(margin) <= 1.5,labels = c(">1.5%","0-1.5%") )
  ) %>%
  {
    ggplot(.,aes(y=win,x=living_day_imp_pre/365.25)) + 
      geom_violin(draw_quantiles = c(0.025,0.5,0.975),fill="darkgray") + 
      geom_point(
        aes(color=abs_margin,shape=win),
        size=5,position = position_jitter(width = 0.0,height=0.1)
      ) + 
      geom_smooth(method=lm,formula=y~x,color="black",size=1) +
      scale_color_manual(values=c("burlywood1","firebrick")) +
      labs(
        title = "Years lived before election by win",
        subtitle = sprintf(
          "%s\n%s",
          paste(
            "At younger ages (towards left),",
            "more near-zero-margin wins (red triangles)",
            "than near-zero-margin losses (red circles).",
            sep=" "
          ),
          "Is this why can appear as if more longevity given close wins?"
        ),
        x = "Years lived before election"
      )
  } %>%
  print()

df_rdd2  %>% 
  mutate(
    win_jtr = as.double(win)+rnorm(n(),1,0.1),
    win=factor(win),
    abs_margin= factor(abs(margin) > 1.5,labels = c("0-1.5%",">1.5%") )
  ) %>%
  {
    ggplot(.,aes(y=win,x=living_day_imp_post/365.25)) + 
      geom_violin(draw_quantiles = c(0.025,0.5,0.975),fill="darkgray") + 
      geom_point(
        aes(y=win_jtr,color=abs_margin,shape=win),
        size=6
      ) + 
      geom_text(aes(y=win_jtr,label=round(living_day_imp_pre/365.25))) +
      scale_color_manual(values=c("firebrick","burlywood1")) +
      labs(
        title = "Years lived AFTER election by win, labeled by age at election",
        subtitle = sprintf(
          "%s\n%s",
          paste(
            "Greater years after election (towards right) coincide with",
            "more near-zero-margin wins (red triangles) than losses (red circles);",
            "but ages at election (labels) are lower for near-zero-margin wins",
            "than for near-zero-margin losses.",
            sep=" "
          ),
          "Is this why can appear as if more longevity given close wins?"
        ),
        x = "Years lived AFTER election",
        caption=paste(
          "This implies we should predict margin of victory given age at",
          "election.\nThen greater longevity just falls out coincidentally",
          "as a by-product of being younger.\nWe could also just predict",
          "age at death given win and margin to show that election outcome",
          "doesn't impact longevity!",
          sep=" "
        )
      )
  } %>%
  print()


df_rdd2  %>% 
  mutate(
    win=factor(win,labels=c("lost","won")),
    abs_margin= factor(abs(margin) > 1.5,labels = c("0-1.5%",">1.5%") )
  ) %>% {
    ggplot(.,aes(x=death_date_imp)) + 
      geom_histogram(bins=10) + 
      facet_wrap(~ win + abs_margin,ncol=1) +
      labs(
        title = "Death Date Distribution by win & margin",
        subtitle = "Does appear that 'won, 0-1.5%' have later death dates than others."
      )
  } %>%
  print()

df_rdd2  %>% 
  mutate(
    win=factor(win,labels=c("lost","won")),
    abs_margin= factor(abs(margin) > 1.5,labels = c("0-1.5%",">1.5%") )
  ) %>% {
    ggplot(.,aes(x=years_after)) + 
      geom_histogram(bins=10) + 
      facet_wrap(~ win + abs_margin,ncol=1) +
      labs(
        title = "Distribution of Years Lived After Election by win & margin",
        subtitle = "Does appear that 'won, 0-1.5%' have more years lived after election than others."
      )
  } %>%
  print()

# Tables =====
# This table seems to show a clear increase in longevity with "won_0-1.5%"
# over "lost_0-1.5%".
df_rdd2  %>% 
  mutate(
    win=factor(win),
    abs_margin= factor(abs(margin) > 1.5,labels = c("0-1.5%",">1.5%") )
  ) %>% 
  group_by(win,abs_margin) %>% 
  mutate(
    before=living_day_imp_pre/365.25,
    after=living_day_imp_post/365.25,
    life=before+after
  ) %>%
  summarize(
    n = n(),
    fem_pct = mean(female)*100,
    across(c(before:life),list(mean=mean,med=median,sd=sd)),
    .groups="drop"
  ) %>%
  mutate(win = c("lost","won")[as.integer(win)]) %>%
  unite(col="win_absmarg",win,abs_margin) %>%
  pivot_longer(cols=!c(win_absmarg),names_to="stat",values_to="val") %>%
  pivot_wider(names_from=win_absmarg,values_from=val) %>%
  print(n=Inf)

# Correlation of margin and win ====
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
  data    = df_rdd2
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
  data    = df_rdd2,
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

# Residual expected lifetime vs. win.
win_marg_contrib <- df_rdd2 %>% 
  select(win,margin) %>% 
  as.matrix() %>% 
  { . %*% t(fit1_coef[,c("win","margin")])} %>% 
  set_colnames(sprintf("mcmc_%04d",seq_len(ncol(.)))) %>% 
  as_tibble() %>% 
  mutate(win = df_rdd2$win) %>% 
  select(win,everything())

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
  newdata = df_rdd2 %>% mutate(win = sign(margin))
)

brm_fit5 <- update( brm_fit1, formula. = ~ . - margin + poly(margin,2) )

