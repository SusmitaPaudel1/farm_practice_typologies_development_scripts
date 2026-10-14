#Run the analysis for the typology application using linear mixed models

# install.packages("lme4")
# install.packages("performance")
# install.packages("olsrr")
# install.packages("ggplot2")
# install.packages("jtools")
# install.packages("see")

library(performance)
library(lme4)
library(lmerTest)
library(olsrr)
library(ggplot2)
library(jtools)
library(see)
library(ggplot2)
library(dplyr)
library(patchwork)


# setwd("E://Application Case Study//Maumee far parcels generic lulc WQ")

#CDL Dataset
data_lulc = read.csv("E://Application Case Study//Maumee farm parcels generic lulc WQ//watshed_farmp_generic_lulc_wq_TKN_0823_cleaned_with_daymet.csv", check.names = FALSE)
#number of predictors = 13 and 1 random effect
siteid <- unique(data_lulc$PourPtID)
siteid
length(siteid)

#Typology Dataset
data_typo = read.csv("E://Application Case Study//Maumee farm parcels typology WQ//watshed_farmp_typology_wq_TKN_0823_cleaned_with_daymet.csv", check.names = FALSE)
#43 predictor variables and 1 random effect
siteid <- unique(data_typo$PourPtID)
siteid
length(siteid)
nrow(data_typo)

#rename the columns before modeling so that the spaces in the predictor variables won't behave like trash in the model
names(data_lulc) <- make.names(names(data_lulc))
names(data_lulc)

names(data_typo) <- make.names(names(data_typo))
names(data_typo)

#log transform the Phosphorous variable since its right skewed
data_lulc$logTKN <- log(data_lulc$Kjeldahl.nitrogen)
data_typo$logTKN <- log(data_typo$Kjeldahl.nitrogen)

#scale the numeric predictors
data_lulc$Area_sc <- scale(data_lulc$AreaSqKm)
data_lulc$Year_sc <- scale(data_lulc$Year)
data_lulc$Month_sc <- scale(data_lulc$Month)
data_lulc$prcp_sc <- scale(data_lulc$prcp)
data_lulc$tmin_sc <- scale(data_lulc$tmin)
data_lulc$tmax_sc <- scale(data_lulc$tmax)

data_typo$Area_sc <- scale(data_typo$AreaSqKm)
data_typo$Year_sc <- scale(data_typo$Year)
data_typo$Month_sc <- scale(data_typo$Month)
data_typo$prcp_sc <- scale(data_typo$prcp)
data_typo$tmin_sc <- scale(data_typo$tmin)
data_typo$tmax_sc <- scale(data_typo$tmax)

#fit the model with generic lulc
model_lulc <- lmer(logTKN ~ Corn + Fallowing + Forage.and.Cover.Crops + Natural.Land.Cover + Other.Crops + Other.Land + Pasture.Hay + Soybeans + Urban.Area + Wheat 
                   + Area_sc + prcp_sc + tmin_sc + tmax_sc+ Month_sc + (1 | PourPtID), data = data_lulc)

summary(model_lulc)

check_collinearity(model_lulc)

#remove the variables with high VIF and then run a model
model_lulc_reduced <- lmer(logTKN ~ Corn + Forage.and.Cover.Crops + Natural.Land.Cover + Other.Crops + Other.Land
                           + Pasture.Hay + Soybeans + Wheat + Urban.Area + Area_sc + prcp_sc + tmax_sc + Month_sc + (1 | PourPtID),
                           data = data_lulc)

summary(model_lulc_reduced)
check_collinearity(model_lulc_reduced)

#Fit the annual typology full model
library(lmerTest)
model_typo.lm <- lmer(
  logTKN ~
    Cn.Con + Cn.IR + Cn.IRTD + Cn.TD + Nat.Con + Nat.IR + Nat.IRTD + Nat.TD + Oc.Con + Oc.IR + Oc.IRTD + Oc.TD + Pa.Con + Pa.IR + Pa.IRTD + Pa.TD +
    Sb.Con + Sb.IR + Sb.IRTD + Sb.TD + Ur.Con + Ur.IR + Ur.IRTD + Ur.TD + Wt.Con + Wt.IR + Wt.IRTD + Wt.TD + PSo.TD + So.Con + So.TD + PSo.IRTD + Oc.C + Oc.CTD + Pa.C + 
    Sb.CTD + Cn.CTD + Sb.C + Wt.CTD + Cn.C + Pa.CTD + Area_sc + prcp_sc + tmax_sc + Month_sc + tmin_sc +(1 | PourPtID), data = data_typo)

summary(model_typo.lm)

check_collinearity(model_typo.lm)

#remove the highly correlated variables and run the model
model_typo_reduced <- lmer(logTKN ~ 
                             Cn.TD + Nat.TD + Sb.TD + Pa.Con + Sb.Con +  Oc.TD + Wt.TD + Ur.Con + Wt.Con + Area_sc + prcp_sc + tmax_sc  + Month_sc + 
                             (1 | PourPtID), data = data_typo)

summary(model_typo_reduced)

check_collinearity(model_typo_reduced)

#check the AIC and BIC of the full as well as reduced model
AIC(model_lulc, model_typo.lm)
BIC(model_lulc, model_typo.lm)

AIC(model_lulc_reduced, model_typo_reduced)
BIC(model_lulc_reduced, model_typo_reduced)

r2(model_lulc_reduced)
r2(model_typo_reduced)

#Check for the R2 of the models
r2(model_lulc)
r2(model_typo.lm)

# # *****************************************************************************************************************************
# Plot the observed vs predicted plot

#Add predictions
data_lulc$pred_lulc <- predict(model_lulc_reduced)
data_typo$pred_typo <- predict(model_typo_reduced)

#Combine for plotting
plot_pred_tkn <- bind_rows(
  data_lulc %>%
    select(logTKN, pred_lulc) %>%
    rename(Observed = logTKN, Predicted = pred_lulc) %>%
    mutate(Model = "Conventional LULC"),
  
  data_typo %>%
    select(logTKN, pred_typo) %>%
    rename(Observed = logTKN, Predicted = pred_typo) %>%
    mutate(Model = "Annual Typology")
)
plot_pred_tkn


#Observed vs Predicted
p_obs_pred <- ggplot(plot_pred_tkn, aes(x = Observed, y = Predicted)) +
  geom_point(alpha = 0.35, size = 1.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_wrap(~Model) +
  labs(
    x = "Observed log-transformed Total Kjeldahl Nitrogen",
    y = "Predicted log-transformed Total Kjeldahl Nitrogen"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

p_obs_pred

#############################################################################################################################

# validation without scaling predictors

form_lulc <- formula(model_lulc_reduced)
form_typo <- formula(model_typo_reduced)

match_cols <- c(
  "PourPtID", "ActivityStartDate", "Time", "Kjeldahl.nitrogen",
  "MeasureUnitCode_12_13", "ResultSampleFractionText"
)

lulc_cv <- data_lulc %>%
  group_by(across(all_of(match_cols))) %>%
  mutate(RepeatID = row_number()) %>%
  ungroup()

typo_cv <- data_typo %>%
  group_by(across(all_of(match_cols))) %>%
  mutate(RepeatID = row_number()) %>%
  ungroup()

keys <- c(match_cols, "RepeatID")

lulc_cv <- lulc_cv %>% arrange(across(all_of(keys)))
typo_cv <- typo_cv %>% arrange(across(all_of(keys)))

vars_lulc <- all.vars(form_lulc)
vars_typo <- all.vars(form_typo)

keep <- complete.cases(lulc_cv[, vars_lulc]) &
  complete.cases(typo_cv[, vars_typo]) &
  is.finite(lulc_cv$logTKN) &
  is.finite(typo_cv$logTKN)

lulc_cv <- lulc_cv[keep, ]
typo_cv <- typo_cv[keep, ]

# Observation-level holdout within each monitoring site
set.seed(123)

lulc_cv <- lulc_cv %>%
  group_by(PourPtID) %>%
  mutate(
    TestRow = if (n() > 1) {
      row_number() %in% sample(
        seq_len(n()),
        size = min(max(1, round(0.20 * n())), n() - 1)
      )
    } else {
      FALSE
    }
  ) %>%
  ungroup() %>%
  mutate(Set = ifelse(TestRow, "Test", "Train"))

# Use exactly the same split for typology
typo_cv$Set <- lulc_cv$Set

lulc_cv$TestRow <- NULL

# Number of sites and observations
lulc_cv %>%
  group_by(Set) %>%
  summarise(
    Sites = n_distinct(PourPtID),
    Observations = n(),
    .groups = "drop"
  )

# validation function
run_holdout <- function(df, form, label) {
  
  train <- df[df$Set == "Train", ]
  test  <- df[df$Set == "Test", ]
  
  fit <- lmer(
    form,
    data = train,
    REML = FALSE,
    na.action = na.fail,
    control = lmerControl(
      optimizer = "nloptwrap",
      optCtrl = list(
        ftol_abs = 1e-8,
        xtol_abs = 1e-8,
        maxeval = 10000
      )
    )
  )
  
  test$Predicted <- predict(
    fit,
    newdata = test,
    re.form = NULL
  )
  
  test %>%
    transmute(
      PourPtID,
      Year,
      Month,
      Observed = logTKN,
      Predicted,
      Model = label
    )
}

# Run validation for both models
plot_prd_nitrite <- bind_rows(
  run_holdout(lulc_cv, form_lulc, "Conventional LULC"),
  run_holdout(typo_cv, form_typo, "Annual Typology")
)

# Performance metrics
cv_metrics <- plot_prd_nitrite %>%
  group_by(Model) %>%
  summarise(
    RMSE = sqrt(mean((Observed - Predicted)^2)),
    MAE  = mean(abs(Observed - Predicted)),
    R2_test = 1 - sum((Observed - Predicted)^2) /
      sum((Observed - mean(Observed))^2),
    .groups = "drop"
  )

cv_metrics

##############################################################################
#refit with maximum likelihood for AIC/BIC comparison
lulc_ml <- update(model_lulc_reduced, REML = FALSE)
typo_ml <- update(model_typo_reduced, REML = FALSE)

AIC(lulc_ml, typo_ml)
BIC(lulc_ml, typo_ml)



