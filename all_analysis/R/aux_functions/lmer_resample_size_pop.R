### Description
# Fit Linear Mixed-Effects Models

### Usage
# lmer_size_pop

### Arguments
# pop_data : data.frame() with merged populational size, ForCens (relative abundance) and WOA13 (SST) data
# overwrite : TRUE re-run anyway, FALSE do not re-run if plots already exist


lmer_resample_size_pop <- function(pop_data){
  
    # Linear Mixed-Effects Models (lme4, MuMIn)
    
    # Random intercept
    # model <- lmer(response ~ predictor1 + predictor2 + (1 | random effect))
    
    # Random intercept (1+) and slope (pred | random)
    # model <- lmer(response ~ predictor1 + predictor2 + (1 + predictor1 | random effect)
    lmm_0 <-  lmer(area_log_95q ~ 1 + (1+rel_abund|species), data = pop_data, REML=FALSE)
    lmm_0abund <- lmer(area_log_95q ~ rel_abund + (1+rel_abund|species), data = pop_data, REML=FALSE)
    # REML = FALSE: estimates are chosen to optimize log-likelihood (instead of REML)
    # http://rinterested.github.io/statistics/mixed_effects_comparison.html
    anova_0_0abund <- anova(lmm_0, lmm_0abund, refit=FALSE) 
    print(anova_0_0abund) # abundance
    print(xtable(as.data.frame(anova_0_0abund)), include.rownames=FALSE) # LaTeX
    
    
    ### R-squared of mixed-effects models
    # MuMIn: package to calculate R-squared of mixed-effects models
    # it returns the marginal R^2 (R2m) associated with your fixed effects, 
    # and the conditional R^2 (R2c) associated with your fixed effects plus the random effects 
    # Usually we will be interested in the marginal effects...
    print(rbind(lmm_0 = r.squaredGLMM(lmm_0), lmm_0abund = r.squaredGLMM(lmm_0abund)))
    
    
}