# 21/12/17: just left this (below) here, I don't remember why I did this exactly back in april

lm_size_pop <- function(size_pop_df, overwrite){


# Linear regression, intercept forced to be zero (assumption)
fit_mean <- lm(I(buckley$area_sqrt_mean - intercept) ~ 0 + resample$area_sqrt_mean)
fit_50q <- lm(I(buckley$area_sqrt_median - intercept) ~ 0 + resample$area_sqrt_median)
fit_75q <- lm(I(buckley$area_sqrt_75q - intercept) ~ 0 + resample$area_sqrt_75q)
fit_95q <- lm(I(buckley$area_sqrt_95q - intercept) ~ 0 + resample$area_sqrt_95q)
fit_max <- lm(I(buckley$area_sqrt_max - intercept) ~ 0 + resample$area_sqrt_max)
# 21/12/17: I think I wanted to measure the least biased measure (e.g. 75q) based on the angle of the slope of the lm against the slope 1:1
# sum(fit_mean$residuals^2)
# plot(buckley$area_sqrt_mean ~ resample$area_sqrt_mean)
# abline(0, coef(fit_mean), col="red")
# abline(0, 1)

lmax <- lm(buckley$area_sqrt_max ~ resample$area_sqrt_max)
summary(lmax)
# Analysing a bit 
lmax$coefficients
lmax$predicted <- predict(lmax)   # Save the predicted values
lmax$residuals <- residuals(lmax)
flmax <- fortify(lmax)
ggplot(flmax, aes(x = .fitted, y = .resid)) + geom_point()
plot(lmax)
head(fortify(lmax))


}
