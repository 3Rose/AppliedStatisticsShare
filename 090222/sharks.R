# Problem n.4
# The file walesharks.txt collects the number y of sightings of wale sharks during January 2022 at 64 observatory
# points in the Indian Ocean. The dataset also reports the UTM coordinates si of those locations, and the Chlorophyll
# concentration x(si) (mg/m3 ) measured at the same locations. Consider for the variable y(si ), i = 1, ...64, the
# following model
# log[y(si)] = a0 + a1 · log[x(si )] + δ(si),
# with δ(si) a stationary residual.


sharks <- read.table("walesharks.txt", sep = " ", header = T)
sharks
# a) Estimate via generalized least squares the parameters a0 , a1 of the model. Report the model estimated for
# δ(si), and discuss the model assumptions (use an exponential model without nugget for δ(si )).

# We assume isotropy

coordinates(sharks) <- c('x','y')
sharks$sights <- log(sharks$sights)
sharks

# Build empirical variogram
v=variogram(sights ~ log.chlorofill, data=sharks)
plot(v,pch=19)

# Fit the variogram with exponential model
v.fit <- fit.variogram(v, vgm(0.4, "Exp", 100000))
plot(v, v.fit, pch = 3)

# We predict the zetas via GLS and then approximate the coefficients
# of the linear model



# b) Provide a kriging prediction log[y ∗ (s0 )] of the log-number of sightings at an observatory point located close
# to the island of Fenfushi (South Ari Atoll, Maldives) s0 = (253844.8, 385997.7). For this purpose, use a
# point prediction of the log-transformed Chlorophyll concentration log[x(s0 )] obtained through a spatially
# stationary model (use a spherical model without nugget for log[x(s)]; report the estimated model, and the
#                   point prediction log[x(si)]∗).

# We first predict the log-chlorophyll concentration

s0.new <- data.frame(
  x = 253844.8,
  y = 385997.7
)
# Use a stationary model for log.chlorofill
coordinates(s0.new) <- c('x','y')
v=variogram(log.chlorofill ~ 1, data=sharks)
plot(v,pch=19)
# As the text suggest we use a spherical model
v.fit <- fit.variogram(v, vgm(3, "Sph", 100000))
plot(v, v.fit, pch = 3)
# Now we provide a point prediction of the log-chrolofill 

# The stationary model for log.chlorofill
# zeta.chlorofill = b_0 + b_1 * delta(s_i)
g.t <- gstat(formula = log.chlorofill ~ 1, data = sharks, model = v.fit)
# ordinary kriging (stationary model)
predict(g.t, s0.new)
# Our point prediction of log.chlorofill
s0.new$log.chlorofill <- 3.983747

# And we predict the log sights at those coordinates
# Use same model from before
v=variogram(sights ~ log.chlorofill, data=sharks)
plot(v,pch=19)

# Fit the variogram with exponential model (same as before)
v.fit <- fit.variogram(v, vgm(0.4, "Exp", 100000))
plot(v, v.fit, pch = 3)

# Our point prediction
g.t <- gstat(formula = sights ~ log.chlorofill, data = sharks, model = v.fit)
pred <- predict(g.t,s0.new, BLUE = FALSE)
pred
pred$var1.pred
# c) Report the kriging variance σ 2(s0 ) of the point prediction at point (b). Would you deem the variance σ 2 (s0)
# to be fully representative of the uncertainty associated with the prediction y ∗(s0 )?

# The kriging variance from point b)

pred$var1.var
# 0.4

# Considering that the variance of points where I do have data is
# practically zero
pred <- predict(g.t, sharks, BLUE = FALSE)
pred

# 0.4 is considerably bigger than the other variances in the dataset (approaching
# numerical zero). So it is consistent with the uncertainty associated to the
# new point 
