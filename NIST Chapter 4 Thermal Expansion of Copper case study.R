# Thermal Expansion of Cu case study
# NIST 4.6.4

library(tidyverse)
CTECu <- read_table2(
  "~/Google Drive/UU PMST/MST 6600 - Advanced Statistical Techniques/Coefficient of Thermal Expansion - Cu.txt")
View(CTECu)

# plot of data - NEED TO ADD `data` becasue names have non-supported characters

ggplot(CTECu, aes(Temp, Cu)) +
  geom_point()

# Quadratic/Quadratic (Q/Q) model
# The NIST handbook has a procedure for calculing estimates for the model
# Below, I just used guess values
# y = (A0 + A1*x + A2*x^2)/(1 + B1*x + B2*X^2)

m.Cu <- nls(Cu ~ ((a0 + a1*Temp + a2*Temp^2)/(1 + b1*Temp + b2*Temp^2)), 
            CTECu, start = list(a0 = 0, a1 = -1, a2 = -1, b1 = 0, b2 = 0), trace = T)
summary(m.Cu)


# create a function using the fit parameters

Cu.fit <- function(x) {
  ((summary(m.Cu)$coefficients[1] + summary(m.Cu)$coefficients[2]*x + summary(m.Cu)$coefficients[3]*x^2)/(1 + summary(m.Cu)$coefficients[4]*x + summary(m.Cu)$coefficients[5]*x^2))
  }

# add the fitted curve to the graph
ggplot(CTECu, aes(Temp, Cu)) +
  geom_point() + 
  stat_function(fun = Cu.fit, colour = "green", linetype = "dashed") +
  ggtitle("Thermal Expansion of Copper", subtitle = "nls function") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) #needed to center the title :^(

# plot the residuals
Cu.resid = resid(m.Cu)

ggplot() +
  geom_point(aes(CTECu$Temp, Cu.resid)) +
  geom_hline(aes(yintercept=0)) +
  geom_hline(aes(yintercept=+2*(summary(m.Cu)$sigma)), linetype = "dashed") +
  geom_hline(aes(yintercept=-2*(summary(m.Cu)$sigma)), linetype = "dashed") +
  ggtitle("Thermal Expansion of Copper Residuals", subtitle = "+/- 2(Residual Statndard Deviation)") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
# Fit is not very good. Shows definate structure. Q/Q model not sufficient.

# 4.6.4.5 Cubic/Cubic Rational Function
# y = (A0 + A1*x + A2x^2 + A3X^3)/(1 + B1x + B2X^2 + B3X^3)

mcc.Cu <- nls(Cu ~ ((a0 + a1*Temp + a2*Temp^2 + a3*Temp^3)/(1 + b1*Temp + b2*Temp^2 + b3*Temp^3)), 
            CTECu, start = list(a0 = 0, a1 = -1, a2 = -1, a3 = 0, b1 = 0, b2 = 0, b3 = 0), 
            trace = T)

summary(mcc.Cu)
# Formula: Cu ~ ((a0 + a1 * Temp + a2 * Temp^2 + a3 * Temp^3)/(1 + b1 * 
#                                                           Temp + b2 * Temp^2 + b3 * Temp^3))
# 
# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
#   a0  1.078e+00  1.707e-01   6.313 1.40e-09 ***
#   a1 -1.227e-01  1.200e-02 -10.224  < 2e-16 ***
#   a2  4.086e-03  2.251e-04  18.155  < 2e-16 ***
#   a3 -1.426e-06  2.758e-07  -5.172 5.06e-07 ***
#   b1 -5.761e-03  2.471e-04 -23.312  < 2e-16 ***
#   b2  2.405e-04  1.045e-05  23.019  < 2e-16 ***
#   b3 -1.231e-07  1.303e-08  -9.453  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.0818 on 229 degrees of freedom
# 
# Number of iterations to convergence: 23 
# Achieved convergence tolerance: 1.664e-06


# create a function using the fit parameters

cc.Cu.fit <- function(x) {
  ((summary(mcc.Cu)$coefficients[1] + summary(mcc.Cu)$coefficients[2]*x + 
      summary(mcc.Cu)$coefficients[3]*x^2 + summary(mcc.Cu)$coefficients[4]*x^3)/(1 + summary(mcc.Cu)$coefficients[5]*x + summary(mcc.Cu)$coefficients[6]*x^2 + summary(mcc.Cu)$coefficients[7]*x^3))
  }

# add the fitted curve to the graph
ggplot(CTECu, aes(Temp, Cu)) +
  geom_point() + 
  stat_function(fun = cc.Cu.fit, colour = "green", linetype = "dashed") +
  ggtitle("Thermal Expansion of Copper", subtitle = "nls function (C/C)") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) #needed to center the title :^(

# plot the residuals
cc.Cu.resid = resid(mcc.Cu)

ggplot() +
  geom_point(aes(CTECu$Temp, cc.Cu.resid)) +
  geom_hline(aes(yintercept=0)) +
  geom_hline(aes(yintercept=+2*(summary(mcc.Cu)$sigma)), linetype = "dashed") +
  geom_hline(aes(yintercept=-2*(summary(mcc.Cu)$sigma)), linetype = "dashed") +
  ggtitle("Thermal Expansion of Copper Residuals (C/C)", subtitle = "+/- 2(Residual Statndard Deviation)") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# Let's fit the data with the LOESS method.
# first we can plot the data and use the geom_smooth layer

ggplot(CTECu, aes(Temp, Cu)) +
  geom_point() + 
  stat_smooth(method = "loess", span = 0.2, linetype = "dashed", size = 0.5) +
  ggtitle("Thermal Expansion of Copper", subtitle = "analysis with LOESS model") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) #needed to center the title :^(

mloess.Cu <- loess(Cu ~ Temp, CTECu, span = 0.2)
summary(mloess.Cu)
# Call:
# loess(formula = Cu ~ Temp, data = CTECu, span = 0.2)
# 
# Number of Observations: 236 
# Equivalent Number of Parameters: 15.02 
# Residual Standard Error: 0.09039 
# Trace of smoother matrix: 16.62  (exact)
# 
# Control settings:
# span     :  0.2 
# degree   :  2 
# family   :  gaussian
# surface  :  interpolate	  cell = 0.2
# normalize:  TRUE
# parametric:  FALSE
# drop.square:  FALSE 
# plot the residuals

mloess.Cu.resid = resid(mloess.Cu)

loess.sigma <- sqrt(deviance(mloess.Cu))/(df.residual(mloess.Cu))

ggplot() +
  geom_point(aes(CTECu$Temp, mloess.Cu.resid)) +
  geom_hline(aes(yintercept=0)) +
  geom_hline(aes(yintercept=+2*((mloess.Cu)$s)), linetype = "dashed") +
  geom_hline(aes(yintercept=-2*((mloess.Cu)$s)), linetype = "dashed") +
  ggtitle("Thermal Expansion of Copper Residuals (LOESS)", subtitle = "+/- 2(Residual Statndard Deviation)") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

summary(mloess.Cu)
names(mloess.Cu)
