# Load Cell Calibration 
# NIST 4.6.1

library(tidyverse)
LoadCell <- read_table2(
  "~/Google Drive/UU PMST/MST 6600 - Advanced Statistical Techniques/LoadCellCalibration.txt")
View(LoadCell)

m.LC <- lm(Deflection ~ Load, LoadCell)
summary(m.LC)

Call:
  lm(formula = Deflection ~ Load, data = LoadCell)

# Residuals:
#   Min         1Q     Median         3Q        Max 
# -0.0042751 -0.0016308  0.0005818  0.0018932  0.0024211 
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept) 6.150e-03  7.132e-04    8.623 1.77e-10 ***
#   Load        7.221e-07  3.969e-10 1819.289  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.002171 on 38 degrees of freedom
# Multiple R-squared:      1,	Adjusted R-squared:      1 
# F-statistic: 3.31e+06 on 1 and 38 DF,  p-value: < 2.2e-16

# 4.6.1.2
ggplot(LoadCell, aes(Load, Deflection)) +
  geom_point()
# VERY linear

# 4.6.1.2
summary(m.LC) # see above

#4.6.1.4 Graphical Residual ANalysis - Initial Model
ggplot(LoadCell, aes(Load, Deflection)) +
  geom_point() +
  stat_smooth(method = lm, linetype = "dashed", colour = "blue", size = 0.5) +
  ggtitle("NIST Load Cell Calibration Data", subtitle = "+/- 95% Confidence Intervals are not visible") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

LC.resid = resid(m.LC)

ggplot() +
  geom_point(aes(LoadCell$Load, LC.resid)) +
  geom_hline(aes(yintercept=0)) +
  geom_hline(aes(yintercept=+2*(summary(m.LC)$sigma)), linetype = "dashed") +
  geom_hline(aes(yintercept=-2*(summary(m.LC)$sigma)), linetype = "dashed") +
  ggtitle("Deflection Load Residuals", subtitle = "+/- 2(Residual Statndard Deviation)") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

library(alr3)
pureErrorAnova(m.LC)
# Analysis of Variance Table
# 
# Response: Deflection
#              Df  Sum Sq Mean Sq    F value    Pr(>F)    
# Load          1 15.6039  15.604 3.3842e+08 < 2.2e-16 ***
# Residuals    38  0.0002   0.000                         
# Lack of fit  18  0.0002   0.000 2.1475e+02 < 2.2e-16 ***
# Pure Error   20  0.0000   0.000                         
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# 4.6.1.7 Model Fitting - Model #2
Load2 <- LoadCell$Load^2
m2.LC <- lm(Deflection ~ Load + Load2, LoadCell) 
# we created a linear model in two variables, one of which is hte square of the other

summary(m2.LC)
# Call:
#   lm(formula = Deflection ~ Load + Load2, data = LoadCell)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -4.468e-04 -1.578e-04  3.817e-05  1.088e-04  4.235e-04 
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    6.736e-04  1.079e-04    6.24 2.97e-07 ***
#   Load         7.321e-07  1.578e-10 4638.65  < 2e-16 ***
#   Load2       -3.161e-15  4.867e-17  -64.95  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.0002052 on 37 degrees of freedom
# Multiple R-squared:      1,	Adjusted R-squared:      1 
# F-statistic: 1.853e+08 on 2 and 37 DF,  p-value: < 2.2e-16

LC2.resid = resid(m2.LC)

ggplot() +
  geom_point(aes(LoadCell$Load, LC2.resid)) +
  geom_hline(aes(yintercept=0)) +
  geom_hline(aes(yintercept=+2*(summary(m2.LC)$sigma)), linetype = "dashed") +
  geom_hline(aes(yintercept=-2*(summary(m2.LC)$sigma)), linetype = "dashed") +
  ggtitle("Deflection Load Residuals - Model #2", subtitle = "+/- 2(Residual Statndard Deviation)") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))

# The plot shows an even distribution of the residuals about 0!

# Don't do calibration step at this time.
