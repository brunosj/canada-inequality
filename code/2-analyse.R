#--------------------------------------------------------------------
# Voter Turnout and Income Inequality in Canada
# Bruno St-Jacques (Hertie School of Governance)
# 2019
#--------------------------------------------------------------------

data1 <- select(dat11, turnout, giniindex,
                pension_inc,
                invest_inc,
                self_inc,
                benefits_inc,
                other_inc,
                fem_inc,
                fem_pop,
                percYouth
)

mod1 <- lm(turnout ~ .,
           data=data1)
car::vif(mod1)
summary(mod1)
coef(mod1)
names(mod1)

data2 <- select(dat11, turnout, giniindex,
                pension_inc,
                invest_inc,
                self_inc,
                benefits_inc,
                other_inc,
                fem_inc,
                fem_pop,
                percYouth,
                pop_2011,
                owner_occupiers,
                fed_density,
                majority,
                PCC_2011
)

mod2 <- lm(turnout ~ .,
           data=data2)
summary(mod2)
car::vif(mod2)
coef(mod2)
names(mod2)

# confidence interval
ci <- confint(mod2)
print(ci)

# subsamples loop
N <- nrow(data2)
C <- 50
S <- 75

sumb2 <- 0
for (i in 1:C){   # a loop over the number of subsamples
  set.seed(3*i)   # a different seed for each subsample  
  subsample <- data2[sample(1:N, size=S, replace=TRUE), ]
  mod1_sub <- lm(turnout~., data=subsample)
  #sum b2 for all subsamples:
  sumb2 <- sumb2 + coef(mod2)[[2]]
}
print(sumb2/C, digits = 3) # average of 50 estimates of beta-gini20k

# correlation matrix
datacor <- cor(data1)
corrplot(datacor, method = "circle", type = "upper", tl.srt = 45)  # use correlation plot from corrplot package

# plots
ggplot(data1, aes(x = (giniindex), y=turnout)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color='darkblue') 



# anova
anov <- anova(mod2)

# residuals
ehat <- mod1$residuals
plot(data1$giniindex, ehat, xlab="gini", ylab="residuals") # normal distribution

# Jarque-Bera test (normality hypothesis)
ehat <- resid(mod1)
ebar <- mean(ehat)
sde <- sd(ehat)
hist(ehat, col="grey", freq=FALSE, main="",
     ylab="density", xlab="ehat")
curve(dnorm(x, ebar, sde), col=2, add=TRUE,
      ylab="density", xlab="ehat")

# Breusch-Pagan heteroskedasticity test
kable(tidy(bptest(mod2)), 
      caption="Breusch-Pagan heteroskedasticity test")

# AIC and BIC
smod1 <- summary(mod1)
Rsq <- smod1$r.squared
AdjRsq <- smod1$adj.r.squared
aic <- AIC(mod1)
bic <- BIC(mod1)
mod1_stats <- c(Rsq, AdjRsq, aic, bic)

smod2 <- summary(mod2)
Rsq2 <- smod2$r.squared
AdjRsq2 <- smod2$adj.r.squared
aic2 <- AIC(mod2)
bic2 <- BIC(mod2)
mod2_stats <- c(Rsq2, AdjRsq2, aic2, bic2)

mod_stats <- rbind (mod1_stats, mod2_stats)
mod_stats

