#--------------------------------------------------------------------
# Voter Turnout and Income Inequality in Canada
# Bruno St-Jacques (Hertie School of Governance)
# 2019
#--------------------------------------------------------------------

# plot
library(ggplot2)
p1 <- ggplot(dat11, aes(x = (gini20k), y=turnout)) +
  geom_point(size=1) +
  geom_smooth(method = 'lm', se = FALSE, color='darkblue') +
  labs(title="Figure 1. Income inequality and voter turnout",
       subtitle= "2015 General Elections, by Federal Electoral District"
  ) +
  xlab("Gini coefficient") + 
  ylab("Voter turnout") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(face="bold"),
        axis.title = element_text(size=8)) +
  theme(plot.title = element_text(size = 10), 
        plot.subtitle = element_text(size = 8))

print(p1)

# models
stargazer(mod1,mod2, title= "Regression Results", 
          type = 'latex', 
          header=FALSE, 
          no.space = TRUE,
          covariate.labels = c('Gini coefficient','Pension income','Investment income','Self-employed income', 'Benefits income', 'Other income', 'Female income', 'Female voting age population', 'Youth (15-34 years old)', 'Total population','Population stability','Population density','Closeness of electoral race', 'CPC incumbent candidate')
)