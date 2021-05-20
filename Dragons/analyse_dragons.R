library(ggplot2)
library(lme4)
library(cowplot)
library(colorDF)
library(broom)

dragons <- read.csv("dragon_IQ.csv")
theme_set(theme_bw())
hist(dragons$IQ)
hist(dragons$Weight)

g1 <- ggplot(dragons, aes(x=Weight)) + geom_histogram() 
g2 <- ggplot(dragons, aes(x=IQ)) + geom_histogram() 
plot_grid(g1, g2)

summary_colorDF(dragons)

ggplot(dragons, aes(x=Weight, y=IQ)) + geom_point()
ggplot(dragons, aes(x=Weight, y=IQ, color=Class)) + geom_point()
ggplot(dragons, aes(x=Weight, y=IQ, color=Class)) + geom_point() + facet_wrap(~ Class, scale="free")
ggplot(dragons, aes(x=Weight, y=IQ, color=Class)) + geom_point() + 
  geom_smooth(method="lm") + facet_wrap(~ Class, scale="free")
ggplot(dragons, aes(x=Weight, y=IQ, color=Species)) + geom_point() + 
  geom_smooth(method="lm") + facet_wrap(~ Class, scale="free")

## individual lms

glm1 <- glm(IQ ~ Weight + Class + Species, data=dragons)
glm2 <- glm(IQ ~ Class + Species, data=dragons)
anova(glm1, glm2, test="Chisq")


## lmer

lmod1 <- lmer(IQ ~ Weight + (1|Class/Species), data=classes)
lmod2 <- lmer(IQ ~ 1 + (1|Class/Species), data=classes)
lmod3 <- lmer(IQ ~ Weight + (1|Species), data=classes)
afex <- mixed(IQ ~ Weight + (1|Species), data=classes)

anova(lmod1, lmod2)


