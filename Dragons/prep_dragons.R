library(ggplot2)
library(tidyverse)
library(lme4)
library(broom)

Species <- list(
  Stoker=c(
  "Monstrous Nightmare", "Terrible Terror", "Red Death", "Typhoomerang", "Fireworm",
  "Fire Terror", "Singetail", "Hobblegrunt", "Flame Whipper", "Green Death",
  "Night Terror", "Silver Phantom"),

"Boulder"=c(
  "Gronckle", "Hotburple", "Whispering Death", "Screaming Death", 
  "Catastrophic Quaken", "Eruptodon", "Sentinel", "Snafflefang", "Grapple Grounder", 
  "Crimson Goregutter"),

"Tracker"=
  c("Rumblehorn", "Thunderclaw", "Mudraker", "Snifflehunch"),

"Sharp"=c(
  "Deadly Nadder", "Stormcutter", "Timberjack", "Razorwhip", 
  "Grim Gnasher", "Speed Stinger", "Raincutter", "Windstriker", "Stinger",
  "Shivertooth", "Prickleboggle", "Devilish Dervish", "Thornridge"),

#"Tidal Class dragons live in or near the ocean, and unlike most dragons, they do not breathe fire as their ranged attacks, and although very few can, they rarely do use them. The dragons in this class are generally larger in size than most of the other classes. Hiccup also stated that Tidal Class dragons don't react well to signs of aggression. The dragons included are:",
"Tidal"=c(
  "Thunderdrum", "Scauldron", "Seashocker", "Submaripper", "Shellfire",
  "Sliquifier", "Sand Wraith", "Tide Glider", "Shockjaw", "Windwalker",
  "Purple Death", "Luminous Krayfin", "Bewilderbeast"),

#"Little is known about the Mystery Class dragons due to how stealthy and
# sneaky they are. Dragons in this class are generally more feared than those in
# the other dragon classes. The dragon Species included are:",

"Mystery"=c(
  "Hideous Zippleback", "Changewing", "Lycanwing", "Boneknapper",
  "Armorwing", "Dramillion", "Smothering Smokebreath", "Flightmare",
  "Buffalord", "Slithersong", "Slitherwing", "Snow Wraith", "Sandbuster",
  "Cavern Crasher", "Gobsucker", "Sweet Death", "Sword Stealer",
  "Foreverwing", "Death Song", "Hobgobbler", "Silkspanner", "Snaptrapper",
  "Shadow Wing"),

#"Strike Class dragons are characterized by their "blazing speed, vice-like jaw
# strength, and extreme intelligence," as well as pinpoint accuracy, powerful
# attacks, explosive firepower and a unique ability to allow them to navigate in
# their respective environments/atmosphere. The dragons in this class are some of
# the rarest of all dragons, and some are close to extinction. They are also the
# most difficult to train but the most loyal once they have been trained, they
# are also generally among the most powerful dragons. The dragons included are:",

"Strike"=c(
  "Night Fury", "Triple Stryke", "Skrill", "Deathgripper", "Snow Wraith",
  "Light Fury"))


classes <- names(Species)
# [1] "Stoker"  "Boulder" "Tracker" "Sharp"   "Tidal"   "Mystery" "Strike" 

classes <- read.table(text=
'"Class"     "ClWeight"    "ClIQ"
"Stoker"       0          0
"Boulder"      3         -1
"Tracker"      0         1
"Sharp"        2        -1 
"Tidal"        1.5        0
"Mystery"      0          .5
"Strike"       -2          3', header=TRUE) 

ggplot(classes, aes(x=ClWeight, y=ClIQ)) + geom_point()

classes$Species <- map_chr(Species, paste, collapse=",")
classes <- classes %>% separate_rows(Species, sep=", *") %>%
  group_by(Class) %>% 
  mutate(SpWeight=rnorm(n(), mean=ClWeight, sd=1)) %>%
  mutate(SpIQ=rnorm(n(), mean=ClIQ, sd=1)) %>%
  mutate(SpIntercept=rnorm(n(), mean=0, sd=1)) %>%
  mutate(SpCoef=rnorm(n(), mean=0.25, sd=.3)) %>%
  mutate(nn=sample(5:10, 1)) %>%
  uncount(nn) %>% ungroup() %>%
  mutate(Weight=rnorm(n(), mean=SpWeight)) %>%
  mutate(IQ=SpIntercept + SpCoef * Weight + rnorm(n(), mean=SpIQ)) %>%
  mutate(Weight=300 + 20 * Weight, 
         IQ=100 + 15 * IQ / sd(IQ))

hist(classes$Weight)
hist(classes$IQ)

write.csv(classes, file="dragon_IQ.csv")


lmod1 <- lmer(IQ ~ Weight + (1|Class/Species), data=classes)
lmod2 <- lmer(IQ ~ 1 + (1|Class/Species), data=classes)
lmod3 <- lmer(IQ ~ Weight + (1|Species), data=classes)
anova(lmod1, lmod2)
anova(lmod1, lmod3)
lmod3 <- lmer(IQ ~ Weight + (Weight|Class/Species), data=classes)
lmod4 <- lmer(IQ ~ 1 + (Weight|Class/Species), data=classes)
anova(lmod3, lmod4)

lm(IQ ~ Weight + Species, data=classes) %>% tidy()




df <- data.frame(Group=LETTERS[1:10]) %>%
  mutate(yGroup=rnorm(n()), xGroup=rnorm(n())) %>%
  mutate(coefGroup=rnorm(n(), mean=.15)) %>%
  mutate(size=sample(5:10, 1)) %>% uncount(size) %>%
  group_by(Group) %>%
  mutate(x=xGroup + rnorm(n()), y=yGroup + coefGroup * x + rnorm(n(), sd=.5))

ggplot(df, aes(x=x, y=y, color=Group)) + geom_point()
lm(y ~ x + Group, data=df) %>% tidy()

lmod1 <- lmer(y ~ x + (1|Group), data=df)
lmod2 <- lmer(y ~ 1 + (1|Group), data=df)
anova(lmod1, lmod2)

