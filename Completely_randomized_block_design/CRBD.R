#Johan Ávalos Mendieta - C00817
#Josef Ruzicka González - B87095
#Jonathan Cespedes - B41713


# Primera Parte

# Fase 1

install.packages("ACSWR")
library(ACSWR)
# cargamos el conjunto de datos hardness
data(hardness)
# exploramos el conjunto de datos
str(hardness)
# vemos la composición de los datos de hardness
head(hardness)
# Análisis Visual mediante el Diagrama de Interación
interaction.plot(hardness$Test_Coupon,
                 hardness$Tip_Type,
                 hardness$Hardness) 

# Fase 2

boxplot(hardness$Hardness ~ hardness$Tip_Type)
boxplot(hardness$Hardness ~ hardness$Test_Coupon)

# Fase 3

anova(aov(hardness$Hardness ~ factor(hardness$Tip_Type) +
            factor(hardness$Test_Coupon)))
fit <- aov(hardness$Hardness ~ factor(hardness$Tip_Type) + factor(hardness$Test_Coupon))
TukeyHSD(fit,
         which= "factor(hardness$Tip_Type)",
         ordered=TRUE)
summary(aov(hardness$Hardness ~ factor(hardness$Tip_Type) +
              factor(hardness$Test_Coupon))) 

# Fase 4

summary(lm(hardness$Hardness ~ factor(hardness$Tip_Type) +
             factor(hardness$Test_Coupon))) 
par(mfrow=c(2,2))
plot(lm(hardness$Hardness ~ factor(hardness$Tip_Type) + factor(hardn %>% %>% ess$Test_Coupon)))


# Segunda parte


library(ACSWR)
data("intensity")
intensity

interaction.plot(intensity$Filter,
                 intensity$Ground,
                 intensity$Intensity) 

boxplot(intensity$Intensity ~ intensity$Filter)
boxplot(intensity$Intensity ~ intensity$Ground)


anova(aov(intensity$Intensity ~ factor(intensity$Ground) +
            factor(intensity$Filter)))
fit <- aov(intensity$Intensity ~ factor(intensity$Ground) + factor(intensity$Filter))
TukeyHSD(fit,
         which= "factor(intensity$Ground)",
         ordered=TRUE)

summary(aov(intensity$Intensity ~ factor(intensity$Ground) +
              factor(intensity$Filter))) 


summary(lm(intensity$Intensity ~ factor(intensity$Ground) +
             factor(intensity$Filter))) 
par(mfrow=c(2,2))
plot(lm(intensity$Intensity ~ factor(intensity$Ground) + factor(intensity$Filter)))