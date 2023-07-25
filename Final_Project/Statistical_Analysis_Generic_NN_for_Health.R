# Parte I

# Load data and libraries
library(ACSWR)
#install.packages("ARTool")
library(ARTool)

#df = diseases
df = Generic_NN_for_health

# Remove rows with lr = 0.03
df<-df[!(df$Learning_Rate=="0.03"),]
df_f1 <- subset(df, select = -c(Accuracy))
df_f1
df_acc <- subset(df, select = -c(F1_Score))
df_acc

# Interaction Plots
par(mfrow=c(1,1))
## F1_Score
interaction.plot(df_f1$NN_Architecture,df_f1$Learning_Rate,df_f1$F1_Score, 
                 main="Interacción entre Tasa de Aprendizaje y Arquitectura para Puntaje F1",
                 xlab="Arquitectura", ylab="Media de Puntaje F1",
                 trace.label = "Tasa de Aprendizaje")
interaction.plot(df_f1$NN_Architecture,df_f1$Dataset,df_f1$F1_Score, 
                 main="Interacción entre Conjunto de Datos y Arquitectura para Puntaje F1",
                 xlab="Arquitectura", ylab="Media de Puntaje F1",
                 trace.label = "Conjunto de Datos")
interaction.plot(df_f1$Learning_Rate,df_f1$Dataset,df_f1$F1_Score, 
                 main="Interacción entre Tasa de Aprendizaje y Conjunto de Datos para Puntaje F1",
                 xlab="Tasa de Aprendizaje", ylab="Media de Puntaje F1",
                 trace.label = "Conjunto de Datos")

## Accuracy
interaction.plot(df_acc$NN_Architecture,df_acc$Learning_Rate,df_acc$Accuracy, 
                 main="Interacción entre Tasa de Aprendizaje y Arquitectura para Exactitud",
                 xlab="Arquitectura", ylab="Media de Exactitud",
                 trace.label = "Tasa de Aprendizaje")
interaction.plot(df_acc$NN_Architecture,df_acc$Dataset,df_acc$Accuracy, 
                 main="Interacción entre Conjunto de Datos y Arquitectura para Exactitud",
                 xlab="Arquitectura", ylab="Media de Exactitud",
                 trace.label = "Conjunto de Datos")
interaction.plot(df_acc$Learning_Rate,df_acc$Dataset,df_acc$Accuracy, 
                 main="Interacción entre Tasa de Aprendizaje y Conjunto de Datos para Exactitud",
                 xlab="Tasa de Aprendizaje", ylab="Media de Exactitud",
                 trace.label = "Conjunto de Datos")


# Convert all columns except col2 into factors
df_f1[, -4] <- lapply(df_f1[, -4], factor)
df_f1

# ARTool: R Package for the Aligned Rank Transform for Nonparametric Factorial ANOVAs
# Alligned Rank Transform
# F1-Score
m_f1 <- art(F1_Score~ NN_Architecture + Learning_Rate + Dataset+ (NN_Architecture*Learning_Rate)+
           (NN_Architecture*Dataset)+(Learning_Rate*Dataset)+(NN_Architecture*Dataset*Learning_Rate),data=df_f1)

# verify transform success
summary(m_f1)

# Anova
anova(m_f1)

# Post-Hoc, utilizando Tukey por haber transformado los datos, normalmente se utiliza
# Dunn en datos no parametricos.
tukey = art.con(m_f1, "NN_Architecture")
tukey

##########################################################
# Accuracy
# Convert all columns except col2 into factors
df_acc[, -4] <- lapply(df_acc[, -4], factor)
df_acc

m_acc <- art(Accuracy~ NN_Architecture + Learning_Rate + Dataset+ (NN_Architecture*Learning_Rate)+
           (NN_Architecture*Dataset)+(Learning_Rate*Dataset)+(NN_Architecture*Dataset*Learning_Rate),data=df_acc)

# verify transform success
summary(m_acc)

# Anova
anova(m_acc)

# Post-Hoc, utilizando Tukey por haber transformado los datos, normalmente se utiliza
# Dunn en datos no parametricos.
tukey_acc = art.con(m_acc, "NN_Architecture")
tukey_acc