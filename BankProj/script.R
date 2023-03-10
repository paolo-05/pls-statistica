# import librerie
library(rpart)
library(rpart.plot)
library(caret)

# import dataset
bank.url <- "dataset.csv"
bank <- read.csv(bank.url, header = TRUE, sep = ",")

#conversione variabile risposta in fattore
bank$churn <- as.factor(bank$churn)

# prima occhiata del dataset
head(bank)

# albero di previsione con tutte le variabili (tranne customer_id che è dichiarata come inutile)
fit.tree.all <- rpart(churn ~ . - customer_id, data=bank)
rpart.plot(fit.tree.all, type = 5, under = TRUE, extra = 100, 
           box.palette = c("cyan2", "lightsalmon"))
# calcolo della bontà del modello con tutte le variabili
cm.all <- confusionMatrix(predict(fit.tree.all, type="class"), bank$churn)
cm.all$table
accuracy.all <- sum(diag(cm.all$table))/sum(cm.all$table)
accuracy.all # il modello sicuramente prevede bene, ma è troppo complicato


# istogramma che analizza l'importanza delle variabili
barplot(fit.tree.all$variable.importance, col = 10)


# primo albero di prova con solo age
fit.tree.1v <- rpart(churn ~ age, data = bank)
rpart.plot(fit.tree.1v, type = 5, under = TRUE, extra = 100, 
            box.palette = c("cyan2", "lightsalmon"))

# calcolo accuracy
cm.1v <- confusionMatrix(predict(fit.tree.1v, type="class"), bank$churn)
cm.1v$table
accuracy.1v <- sum(diag(cm.1v$table))/sum(cm.1v$table)
accuracy.1v # accuracy buona, ma migliorabile



# albero con le variabili più importanti
fit.tree.final <- rpart(churn ~ age + products_number + active_member + balance, data=bank)
rpart.plot(fit.tree.final, type = 5, under = TRUE, extra = 100, 
           box.palette = c("cyan2", "lightsalmon"))



# analisi e confronto della bontà della previsione
cm.final <- confusionMatrix(predict(fit.tree.final, type="class"), bank$churn)
cm.final$table
accuracy.final <- sum(diag(cm.final$table))/sum(cm.final$table)
accuracy.final  # accuracy ottima con un modello efficiente


# costruzione di un grafico per capire se c'è correlazione fra numero di prodotti, età e churn
cols <- ifelse(bank$churn == 0, "blue", "red")
plot(bank$products_number, bank$age, col=cols, pch=20, 
     ylab="Eta'", xlab = "Numero di prodotti")
legend("topright", c("will churn", "won't churn"), pch = 20,
       col=c("blue", "red"), bty = "n")

