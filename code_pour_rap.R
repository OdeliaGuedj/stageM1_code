## Graphes 3.1.1
ggplot(baseline, aes(x = baseline$age0)) + geom_histogram() + annotate("text", x = 75, y= 500, label = a) + annotate("text", x = 70, y= 500, label = "a = ") 

summary(baseline$activproBis)
baseline$activproBis = factor(baseline$activproBis, c("T","R","C","I","NSP",NA))
ggplot(baseline, aes( x = baseline$activproBis, fill = baseline$activproBis)) + geom_bar(stat = "count") +theme_classic() + xlab(
              "Activité Profesionnelle")+ ylab(
                "Nombre de sujets") + ggtitle (
                  "Classification manuelle de l'activité profesionnelle \n des sujets de EPP3, n = 10157. ") + annotate(
                    "text", x=1, y=6000, label= "5666") + annotate(
                      "text", x=2, y=3500, label= "3209")+annotate(
                       "text", x=3, y=1200, label= "883")+annotate(
                         "text", x=4, y=500, label= "270")+annotate(
                           "text", x=5, y=400, label= "113")+annotate(
                               "text", x=6, y=200, label= "16")



library(cowplot)
window()
df1 = ggplot(baseline, aes(baseline$age0)) + geom_histogram(col = "white", fill = "black", bins  = 20) + xlab("n = 10157")+ ylab("")+ggtitle("Baseline") +theme_minimal()
df2 = ggplot(baseline[which(baseline$activproBis == "T"),], aes(baseline[which(baseline$activproBis == "T"), "age0"])) + geom_histogram(col = "white", fill = "blue", bins  = 20)  + xlab("n = 5666")+ ylab("") + ggtitle("Travailleurs")+theme_minimal()
df3 = ggplot(baseline[which(baseline$activproBis == "R"),], aes(baseline[which(baseline$activproBis == "R"), "age0"])) + geom_histogram(col = "white", fill = "yellow", bins  = 20) + xlab("n = 3209")+ ylab("") + ggtitle("Retraités")+theme_minimal()
df4 = ggplot(baseline[which(baseline$activproBis == "C"),], aes(baseline[which(baseline$activproBis == "C"), "age0"])) + geom_histogram(col = "white", fill = "orange", bins  = 20)+ xlab("n = 883")+ ylab("") + ggtitle("Chômeurs")+theme_minimal()
df5 = ggplot(baseline[which(baseline$activproBis == "I"),], aes(baseline[which(baseline$activproBis == "I"), "age0"])) + geom_histogram(col = "white", fill = "green", bins  = 20)+ xlab("n = 270")+ ylab("") + ggtitle("Inactifs")+theme_minimal()
df6 = ggplot(baseline[which(baseline$activproBis == "NSP"),], aes(baseline[which(baseline$activproBis == "NSP"), "age0"])) + geom_histogram(col = "white", fill = "purple", bins  = 20) + xlab("n = 113")+ ylab("") + ggtitle("NSP")+theme_minimal()
#df7 = ggplot(baseline[which(is.na(baseline$activproBis)),], aes(baseline[which(is.na(baseline$activproBis)), "age0"])) + geom_histogram(col = "white", fill = "brown", bins  = 20) + xlab("n = 16")+ ylab("Sujets NA") + ggtitle("Age NA")
plot_grid(df1, df2, df3, df4, df5, df6)

##graphe ACM
library(factoextra)
plot.MCA(ACM_activpro, choix = "var", title = "Graphe des Variables")
plot.MCA(ACM_activpro, choix = "ind", invisible = "var", title = "Graphe des Individus")

fviz_mca_biplot(ACM_activpro, label ="var", col.var = "cos2", select.var = list(contrib = 15)) + 
  labs(title = "Graphe des Individus et des Modalités")

##graphes CAH
par(mfrow=c(1,2))
plot(cah_activpro, choice = "bar", cex = 0.01, title = "Intertie inter-cluster")
plot(cah_activpro, choice = "tree", cex = 0.01, title = "Dendrogramme",tree.barplot = F)

plot.MCA(ACM_activpro, choix = "ind", invisible = "var", title  = "Graphe des Individus") 
plot(cah_activpro, choice = "map")

## graphe comparaison activproBis et activproCAH
activproCAH_prop = (table(baseline$activproCAH, useNA = "always")/10157)*100
actvproBis_prop = (table(baseline$activproBis, useNA = "always")/10157)*100
comp_activpro_prop = rbind(actvproBis_prop,activproCAH_prop)
barplot(as.matrix(comp_activpro_prop), col = theme2lbleu , 
        beside = TRUE, 
        main = "Comparaison des deux codages \n de la variable activpro (%)", 
        xlab='Modalités', ylab='Pourcentage')
legend("right", legend = c("activproManuel","activproCAH"), col='black', pch = c(22), pt.bg = theme2lbleu, bty ='n')
