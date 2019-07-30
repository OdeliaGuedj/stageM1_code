## Graphes 3.1.1
summary(baseline$activproBis)
baseline$activproBis = factor(baseline$activproBis, c("T","R","C","I","NSP",NA))
ggplot(baseline, aes( x = baseline$activproBis, fill = baseline$activproBis)) + geom_bar(stat = "count") +
  theme_void() + 
  xlab("Activité Profesionnelle")+ 
  ylab("Nombre de sujets") + 
  ggtitle ("") + 
  annotate("text", x=1, y=6000, label= "5666") + 
  annotate("text", x=2, y=3500, label= "3209") +
  annotate("text", x=3, y=1200, label= "883") +
  annotate( "text", x=4, y=500, label= "270") +
  annotate("text", x=5, y=400, label= "113") +
  annotate("text", x=6, y=200, label= "16") + scale_fill_brewer("Activité Professionnelle",palette = "Blues")



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

#######################################################################################################################################

##graphe ACM
library(factoextra)
plot.MCA(ACM_activpro, choix = "var", title = "Graphe des Variables")
plot.MCA(ACM_activpro, choix = "ind", invisible = "var", title = "Graphe des Individus")

fviz_mca_biplot(ACM_activpro, label ="var", col.var = "cos2", select.var = list(contrib = 15)) + 
  labs(title = "Graphe des Individus et des Modalités")

#######################################################################################################################################

##graphes CAH
par(mfrow=c(1,2))
plot(cah_activpro, choice = "bar", cex = 0.01, title = "Intertie inter-cluster")
plot(cah_activpro, choice = "tree", cex = 0.01, title = "Dendrogramme",tree.barplot = F)

plot.MCA(ACM_activpro, choix = "ind", invisible = "var", title  = "Graphe des Individus") 
plot(cah_activpro, choice = "map")

#######################################################################################################################################

## graphe comparaison activproBis et activproCAH
activproCAH_prop = (table(baseline$activproCAH, useNA = "always")/10157)*100
actvproBis_prop = (table(baseline$activproBis, useNA = "always")/10157)*100
comp_activpro_prop = rbind(actvproBis_prop,activproCAH_prop)
barplot(as.matrix(comp_activpro_prop), col = c("#AFAFAF","#D2CAEC") , 
        beside = TRUE, 
        main = "", 
        xlab='Modalités', ylab='Pourcentage')
legend("right", legend = c("activpro","activproCAH"), col='black', pch = c(22), pt.bg = c("#AFAFAF","#D2CAEC"), bty ='n')

#######################################################################################################################################

## tableau de graphes var sociales

### sexe
df_sexe <- data.frame(
  sexe = c("Homme", "Femme"),
  nb_sujets = c(length(which(baseline$sexe == "H")), length(which(baseline$sexe == "F"))))
df_sexe$prop = round(((df_sexe$nb_sujets)/10157)*100,2)

pie_sexe = ggplot(df_sexe, aes(x="", y = nb_sujets, fill = sexe)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + blank_theme + theme(axis.text.x=element_blank()) +
  geom_text(aes(y = nb_sujets/2 + c(0, cumsum(nb_sujets)[-length(nb_sujets)]), 
                label = nb_sujets/100 ), size=5, color = "white") +
 scale_fill_manual ("Sexe",values = c("#77B5FE","#FEBFD2"))

### Entourage
df_entourage <- data.frame(
  entourage = c("Couple","Seul", "Famille", "NA"),
  nb_sujets = c(length(which(baseline$entourage == "couple")), 
                length(which(baseline$entourage == "seul")), 
                length(which(baseline$entourage == "famille")),
                length(which(is.na(baseline$entourage)))
                ))
df_entourage$prop = round((df_entourage$nb_sujets/10157)*100,2)
df_entourage$entourage = factor(df_entourage$entourage, c("NA","Famille","Seul","Couple"))


flip_barPlot_entourage =  ggplot(data = df_entourage, aes(x = df_entourage$entourage , y = df_entourage$prop)) +
  geom_bar(position="dodge",stat="identity" ,  fill = "#BBD2E1") +
  coord_flip() + 
  ggtitle("") + 
  scale_fill_grey( 
    breaks = levels(df_entourage$entourage),  
    labels = levels(df_entourage$entourage)) +                               
  theme_bw() +   
  scale_x_discrete(
    limits = levels(df_entourage$entourage),
    labels = levels(df_entourage$entourage))  +                             
  theme(
    legend.title=element_blank(),  
    legend.position=c(.73,.7),
    axis.title.y=element_blank(), 
    text=element_text(family="serif",size=20),
    plot.title=element_text(face="bold",hjust=c(0,0))
  ) +
  ylab("Statut Famillial")




### age 
hist_age = ggplot(baseline, aes(baseline$age0)) + 
  geom_histogram(fill = "#DD985C", col = "white",bins = 50) + 
  #scale_color_aaas(name = "sexe")+
  xlab("Âge") + 
  geom_vline(xintercept = 54.47,  color = "brown", size=1) +
  annotate(geom = "text",x = 56, y = 500, label = "Q1")+
  geom_vline(xintercept = 59.11,  color = "brown", size=1) +
  annotate(geom = "text",x = 61, y = 550, label = "Med") +
  geom_vline(xintercept = 63.66,  color = "brown", size=1) +
  annotate(geom = "text",x = 65, y = 500, label = "Q3")

### education
df_education <- data.frame(
  education = c(levels(baseline$education),"NA"),
  nb_sujets = c(length(which(baseline$education == "Analphabète")),
                length(which(baseline$education == "Bac+2")),
                length(which(baseline$education == "Baccalauréat")),
                length(which(baseline$education == "CAP/Brevet des collèges")),
                length(which(baseline$education == "Licence/Maitrise/+")), 
                length(which(baseline$education == "Sans diplôme")),
                length(which(is.na(baseline$education)))
                ))
df_education$prop = round((df_education$nb_sujets/10157)*100,2)
df_education$education = factor(df_education$education, c("NA",
                                                          "Analphabète",
                                                          "Sans diplôme",
                                                          "Bac+2",
                                                          "Baccalauréat",
                                                          "CAP/Brevet des collèges",
                                                          "Licence/Maitrise/+"))

flip_barPlot_education = ggplot(data = df_education, aes(x = education , y = prop)) +
  geom_bar(position="dodge",stat="identity" ,  fill = "#B0F2B6") +
  coord_flip() + 
  ggtitle("") + 
  #scale_fill_grey( 
    #breaks = levels(df_education$education),  
    #labels = levels(df_education$education)) +                               
  theme_bw() +   
  scale_x_discrete(
    limits = levels(df_education$education),
      labels = levels(df_education$education))  +                             
  theme(
    legend.title=element_blank(),  
    legend.position=c(.73,.7),
    axis.title.y=element_blank(), 
    text=element_text(family="serif",size=20),
    plot.title=element_text(face="bold",hjust=c(0,0))
  ) +
  scale_fill_gradient("Count", low = "green", high = "red") +
  ylab("Niveau d'éducation")






### activproBis
df_activproBis = data.frame(
  activite_pro = c("Travailleurs","Retraités","Chômeurs","Inactifs","Ne Sais Pas", "NA"),
  nb_sujets = c(length(which(baseline$activproBis == "T")),
                length(which(baseline$activproBis == "R")),
                length(which(baseline$activproBis == "C")),
                length(which(baseline$activproBis == "I")),
                length(which(baseline$activproBis == "NSP")),
                length(which(is.na(baseline$activproBis)))))
df_activproBis$prop = round((df_activproBis$nb_sujets/10157)*100,2)
df_activproBis$activite_pro = factor(df_activproBis$activite_pro, c("NA","Ne Sais Pas","Inactifs","Chômeurs","Retraités","Travailleurs"))

flip_barPlot_activproBis  = ggplot(data = df_activproBis, aes(x = df_activproBis$activite_pro , y = prop)) +
  geom_bar(position="dodge",stat="identity" ,  fill = "#FE96A0") +
  coord_flip() + 
  ggtitle("") + 
  scale_fill_grey( 
    breaks = levels(df_activproBis$activite_pro),  
    labels = levels(df_activproBis$activite_pro)) +                               
  theme_bw() +   
  scale_x_discrete(
    limits = levels(df_activproBis$activite_pro),
    labels = levels(df_activproBis$activite_pro))  +                             
  theme(
    legend.title=element_blank(),  
    legend.position=c(.73,.7),
    axis.title.y=element_blank(), 
    text=element_text(family="serif",size=20),
    plot.title=element_text(face="bold",hjust=c(0,0))
  ) +
  ylab("Activité Professionnelle")


### csp
df_csp = data.frame(
  csp = c(levels(baseline$csp), "NA"),
  nb_sujets = c(length(which(baseline$csp == "Basse")),
                length(which(baseline$csp == "Elevée")),
                length(which(baseline$csp == "Inactifs")),
                length(which(baseline$csp == "Intermediaire")),
                length(which(baseline$csp == "Sans Emplois")),
                length(which(is.na(baseline$csp)))))
df_csp$prop = round(((df_csp$nb_sujets)/10157)*100,2)
df_csp$csp = factor(df_csp$csp, c("NA","Inactifs","Basse","Sans Emplois","Intermediaire","Elevée",NA))



flip_barPlot_csp  = ggplot(data = df_csp, aes(x = csp , y = prop)) +
  geom_bar(position="dodge",stat="identity" ,  fill = "#9683EC") +
  coord_flip() + 
  ggtitle("") + 
  scale_fill_grey( 
    breaks = levels(df_csp$csp),  
    labels = levels(df_csp$csp)) +                               
  theme_bw() +   
  scale_x_discrete(
    limits = levels(df_csp$csp),
    labels = levels(df_csp$csp))  +                             
  theme(
    legend.title=element_blank(),  
    legend.position=c(.73,.7),
    axis.title.y=element_blank(), 
    text=element_text(family="serif",size=20),
    plot.title=element_text(face="bold",hjust=c(0,0))
  ) +
  ylab("Catégorie Socio-Professionnelle") 


plot_grid(pie_sexe, flip_barPlot_entourage,hist_age,flip_barPlot_education,flip_barPlot_csp, flip_barPlot_activproBis)
#######################################################################################################################################

## Tab graphes var medicales

### bmi
hist_bmi = ggplot(baseline, aes(baseline$bmi)) + 
  geom_histogram(fill = "#B0F2B6", col = "white",bins = 50) + 
  xlab("Indice de Masse Corporelle") + 
  geom_vline(xintercept = 22.66,  color = "#175732", size=1) +
  annotate(geom = "text",x = 24, y = 1000, label = "Q1")+
  geom_vline(xintercept = 24.91,  color = "#175732", size=1) +
  annotate(geom = "text",x = 26, y = 1000, label = "Med") +
  geom_vline(xintercept = 27.34,  color = "#175732", size=1) +
  annotate(geom = "text",x = 28, y = 900, label = "Q3")

### tabac
df_tabac = data.frame(
  tabac = c("Non Fumeur","Ancien Fumeur","Arrêt en Cours","Fumeur Actuel", "NA"),
  nb_sujets = c(length(which(baseline$tabac == "non_fum")),
                length(which(baseline$tabac == "anc_fum")),
                length(which(baseline$tabac == "arret_cours")),
                length(which(baseline$tabac == "fum_act")),
                length(which(is.na(baseline$tabac)))))
df_tabac$prop = round(((df_tabac$nb_sujets)/10157)*100,2)
df_tabac$tabac = factor(df_tabac$tabac, c("NA","Arrêt en Cours","Fumeur Actuel","Ancien Fumeur","Non Fumeur"))



flip_barPlot_tabac  = ggplot(data = df_tabac, aes(x = tabac , y = prop)) +
  geom_bar(position="dodge",stat="identity" ,  fill = "#F7E269") +
  coord_flip() + 
  ggtitle("") + 
  scale_fill_grey( 
    breaks = levels(df_tabac$tabac),  
    labels = levels(df_tabac$tabac)) +                               
  theme_bw() +   
  scale_x_discrete(
    limits = levels(df_tabac$tabac),
    labels = levels(df_tabac$tabac))  +                             
  theme(
    legend.title=element_blank(),  
    legend.position=c(.73,.7),
    axis.title.y=element_blank(), 
    text=element_text(family="serif",size=20),
    plot.title=element_text(face="bold",hjust=c(0,0))
  ) +
  ylab("Tabac")

###diabete

df_diab <- data.frame(
  diab = c("Oui", "Non", "NA"),
  nb_sujets = c(length(which(baseline$diab == 1)), length(which(baseline$diab == 0)), length(which(is.na(baseline$diab)))))
df_diab$prop = round(((df_diab$nb_sujets)/10157)*100,2)

pie_diab = ggplot(df_diab, aes(x="", y = nb_sujets, fill = diab)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + blank_theme + theme(axis.text.x=element_blank()) +
  geom_text(aes(y = nb_sujets/2 + c(0, cumsum(nb_sujets)[-length(nb_sujets)]), 
                label = nb_sujets/100 ), size=4, color = "#22427C") +
  scale_fill_brewer("Diabète", palette = "Blues")

#aes(y = nb_sujets/10 + c(0, cumsum(nb_sujets)[-length(nb_sujets)])

### cvd
df_cvd = as.data.frame(df_cvd)
df_cvd$prop = round((df_cvd$nb_sujets/10157)*100,2)

barPlot_cvd = ggplot(data=df_cvd, aes(x=NomBis, y=prop, fill= mod)) +
  geom_bar(stat="identity", position=position_dodge()) + scale_fill_brewer("Maladie \n CV ?") + 
  theme(axis.text.x = element_text(face="bold", color="black", 
                                   size=8, angle=40))+
  scale_x_discrete("Principales maladies Cardiovasculaires (CV)", labels = c("Angine de Poitrine" = "Angine \n de Poitrine",
                                     "Autre Pb CV" = "Autre \n Pb CV",
                                     "AVC" = "AVC",
                                     "Embollie Pulmonaire" = "Embollie \n Pulmonaire",
                                     "Infarctus du Myocarde" = "Infarctus \n du Myocarde",
                                     "Maladie des Artères" = "Maladie \n des Artères",
                                     "Phlébite" = "Phlébite",
                                     "Pontage" = "Pontage"
                                             )) +ylab("Proportion")
###cancer
df_cancer <- data.frame(
  cancer = c("OUI", "NON","NA"),
  nb_sujets = c(length(which(baseline$cancer == 1)), length(which(baseline$cancer == 0)),
                length(which(is.na(baseline$cancer)))))
df_cancer$prop = round(((df_cancer$nb_sujets)/10157)*100,2)

pie_cancer = ggplot(df_cancer, aes(x="", y = nb_sujets, fill = cancer)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + blank_theme + theme(axis.text.x=element_blank()) +
  geom_text(aes(y = nb_sujets/2 + c(0, cumsum(nb_sujets)[-length(nb_sujets)]), 
                label = nb_sujets/100 ), size=5, color = "#723E64") +
  scale_fill_brewer("Déjà soigné pour un Cancer ?", palette = "Purples")

  
### sport
df_sport2 <- data.frame(
  sport2 = c("Jamais", "1-2 fois par semaine","+3 fois par semaine","NA"),
  nb_sujets = c(length(which(baseline$sport2 == 1)), length(which(baseline$sport2 == 2)),
                length(which(baseline$sport2== 3)),length(which(is.na(baseline$sport2)))))
df_sport2$prop = round(((df_sport2$nb_sujets)/10157)*100,2)

pie_sport2 = ggplot(df_sport2, aes(x="", y = nb_sujets, fill = sport2)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + blank_theme + theme(axis.text.x=element_blank()) +
  geom_text(aes(y = nb_sujets/2 + c(0, cumsum(nb_sujets)[-length(nb_sujets)]), 
                label = nb_sujets/100 ), size=5, color = "#18391E") +
  scale_fill_brewer("Pratique sportive",palette = "Greens")

###hypertension arterielle
df_hta <- data.frame(
  hta = c("OUI", "NON","NA"),
  nb_sujets = c(length(which(baseline$hta == 1)), length(which(baseline$hta == 0)),
                length(which(is.na(baseline$hta)))))
df_hta$prop = round(((df_hta$nb_sujets)/10157)*100,2)

pie_hta = ggplot(df_hta, aes(x="", y = nb_sujets, fill = hta)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + blank_theme + theme(axis.text.x=element_blank()) +
  geom_text(aes(y = nb_sujets/2 + c(0, cumsum(nb_sujets)[-length(nb_sujets)]), 
                label = nb_sujets/100 ), size=5, color = "white") +
  scale_fill_brewer("Sujet Hypertendu",palette = "PuRd")



###alcool
df_alcool = data.frame(
  alcool = c("0", "1-2", "3-5", "5-10", ">10", "NA"),
  nb_sujets = c(length(which(baseline$alcoolCut == 0)),
                length(which(baseline$alcoolCut == "1-2")),
                length(which(baseline$alcoolCut == "3-5")),
                length(which(baseline$alcoolCut == "5-10")),
                length(which(baseline$alcoolCut == ">10")),
                length(which(is.na(baseline$alcoolCut)))))
df_alcool$prop = round(((df_alcool$nb_sujets)/10157)*100,2)

df_alcool$alcool = factor(df_alcool$alcool, c("NA", "5-10", "3-5", "1-2", "0"))

flip_barPlot_alcool = ggplot(data = df_alcool, aes(x = df_alcool$alcool , y = prop)) +
  geom_bar(position="dodge",stat="identity" ,  fill = "#FE96A0") +
  coord_flip() + 
  ggtitle("") + 
  scale_fill_grey( 
    breaks = levels(df_alcool$alcool),  
    labels = levels(df_alcool$alcool)) +                               
  theme_bw() +   
  scale_x_discrete(
    limits = levels(df_alcool$alcool),
    labels = levels(df_alcool$alcool))  +                             
  theme(
    legend.title=element_blank(),  
    legend.position=c(.73,.7),
    axis.title.y=element_blank(), 
    text=element_text(family="serif",size=20),
    plot.title=element_text(face="bold",hjust=c(0,0))
  ) +
  ylab("Consommation d'alcool \n(en nombre de verres quotidiens)")

plot_grid(hist_bmi, flip_barPlot_tabac, pie_diab, barPlot_cvd, pie_cancer, pie_sport2, pie_hta, flip_barPlot_alcool)

###############################################################################################################################

## tab graphes var psycho

###epice
df_epice = data.frame( 
  precarite = c("OUI","NON","NA"),
  nb_sujets = c(length(which(baseline$epice > 30.17)),
                length(which(baseline$epice <= 30.17)),
                length(which(is.na(baseline$epice)))))
df_epice$prop = round((df_epice$nb_sujets/10157*100),2)

pie_epice = ggplot(df_epice, aes(x="", y = nb_sujets, fill = df_epice$precarite)) + 
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + blank_theme + theme(axis.text.x=element_blank()) +
  geom_text(aes(y = nb_sujets/2 + c(0, cumsum(nb_sujets)[-length(nb_sujets)]), 
                label = nb_sujets/100 ), size=5, color = "black") +
  scale_fill_brewer("Précarité \n (Basé sur le score EPICE", palette = "Pastel2" )

###ts
df_TS <- data.frame(
  TS = c("OUI", "NON","NA"),
  nb_sujets = c(length(which(baseline$TS == 1)), length(which(baseline$TS == 0)),
                length(which(is.na(baseline$TS)))))
df_TS$prop = round(((df_TS$nb_sujets)/10157)*100,2)

barPlot_TS = ggplot(df_TS, aes(x="", y = nb_sujets, fill = TS)) + 
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + blank_theme + theme(axis.text.x=element_blank()) +
  geom_text(aes(y = nb_sujets/2 + c(0, cumsum(nb_sujets)[-length(nb_sujets)]), 
                label = nb_sujets/100 ), size=5, color = "black") +
  scale_fill_brewer("Tentative de suicide ?",palette = "Set3")


### depression
df_deptot <- data.frame(
  deptot = c("OUI", "NON","NA"),
  nb_sujets = c(length(which(baseline$deptot == 1)), length(which(baseline$deptot == 0)),
                length(which(is.na(baseline$deptot)))))
df_deptot$prop = round(((df_deptot$nb_sujedeptot)/10157)*100,2)

pie_deptot = ggplot(df_deptot, aes(x="", y = nb_sujets, fill = deptot)) + 
  geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) + blank_theme + theme(axis.text.x=element_blank()) +
  geom_text(aes(y = nb_sujets/2 + c(0, cumsum(nb_sujets)[-length(nb_sujets)]), 
                label = nb_sujets/100 ), size=5, color = "black") +
  scale_fill_brewer("Depression",palette = "PuBu")

plot_grid(pie_epice, barPlot_TS, pie_deptot)
#################################################################################################################################"

## graphes variables croisées

###activpro et srh
ggplot(baseline,aes(baseline$activproBis, baseline$srhNum, fill = baseline$activproBis)) + geom_boxplot() + xlab("Acvtivité Professionnelle") + 
  ylab("Santé perçue") +
  ylim(5,10) +
  scale_fill_jama(name="",
                  breaks=c("R", "I", "C","T","NSP",NA),
                  labels=c("Retraités", "Inactifs", "Chômeurs","Travailleurs","Ne Sais Pas","NA"))


###age et activpro
ggplot(baseline, aes(factor(baseline$activproBis,c("R","I","C","T","NSP",NA)), baseline$age0, fill = baseline$activproBis))+ 
  geom_boxplot() +
  xlab("Activité Professionelle") +
  ylab("Âge") +
  scale_fill_jama(name="",
                  breaks=c("R", "I", "C","T","NSP",NA),
                  labels=c("Retraités", "Inactifs", "Chômeurs","Travailleurs","Ne Sais Pas","NA"))


### age activpro et sexe
ggplot(baseline, aes(baseline$activproBis, baseline$age0, fill = baseline$sexe))+
  geom_boxplot()+
  scale_fill_jama(name="",
                  breaks=c("F", "H",NA),
                  labels=c("Femmes", "Hommes", NA)) +
  xlab("Activité Professionnelle")+
  ylab("Âge")



#################################################################################################################################

## Missing Values
gg_miss_var(database) + scale_x_discrete(  labels = c("Âge", "Indice de Masse Corporelle","nepp3","Sexe", 
                                                      "Activité Professionnelle (classif Manuelle)", "Tabac",  
                                                      "Activité Professionnelle (CAH)","Catégorie Socio-Professionnelle",
                                                      "Situation Familliale", "Diabète", "Infarctus du Myocarde",
                                                      "Depression",  "AVC", "Angine de Poitrine","Autre Pb Cardiaque",
                                                      "Phlébite","Embollie Pulmonaire","Maladie des Artères", 
                                                      "Hypertenssion Arterielle","Niveau d'Education","Cancer", 
                                                      "Sport" ,"Score Epice de Précarité","Alcool (Nb de verres)", 
                                                      "Pontage/Angioplastie/Stent","Tentative de Suicide")) +
  ylab("Nombre de Donées Manquantes")

table_na = kable(df_na[,2:4],"latex", booktabs = T )

###################################################################################################################################"

## flowchart
flowchart = DiagrammeR::grViz("
digraph graph2 {
                              
                              
                              node [shape = box, width = 5]
                              a [label = '@@1']
                              b [label = '@@2']
                              c [label = '@@3']
                              m [label = '@@13']
                              
                              node [shape = oval, width = 0.9]
                              d [label = '@@4']
                              e [label = '@@5']
                              o [label = '@@@15]
                              p [label = '@@@16]                  
                              
                              node [shape = circle, width = 0.9]
                              f [label = '@@6']
                              g [label = '@@7']
                              h [label = '@@8']
                              i [label = '@@9']
                              j [label = '@@10']
                              k [label = '@@11']
                              l [label = '@@12']
                              n [label = '@@14']    
                              
                              a -> b -> c -> d -> e 
                              e -> f
                              e -> g
                              e -> h
                              e -> i            
                              e -> j
                              e -> k
                              e -> l
                              l -> m
                              n -> o
                              n -> p
                              
                              
                              }
                              
                              [1]: 'n = 10157 participants à epp3'
                              [2]: '26 exclus NA  Santé perçue : n = 10131'
                              [3]: '16 exlus NA  activproCAH : n = 10118'
                              [4]: 'Analyses univariées'
                              [5]: 'Analyses mutivariées'
                              [6]: 'Modèle 1'
                              [7]: 'Modèle 2'
                              [8]: 'Modèle 3'
                              [9]: 'Modèle 4'
                              [10]: 'Modèle 5'
                              [11]: 'Modèle 6'
                              [12]: 'Modèle 7'
                              [13]: 'Diagnostique du Modèle, exclusions leviers ou outliers'
                              [14]: 'Modèle final : Logit Pénalisé'
                              [15]: 'Analyses en Sous Groupes'
                              [16]: 'Analyses de Sensibilité'
                              
                              ")










