## Graphes 3.1.1
summary(baseline$activproBis)
baseline$activproBis = factor(baseline$activproBis, c("T","R","C","I","NSP"))
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
  annotate("text", x=6, y=200, label= "16") + 
  scale_fill_brewer("Activité Professionnelle",palette = "Blues")



library(cowplot)
window()
df1 = ggplot(baseline, aes(baseline$age0)) + geom_histogram(col = "white", fill = "black", bins  = 20) + 
  xlab("n = 10157") + ylab("") + ggtitle("Baseline") + 
  theme_classic2() + 
  theme(plot.title = element_text(family = "serif", face= "bold", hjust = 0.5 , size = 15),
  axis.title.x = element_text(family = "serif", size = 10))

df2 = ggplot(baseline[which(baseline$activproBis == "T"),], aes(baseline[which(baseline$activproBis == "T"), "age0"])) + 
  geom_histogram(col = "white", fill = "#5472AE", bins  = 20)  + xlab("n = 5666")+ ylab("") + ggtitle("Travailleurs")+
  theme_classic2() + 
  theme(plot.title = element_text(family = "serif", face= "bold", hjust =0.5, size = 15),
        axis.title.x = element_text(family = "serif", size = 10))

df3 = ggplot(baseline[which(baseline$activproBis == "R"),], aes(baseline[which(baseline$activproBis == "R"), "age0"])) + 
  geom_histogram(col = "white", fill = "#FFDE75", bins  = 20) + xlab("n = 3209")+ ylab("") + ggtitle("Retraités")+
  theme_classic2() + 
  theme(plot.title = element_text(family = "serif", face= "bold", hjust =0.5, size = 15 ),
        axis.title.x = element_text(family = "serif", size = 10))

df4 = ggplot(baseline[which(baseline$activproBis == "C"),], aes(baseline[which(baseline$activproBis == "C"), "age0"])) + 
  geom_histogram(col = "white", fill = "#DD985C", bins  = 20)+ xlab("n = 883")+ ylab("") + ggtitle("Chômeurs")+
  theme_classic2() + 
  theme(plot.title = element_text(family = "serif", face= "bold", hjust =0.5, size = 15 ),
        axis.title.x = element_text(family = "serif", size = 10))

df5 = ggplot(baseline[which(baseline$activproBis == "I"),], aes(baseline[which(baseline$activproBis == "I"), "age0"])) + 
  geom_histogram(col = "white", fill = "#82C46C", bins  = 20)+ xlab("n = 270")+ ylab("") + ggtitle("Inactifs")+
  theme_classic2() +
  theme(plot.title = element_text(family = "serif", face= "bold", hjust =0.5, size = 15 ),
        axis.title.x = element_text(family = "serif", size = 10))

df6 = ggplot(baseline[which(baseline$activproBis == "NSP"),], aes(baseline[which(baseline$activproBis == "NSP"), "age0"])) + 
  geom_histogram(col = "white", fill = "#723E64", bins  = 20) + xlab("n = 113")+ ylab("") + ggtitle("NSP")+ 
  theme_classic2() + 
  theme(plot.title = element_text(family = "serif", face= "bold", hjust =0.5, size = 15 ),
        axis.title.x = element_text(family = "serif", size = 10))

#df7 = ggplot(baseline[which(is.na(baseline$activproBis)),], aes(baseline[which(is.na(baseline$activproBis)), "age0"])) + geom_histogram(col = "white", fill = "brown", bins  = 20) + xlab("n = 16")+ ylab("Sujets NA") + ggtitle("Age NA")
plot_grid(df1, df2, df3, df4, df5, df6)

#######################################################################################################################################

##graphe ACM
library(factoextra)
plot.MCA(ACM_activpro, choix = "var", title = "Graphe des Variables")
plot.MCA(ACM_activpro, choix = "ind", invisible = "var", title = "Graphe des Individus")

fviz_mca_biplot(ACM_activpro, label ="var", col.var = "cos2", select.var = list(contrib = 15)) + 
  labs(title = "Graphe des Individus et des Modalités") + 
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = .5, size = 15)) 

#######################################################################################################################################

##graphes CAH
par(mfrow=c(1,2))
plot(cah_activpro, choice = "bar", cex = 0.01, title = "Intertie inter-cluster")
plot(cah_activpro, choice = "tree", cex = 0.01, title = "Dendrogramme",tree.barplot = F)

plot.MCA(ACM_activpro, choix = "ind", invisible = "var", title  = "Graphe des Individus") 
plot(cah_activpro, choice = "map")

#######################################################################################################################################

## graphe comparaison activproBis et activproCAH

baseline$activproCAH = factor(baseline$activproCAH, c("T","R","C","I","NSP"))
activproCAH_prop = (table(baseline$activproCAH, useNA = "always")/10157)*100

baseline$activproBis = factor(baseline$activproBis, c("T","R","C","I","NSP"))
actvproBis_prop = (table(baseline$activproBis, useNA = "always")/10157)*100

comp_activpro_prop = rbind(actvproBis_prop,activproCAH_prop)

barplot(as.matrix(comp_activpro_prop), col = c("#22427C","#318CE7") , 
        beside = TRUE, 
        main = "", 
        xlab='Modalités', ylab='Pourcentage')
legend(x = 13, y = 60, legend = c("Codage Manuel","CAH"), 
       col='black', pch = c(22), pt.bg = c("#22427C","#318CE7"), bty ='n')

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
                label = nb_sujets/100 ), size=5, color = "black") +
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
  geom_histogram(fill = "#FEE347", col = "white",bins = 50) + 
  #scale_color_aaas(name = "sexe")+
  xlab("Âge") + 
  geom_vline(xintercept = 54.47,  color = "black", size=1) +
  annotate(geom = "text",x = 56.5, y = 500, label = "Q1")+
  geom_vline(xintercept = 59.11,  color = "black", size=1) +
  annotate(geom = "text",x = 61.5, y = 550, label = "Med") +
  geom_vline(xintercept = 63.66,  color = "black", size=1) +
  annotate(geom = "text",x = 65.5, y = 500, label = "Q3")

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
                                                          "CAP/Brevet des collèges",
                                                          "Baccalauréat",
                                                          "Bac+2",
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
                label = nb_sujets/100 ), size=4, color = "black") +
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
                label = nb_sujets/100 ), size=5, color = "black") +
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
                label = nb_sujets/100 ), size=5, color = "black") +
  scale_fill_brewer("Sujet Hypertendu",palette = "PuRd")



###alcool
df_alcool = data.frame(
  alcool = c("0", "1-2", "3+","NA"),
  nb_sujets = c(length(which(baseline$alcoolCut == "0")),
                length(which(baseline$alcoolCut == "1-2")),
                length(which(baseline$alcoolCut == "3+")),
                length(which(is.na(baseline$alcoolCut)))))
df_alcool$prop = round(((df_alcool$nb_sujets)/10157)*100,2)

df_alcool$alcool = factor(df_alcool$alcool, c("NA", "3+", "1-2", "0"))

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

plot_grid(hist_bmi, flip_barPlot_tabac, flip_barPlot_alcool, pie_sport2, pie_diab, pie_cancer, pie_hta)

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
  scale_fill_brewer("Précarité \n (Basé sur le score EPICE)", palette = "Pastel2" )

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


###Stress
hist_stress = ggplot(baseline, aes(baseline$stress)) + 
  geom_histogram(fill = "steelblue", col = "white",bins = 50) + 
  xlab("Score de stress perçu (PSS4)") + ylab("")+
  geom_vline(xintercept = 2,  color = "black", size=1) +
  annotate(geom = "text",x = 2.6, y = 2000, label = "Q1")+
  geom_vline(xintercept = 4,  color = "black", size=1) +
  annotate(geom = "text",x = 4.8, y = 2250, label = "Med") +
  geom_vline(xintercept = 6,  color = "black", size=1) +
  annotate(geom = "text",x = 6.8, y = 2500, label = "Q3")

plot_grid(pie_epice,hist_stress,pie_deptot, pie_deptot, pie_deptot)
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
                              
                              
                              node [shape = box, width = 3]
                              a [label = '@@1']
                              b [label = '@@2']
                              c [label = '@@3']
s [label = '@@19']
t [label = '@@20']
u [label = '@@21']
v [label = '@@22']
w [label = '@@23']
x [label = '@@24']
y [label = '@@25']
z [label = '@@26']
aa [label = '@@27']
bb [label = '@@28']
                              
                              
                              
                              node [shape = oval, width = 0.9]
                              d [label = '@@4']
                              e [label = '@@5']
                              l [label = '@@12']
                              n [label = '@@14']
                              o [label = '@@15']
                              
                              
                              node [shape = circle, width = 0.9]
                              f [label = '@@6']
                              g [label = '@@7']
                              h [label = '@@8']
                              i [label = '@@9']
                              j [label = '@@10']
                              k [label = '@@11']
                              m [label = '@@13']
                              r [label = '@@18']
                              
                              
                              node [shape = diamond, width = 0.9]
                              p [label = '@@16']
                              q [label = '@@17']

                              
a -> b -> c -> d -> e 
e->s
s->f
e->t
t->g
e->u
u->h
u->i
u->j
h->v
i->v
j->v
v->k
k->l
l->w
w->m
m->n
n->p
n->q
p->x
p->y
q->z
q->aa
m->o
o->bb
bb->r
                            

                              
                              
                              }
                              
                              [1]: 'Participants à EPP3: n = 10157'
                              [2]: '26 exclus NA  Santé perçue'
                              [3]: '16 exclus NA  activité professionnelle'
                              [4]: 'Analyses univariées'
                              [5]: 'Analyses mutivariées'
                              [6]: 'Modèle 1'
                              [7]: 'Modèle 2'
                              [8]: 'AIC 1'
                              [9]: 'AIC 2'
                              [10]: 'AIC 3'
                              [11]: 'Modèle 3'
                              [12]: 'Detection obs atypiques'
                              [13]: 'Modèle final'
                              [14]: 'Analyses en Sous Groupes'
                              [15]: 'Analyse de sensibilité'
                              [16]: 'Pathologies lourdes'
                              [17]: 'Précarité'
                              [18]: 'Modèle CAH'
[19]: 'n = 10 131'
[20]: 'n = 10 131'
[21]: 'n = 7 501'
[22]: 'n = 7 501'
[23]: 'n =7 454'
[24]: 'OUI : n = 1 207'
[25]: 'NON : n = 7407'
[26]: 'OUI : n = 1 744'
[27]: 'NON : n = 5757'
[28]: 'n = 7 454'
                              
                              ")

flowchart


###############################################################################################################################################

#Diagnostique du modele
plot(x = database$stress, y = logit , main = "Vérification de la linéarité du lien entre le logit et le stress", xlab = "Stress PSS4" ,
     ylab = 'logit (1.05 - 0.19 Stress)')
plot(model_final, 4)
abline(h = 0.0004, col = "blue")

window()
par(mfrow = c(3,2))
plot(model_final, 1)
plot(model_final, 2)
plot(model_final, 3)
plot(model_final, 4)
plot(model_final, 5)
plot(model_final, 6)

###################################################################################################################################
 
## graphes pour pres

plot(cah_activpro, choice = "3D.map")
library(forestmodel)
forest_model(model_final)


library(broom)
tmp <- tidy(model_final, conf.int = TRUE, exponentiate = TRUE)
str(tmp)

tmp$term = c("Intercepte", "Activité pro: Chômeurs" , "Activité pro: Inactifs" , "Activité pro: NSP", "Activité Pro: Retraités",
             "Sexe: Femmes", "CSP: Basse", "CSP: Elevée", "CSP: Inactifs", "CSP: Sans Emplois", "Education: Analphabète",
             "Education: Bac + 2 ", "Education: Baccalauréat", "Eduation: Licence/Maîtrise/+", "Education: Sans diplômes",
             "Situattion Familiale; Famille", "Situation Familiale: Seul", "Activité Sportive: 2 fois par semaine", 
             "Activité sportive: 3+ fois par semaine",  "IMC: Obésité", "IMC: surpoids", "Hypertension Arterielle: Oui", 
             "Tabac: Anciens Fumeurs", "Tabac: Arrêt en cours", "Tabac: Fumeurs", "Depression: Oui", "Stress perçu")
library(ggplot2)
ggplot(tmp) + aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high) + 
  geom_vline(xintercept = 1) + geom_errorbarh() + geom_point() + 
  scale_x_log10()
