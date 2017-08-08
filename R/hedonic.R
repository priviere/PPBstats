#' Run hedonic test analysis
#'
#' @description
#' \code{hedonic} runs hedonic analysis
#' 
#' 
#' 
# # Nettoyage
# rm(list=ls())
# 
# # importation des données
# d <-read.csv("2016_organo_foirebio-villeneuve_7082016.csv",sep = "\t", header = TRUE)
# code <-read.csv("code_tomates.csv",sep = "\t", header = TRUE)
# 
# # chargement des outils
# library(ggplot2)
# library(dplyr)
# library(plyr)
# library(agricolae)
# 
# # rassemblement des données grâce à la colonne commune 'code'
# a <- join(code,d, by="code")
# 
# # mise en forme des adjectifs
# # création du data frame qui contiendra les valeurs logiques (0 ou 1) pour chacun des adjectifs
# adjectif = as.vector (as.character(a$adjectif))
# split = unlist(strsplit(adjectif, "[;]"))
# split = unique(split)
# split = sort(split)
# df = matrix(0, ncol=length(split), nrow=nrow(a))
# df = as.data.frame(df)
# colnames(df) = split
# out = cbind.data.frame(a,df)
# 
# # attribution des valeurs logiques (0 ou 1)
# for (i in 1:nrow(out)){
#   vec_adj = out[i,"adjectif"]
#   vec_adj = unlist(strsplit(as.character(vec_adj), ";"))
#   
#   for (j in 1:length(vec_adj)) {
#     e = vec_adj[j]
#     if (length(e)>0) {
#       if (!is.na(e)) { out[i,e]= 1 }
#     }
#   }
# }
# 
# #visualisation du data frame créé
# edit(out)
# 
# #définition des types de données
# code = as.factor(out$code)
# out$env = as.factor(out$environnement)
# out$var = as.factor(out$variete)
# out$note = as.numeric (as.character(out$note))
# id_juge = as.factor (out$id_juge)
# sex = as.factor(out$sexe)
# age = as.factor(out$age)
# regime = as.factor(out$ab_ou_non)
# circuit = as.factor(out$circuit)
# adj = as.character.factor(out$adjectif)
# 
# l<-nlevels(out$code)
# for (i in 1:l){
#   n<-nrow(out$id_juge==i)
#   if (n<l){
#     i=j
#     i=i+1
#   }
#   j=j+1
# }
# levels(j)
# 
# 
# ###    Stats descriptives   ###
# 
# # Histogramme des moyennes par variété
# #g<-ggplot(data=out, aes(var,note, color=env)) +
# #  geom_bar(aes(fill=env),position="dodge",na.rm=TRUE, colour='black',width=0.6) +
# #  stat_identity()+
# #  ggtitle("Appréciation globale moyenne par variété") +
# #  xlab("Variétés") + ylab("Appréciation moyenne globale")
# 
# # Boxplot des moyennes par variété
# b1<-ggplot(data=out, aes(x = var, y = note, color=var)) +
#   geom_boxplot(na.rm=TRUE) +
#   facet_wrap(c("env"),nrow = 2) +
#   ggtitle("Appréciation globale par variété") +
#   xlab("Variétés") + ylab("Appréciation globale") +
#   theme(axis.text.x=element_text(size=8,angle=90))
# b1 
# 
# ###     ANOVA     ####
# 
# model <- lm(note~env+var+env*var, data = out)
# anova(model)
# 
# # analyse des résidus
# residus=rstudent(model)
# par(mfrow=c(2,2))
# plot(model,which=1:4)
# 
# # test hsd
# hsd<- HSD.test(model, "var")
# 
# # boxplot avec les lettres
# par(mfrow=c(1,1))
# bp <- boxplot(note~var, data=out)
# lettres<-hsd$groups$M
# names(lettres)<-gsub(pattern=" ", replacement="", x=hsd$groups$trt)
# moys<- hsd$groups$means
# names(moys)<-gsub(pattern=" ", replacement="", x=hsd$groups$trt)
# text(1:9,moys[bp$names], labels=lettres[bp$names])
# 
# # LSD test 'Least Significance Difference'
# gLSDplot=function(model,x,y,adjust){
#   #model : modèle complet, x : facteur étudié, y : variable étudiée, adjust : ajustement ("bonferroni" / "holm" / "none" / "hochberg" / "BH" / "BY")
#   
#   LSD=LSD.test(model, x, p.adj=adjust) #pareil pour p.adj : "bonferroni" / "holm" / "none" / "hochberg" / "BH" / "BY"
#   LSD$groups$trt=factor(LSD$groups$trt, levels=LSD$groups$trt)
#   
#   if (x=="var"){
#     LSD$groups$var=LSD$groups$trt
#     p=ggplot(data=LSD$groups, aes(x=var, y=means)) # ,color=var))
#     p=p + geom_text(data=LSD$groups, aes(x=var, y=means, label=M, vjust=-1))
#     p=p + labs(title = paste("Groupes de variétés significativement differents pour la variable :",y))
#   }
#   else if (x=="env"){
#     LSD$groups$env=LSD$groups$trt
#     p=ggplot(data=LSD$groups, aes(x=env, y=means)) # ,color=env))
#     p=p + geom_text(data=LSD$groups, aes(x=env, y=means, label=M, vjust=-1))
#     p=p + labs(title = paste("Groupes d'environnements significativement differents pour la variable :",y))
#   }
#   
#   p=p + geom_bar(stat="identity") #aes(fill=var), position="dodge")
#   p=p + theme(legend.position="none", axis.text.x=element_text(size=15,angle=90), plot.title = element_text(lineheight=.8, face="bold"))
#   p=p + scale_y_continuous(name=y)
#   
#   return(p)
# } 
# 
# model <- lm(note~env+var+env*var, data = out)
# gLSDplot(model,x="var", y="note", adjust = "bonferroni")
# 
# library(FactoMineR)
# occmot<- read.table("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/Carac_Ter_Panicol.txt", header=TRUE, sep="\t")
# 
# rownames(occmot)<-occmot[,1]
# infos(occmot)
# occmot[,2:5]<-lapply(occmot[,2:5], factor)
# 
# 
# #on garde que les mots cités plus de 4 fois #
# occmot2<- occmot[ ,which(colSums(occmot[ , 5:103])>=4)]
# 
# #mise en oeuvre de l'AFC#
# occmotbis<-occmot4
# occmotbis[,1:5]<-lapply(occmotbis[,1:5],function(x)  as.numeric(factor(x)))
# occmotCA <-CA(occmotbis, ncp=5, row.sup=NULL, col.sup=1:4)
# 
# #Valeur propres des dimensions#
# barplot(occmotCA$eig[ , 1], main="valeurs propres", names.arg=1:nrow(occmotCA$eig))
# 
# #Etude des marges#
# pdf("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol/Carac_Ter_Panicol_marge.pdf")
# par(mfrow=c(1,2))
# barplot(occmotCA$call$marge.col[order(occmotCA$call$marge.col], cex.names=0.5, horiz=TRUE, las=1)
#                                 barplot(occmotCA$call$marge.row[order(occmotCA$call$marge.row)],cex.names=0.5, horiz=TRUE, las=1) 
#                                 
#                                 dev.off()
#                                 
#                                 #graphe des colonnes et des lignes#
#                                 pdf("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/Carac_Ter_Panicol_CA_indiv.pdf")
#                                 plot.CA (occmotCA, axes=c(1,2), invisible= "col", col.row="orange", col.col = "darkgreen", cex=0.8)
#                                 dev.off()
#                                 
#                                 
#                                 pdf("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/Carac_Ter_Panicol_CA_sup.pdf")
#                                 plot.CA (occmotCA, axes=c(1,2), invisible= "col.sup", col.row="orange", col.col.sup= "darkred", cex=0.8)
#                                 dev.off()
#                                 
#                                 as.data.frame(occmot2)
#                                 catdes(occmot2, num.var=1)
#                                 
#                                 
#                                 
#                                 
#                                 
#                                 
#                                 
#                                 
#                                 #Caractérisation des génotypes mot+ de 7#
#                                 carac.gen<- read.table("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/carac_gen.txt", header=TRUE, row.name=1, sep="\t")
#                                 res.ca.gen<-CA(carac.gen, col.sup=1:3)
#                                 
#                                 
#                                 #Caractérisation des boulangers#
#                                 carac.boul<- read.table("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/carac_boul.txt", header=TRUE, row.name=1, sep="\t")
#                                 res.ca.boul<-CA(carac.boul, col.sup=1:4)
#                                 
#                                 
#                                 #Caractérisation des environnements#
#                                 carac.env<- read.table("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/carac_env.txt", header=TRUE, row.name=1, sep="\t")
#                                 res.ca.env<-CA(carac.env, col.sup=1:5)
#                                 
#                                 
#                                 #Caractérisation des années#
#                                 carac.ann<- read.table("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/carac_ann.txt", header=TRUE, row.name=1, sep="\t")
#                                 res.ca.ann<-CA(carac.ann, col.sup=1:2)
#                                 
#                                 
#                                 #export des 4 représentations)
#                                 pdf("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/Carac_Ter.pdf")
#                                 mat=matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)
#                                 layout(mat)
#                                 plot.CA(res.ca.gen, invisible="row", cex=0.5)
#                                 plot.CA(res.ca.boul, invisible="row",cex=0.5)
#                                 plot.CA(res.ca.env, invisible="row",cex=0.5)
#                                 plot.CA(res.ca.ann, invisible="row",cex=0.5)
#                                 dev.off() 
#                                 
#                                 
#                                 #Caractérisation des génotypes mot+ de 10#
#                                 carac.gen<- read.table("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/carac_gen.txt", header=TRUE, row.name=1, sep="\t")
#                                 carac.gen2<- carac.gen[ , c(1:3, 15:ncol(carac.gen))]
#                                 res.ca.gen<-CA(carac.gen2, col.sup=1:3)
#                                 
#                                 
#                                 #Caractérisation des boulangers#
#                                 carac.boul<- read.table("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/carac_boul.txt", header=TRUE, row.name=1, sep="\t")
#                                 carac.boul2<- carac.boul[ , c(1:4, 16:ncol(carac.boul))]
#                                 res.ca.boul<-CA(carac.boul2, col.sup=1:4)
#                                 
#                                 
#                                 #Caractérisation des environnements#
#                                 carac.env<- read.table("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/carac_env.txt", header=TRUE, row.name=1, sep="\t")
#                                 carac.env2<- carac.env[ , c(1:5, 17:ncol(carac.env))]
#                                 res.ca.env<-CA(carac.env2, col.sup=1:5)
#                                 
#                                 
#                                 #Caractérisation des années#
#                                 carac.ann<- read.table("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/carac_ann.txt", header=TRUE, row.name=1, sep="\t")
#                                 carac.ann2<- carac.ann[ , c(1:2, 14:ncol(carac.ann))]
#                                 res.ca.ann<-CA(carac.ann2, col.sup=1:2)
#                                 
#                                 
#                                 #export des 4 représentations#
#                                 pdf("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/Carac_Ter.pdf")
#                                 mat=matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)
#                                 layout(mat)
#                                 plot.CA(res.ca.gen, invisible="row", cex=0.5, title="Corrélation avec le facteur génétique")
#                                 plot.CA(res.ca.boul, invisible="row",cex=0.5,title="Corrélation avec le facteur boulanger")
#                                 plot.CA(res.ca.env, invisible="row",cex=0.5,title="Corrélation avec le facteur environnement")
#                                 plot.CA(res.ca.ann, invisible="row",cex=0.5,title="Corrélation avec le facteur année")
#                                 dev.off() 
#                                 
#                                 
#                                 
#                                 #export des 4 représentations 2° plan#
#                                 pdf("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/Carac_Ter_Plan2.pdf")
#                                 mat=matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)
#                                 layout(mat)
#                                 plot.CA(res.ca.gen, invisible="row", cex=0.5, title="Corrélation avec le facteur génétique", axes=c(2,3))
#                                 plot.CA(res.ca.boul, invisible="row",cex=0.5,title="Corrélation avec le facteur boulanger",axes=c(2,3))
#                                 plot.CA(res.ca.env, invisible="row",cex=0.5,title="Corrélation avec le facteur environnement",axes=c(2,3))
#                                 plot.CA(res.ca.ann, invisible="row",cex=0.5,title="Corrélation avec le facteur année",axes=c(2,3))
#                                 dev.off() 
#                                 
#                                 #export des 4 représentations 3° plan#
#                                 pdf("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/Carac_Ter_Plan3.pdf")
#                                 mat=matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)
#                                 layout(mat)
#                                 plot.CA(res.ca.gen, invisible="row", cex=0.5, title="Corrélation avec le facteur génétique", axes=c(3,4))
#                                 plot.CA(res.ca.boul, invisible="row",cex=0.5,title="Corrélation avec le facteur boulanger",axes=c(3,4))
#                                 plot.CA(res.ca.env, invisible="row",cex=0.5,title="Corrélation avec le facteur environnement",axes=c(3,4))
#                                 plot.CA(res.ca.ann, invisible="row",cex=0.5,title="Corrélation avec le facteur année",axes=c(3,4))
#                                 dev.off() 
#                                 
#                                 #évolution seuil choix mots#
#                                 carac.gen<- read.table("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/carac_gen.txt", header=TRUE, row.name=1, sep="\t")
#                                 carac.gen2<- carac.gen[ , c(1:3, 15:ncol(carac.gen))]
#                                 res.ca.gen2<-CA(carac.gen2, col.sup=1:3)
#                                 carac.gen3<- carac.gen[ , c(1:3, 20:ncol(carac.gen))]
#                                 res.ca.gen3<-CA(carac.gen3, col.sup=1:3)
#                                 carac.gen4<- carac.gen[ , c(1:3, 25:ncol(carac.gen))]
#                                 res.ca.gen4<-CA(carac.gen4, col.sup=1:3)
#                                 carac.gen5<- carac.gen[ , c(1:3, 30:ncol(carac.gen))]
#                                 res.ca.gen5<-CA(carac.gen5, col.sup=1:3)
#                                 
#                                 pdf("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/evol_seuil_mot.pdf")
#                                 mat=matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)
#                                 layout(mat)
#                                 plot.CA(res.ca.gen2, invisible="row", title="les 33 mots les plus cités",cex=0.7)
#                                 plot.CA(res.ca.gen3, invisible="row",title="les 28 mots les plus cités",cex=0.7)
#                                 plot.CA(res.ca.gen4, invisible="row",title="les 23 mots les plus cités",cex=0.7)
#                                 plot.CA(res.ca.gen5, invisible="row",title="les 18 mots les plus cités",cex=0.7)
#                                 dev.off() 