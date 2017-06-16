library(FactoMineR)
occmot<- read.table("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/Carac_Ter_Panicol.txt", header=TRUE, sep="\t")

rownames(occmot)<-occmot[,1]
infos(occmot)
occmot[,2:5]<-lapply(occmot[,2:5], factor)


#on garde que les mots cités plus de 4 fois #
occmot2<- occmot[ ,which(colSums(occmot[ , 5:103])>=4)]

#mise en oeuvre de l'AFC#
occmotbis<-occmot4
occmotbis[,1:5]<-lapply(occmotbis[,1:5],function(x)  as.numeric(factor(x)))
occmotCA <-CA(occmotbis, ncp=5, row.sup=NULL, col.sup=1:4)

#Valeur propres des dimensions#
barplot(occmotCA$eig[ , 1], main="valeurs propres", names.arg=1:nrow(occmotCA$eig))

#Etude des marges#
pdf("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol/Carac_Ter_Panicol_marge.pdf")
par(mfrow=c(1,2))
barplot(occmotCA$call$marge.col[order(occmotCA$call$marge.col], cex.names=0.5, horiz=TRUE, las=1)
                                barplot(occmotCA$call$marge.row[order(occmotCA$call$marge.row)],cex.names=0.5, horiz=TRUE, las=1) 
                                
                                dev.off()
                                
                                #graphe des colonnes et des lignes#
                                pdf("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/Carac_Ter_Panicol_CA_indiv.pdf")
                                plot.CA (occmotCA, axes=c(1,2), invisible= "col", col.row="orange", col.col = "darkgreen", cex=0.8)
                                dev.off()
                                
                                
                                pdf("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/Carac_Ter_Panicol_CA_sup.pdf")
                                plot.CA (occmotCA, axes=c(1,2), invisible= "col.sup", col.row="orange", col.col.sup= "darkred", cex=0.8)
                                dev.off()
                                
                                as.data.frame(occmot2)
                                catdes(occmot2, num.var=1)
                                
                                
                                
                                
                                
                                
                                
                                
                                #Caractérisation des génotypes mot+ de 7#
                                carac.gen<- read.table("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/carac_gen.txt", header=TRUE, row.name=1, sep="\t")
                                res.ca.gen<-CA(carac.gen, col.sup=1:3)
                                
                                
                                #Caractérisation des boulangers#
                                carac.boul<- read.table("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/carac_boul.txt", header=TRUE, row.name=1, sep="\t")
                                res.ca.boul<-CA(carac.boul, col.sup=1:4)
                                
                                
                                #Caractérisation des environnements#
                                carac.env<- read.table("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/carac_env.txt", header=TRUE, row.name=1, sep="\t")
                                res.ca.env<-CA(carac.env, col.sup=1:5)
                                
                                
                                #Caractérisation des années#
                                carac.ann<- read.table("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/carac_ann.txt", header=TRUE, row.name=1, sep="\t")
                                res.ca.ann<-CA(carac.ann, col.sup=1:2)
                                
                                
                                #export des 4 représentations)
                                pdf("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/Carac_Ter.pdf")
                                mat=matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)
                                layout(mat)
                                plot.CA(res.ca.gen, invisible="row", cex=0.5)
                                plot.CA(res.ca.boul, invisible="row",cex=0.5)
                                plot.CA(res.ca.env, invisible="row",cex=0.5)
                                plot.CA(res.ca.ann, invisible="row",cex=0.5)
                                dev.off() 
                                
                                
                                #Caractérisation des génotypes mot+ de 10#
                                carac.gen<- read.table("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/carac_gen.txt", header=TRUE, row.name=1, sep="\t")
                                carac.gen2<- carac.gen[ , c(1:3, 15:ncol(carac.gen))]
                                res.ca.gen<-CA(carac.gen2, col.sup=1:3)
                                
                                
                                #Caractérisation des boulangers#
                                carac.boul<- read.table("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/carac_boul.txt", header=TRUE, row.name=1, sep="\t")
                                carac.boul2<- carac.boul[ , c(1:4, 16:ncol(carac.boul))]
                                res.ca.boul<-CA(carac.boul2, col.sup=1:4)
                                
                                
                                #Caractérisation des environnements#
                                carac.env<- read.table("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/carac_env.txt", header=TRUE, row.name=1, sep="\t")
                                carac.env2<- carac.env[ , c(1:5, 17:ncol(carac.env))]
                                res.ca.env<-CA(carac.env2, col.sup=1:5)
                                
                                
                                #Caractérisation des années#
                                carac.ann<- read.table("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/carac_ann.txt", header=TRUE, row.name=1, sep="\t")
                                carac.ann2<- carac.ann[ , c(1:2, 14:ncol(carac.ann))]
                                res.ca.ann<-CA(carac.ann2, col.sup=1:2)
                                
                                
                                #export des 4 représentations#
                                pdf("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/Carac_Ter.pdf")
                                mat=matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)
                                layout(mat)
                                plot.CA(res.ca.gen, invisible="row", cex=0.5, title="Corrélation avec le facteur génétique")
                                plot.CA(res.ca.boul, invisible="row",cex=0.5,title="Corrélation avec le facteur boulanger")
                                plot.CA(res.ca.env, invisible="row",cex=0.5,title="Corrélation avec le facteur environnement")
                                plot.CA(res.ca.ann, invisible="row",cex=0.5,title="Corrélation avec le facteur année")
                                dev.off() 
                                
                                
                                
                                #export des 4 représentations 2° plan#
                                pdf("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/Carac_Ter_Plan2.pdf")
                                mat=matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)
                                layout(mat)
                                plot.CA(res.ca.gen, invisible="row", cex=0.5, title="Corrélation avec le facteur génétique", axes=c(2,3))
                                plot.CA(res.ca.boul, invisible="row",cex=0.5,title="Corrélation avec le facteur boulanger",axes=c(2,3))
                                plot.CA(res.ca.env, invisible="row",cex=0.5,title="Corrélation avec le facteur environnement",axes=c(2,3))
                                plot.CA(res.ca.ann, invisible="row",cex=0.5,title="Corrélation avec le facteur année",axes=c(2,3))
                                dev.off() 
                                
                                #export des 4 représentations 3° plan#
                                pdf("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/Carac_Ter_Plan3.pdf")
                                mat=matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)
                                layout(mat)
                                plot.CA(res.ca.gen, invisible="row", cex=0.5, title="Corrélation avec le facteur génétique", axes=c(3,4))
                                plot.CA(res.ca.boul, invisible="row",cex=0.5,title="Corrélation avec le facteur boulanger",axes=c(3,4))
                                plot.CA(res.ca.env, invisible="row",cex=0.5,title="Corrélation avec le facteur environnement",axes=c(3,4))
                                plot.CA(res.ca.ann, invisible="row",cex=0.5,title="Corrélation avec le facteur année",axes=c(3,4))
                                dev.off() 
                                
                                #évolution seuil choix mots#
                                carac.gen<- read.table("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/carac_gen.txt", header=TRUE, row.name=1, sep="\t")
                                carac.gen2<- carac.gen[ , c(1:3, 15:ncol(carac.gen))]
                                res.ca.gen2<-CA(carac.gen2, col.sup=1:3)
                                carac.gen3<- carac.gen[ , c(1:3, 20:ncol(carac.gen))]
                                res.ca.gen3<-CA(carac.gen3, col.sup=1:3)
                                carac.gen4<- carac.gen[ , c(1:3, 25:ncol(carac.gen))]
                                res.ca.gen4<-CA(carac.gen4, col.sup=1:3)
                                carac.gen5<- carac.gen[ , c(1:3, 30:ncol(carac.gen))]
                                res.ca.gen5<-CA(carac.gen5, col.sup=1:3)
                                
                                pdf("C:/Users/ITAB1/Desktop/AnalyseR/Carac_Ter_Panicol_AFC/evol_seuil_mot.pdf")
                                mat=matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=TRUE)
                                layout(mat)
                                plot.CA(res.ca.gen2, invisible="row", title="les 33 mots les plus cités",cex=0.7)
                                plot.CA(res.ca.gen3, invisible="row",title="les 28 mots les plus cités",cex=0.7)
                                plot.CA(res.ca.gen4, invisible="row",title="les 23 mots les plus cités",cex=0.7)
                                plot.CA(res.ca.gen5, invisible="row",title="les 18 mots les plus cités",cex=0.7)
                                dev.off() 