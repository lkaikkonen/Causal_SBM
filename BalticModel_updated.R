### BALTIC SEA SBM BN MODEL 
## Kaikkonen L. 
# Edited on 2021-01

# load packages

library(bnlearn)
library(bnviewer)
library(Rgraphviz)
library(DiagrammeR)
library(gRain)
library(gRbase)
library(ggplot2)

# Import node names and states


nodes1<-read.table(~"Node_BalticSeaERA_benthos.csv", header=FALSE,fill=TRUE, sep=',', stringsAsFactors = FALSE)

nodes<-as.matrix(nodes1[,1])

states<-nodes1[,2:6]
colnames(states)<-NULL

# Import network connections as matrix

data<- read.table(~"Edge_BalticSeaERA_benthos.csv", header=TRUE,fill=TRUE, sep=',', stringsAsFactors = FALSE) #read edges

arc.set =as.matrix(data)

dagBS<-empty.graph(nodes) # create empty dag with nodes in the data
arcs(dagBS) = arc.set # create DAG

plot(dagBS) #check DAG structure

g <- Rgraphviz::layoutGraph(bnlearn::as.graphNEL(dagBS))
graph::nodeRenderInfo(g) <- list(fontsize=40)
Rgraphviz::renderGraph(g)

graphviz.plot(dagBS)

# # Set levels/possible outcome states for discrete variables

ST <-states[1,1:3] 
SC<-states[2,1:3] 
DE<-states[3,1:3] 
PR<-states[4,1:2] 
CR<-states[5,1:3] 
VE<-states[6,1:3] 
SS<-states[7,1:3] 
CRel<-states[8,1:2] 
SDep<-states[9,1:3] 

MEpi_dir<-states[10,1:5] 
SEpi_dir<-states[11,1:5] 
In_dir<-states[12,1:5] 
MEpi_indir<-states[13,1:5] 
SEpi_indir<-states[14,1:5] 
In_indir<-states[15,1:5] 
MEpi_tot<-states[16,1:5] 
SEpi_tot<-states[17,1:5] 
In_tot<-states[18,1:5] 

### PARAMETRISATION ###

# CPTs of independent variables

CRp<-array(c(0.45,0.5, 0.05), dim=c(3), dimnames=list(Concretion_removal = CR)) #Mining intensity /Concretion removal
DEp<- array(c(0.10,0.45, 0.45), dim = 3,dimnames=list(Depth_extraction = DE)) # Depth of extraction
SCp <- array(c(0.6, 0.3,0.1), dim = 3,dimnames=list(Sediment_contaminants = SC))  #Sediment contaminants
STp <- array(c(0.6, 0.3,0.1), dim = 3,dimnames=list(Sediment_Type = ST))  #Sediment type
PRp <- array(c(0.5,0.5), dim = 2,dimnames=list(Plume_release = PR)) # Plume release

#Dependent nodes

#NB: argument dim corresponds to the max extent of each of the variables
# Volume of extracted sediment
VolExt<-read.csv("CPTs/BalticSeaERA_benthos_Volume-of-extraction_Concretion-removal.Depth-of-extraction_Laura.csv", header=TRUE,fill=TRUE, sep=',', stringsAsFactors = FALSE)

VolExt<-as.matrix(VolExt[,4:6])
colnames(VolExt) <- NULL
VolExt.pr<-array(t(VolExt),dim=c(3,3,3),dimnames = list(Volume_extraction = VE, Depth_extraction = DE, Concretion_removal=CR)) # read in transposed matrix
VolExt.pr<-VolExt.pr/100 # transform to frequency

# Suspended sediment 
SSed<-read.csv("CPTs/BalticSeaERA_benthos_Suspended-sediment-bottom_Sediment-Type.Plume-release.Volume-of-extraction_Laura.csv", header=TRUE,fill=TRUE, sep=',', stringsAsFactors = FALSE)
SSed<-as.matrix(SSed[,5:7])
colnames(SSed) <- NULL
SS.pr<-array(t(SSed),dim=c(3,3,2,3),dimnames = list(Suspended_sediment_bottom = SS, Volume_extraction = VE, Plume_release=PR, Sediment_Type=ST)) 
SS.pr<-SS.pr/100

# Contaminant release
CoRel<-read.csv("CPTs/BalticSeaERA_benthos_Contaminant-release_Sediment-Type.Volume-of-extraction.Sediment-contaminants_Laura.csv", header=TRUE,fill=TRUE, sep=',', stringsAsFactors = FALSE)
CoRel<-as.matrix(CoRel[,5:6])
colnames(CoRel) <- NULL
CRel.pr<-array(t(CoRel),dim=c(2,3,3,3),dimnames = list(Contaminant_release = CRel,Sediment_contaminants=SC, Volume_extraction = VE,  Sediment_Type=ST)) 
CRel.pr<-CRel.pr/100

#Sediment deposition

SeDep<-read.csv("CPTs/BalticSeaERA_benthos_Sediment-deposition_Suspended-sediment-bottom_Laura.csv", header=TRUE,fill=TRUE, sep=',', stringsAsFactors = FALSE)
SeDep<-as.matrix(SeDep[,3:5])
colnames(SeDep) <- NULL
SeDeppr<-array(t(SeDep),dim=c(3,3),dimnames = list(Sediment_deposition=SDep,Suspended_sediment_bottom=SS)) 
SeDeppr<-SeDeppr/100

# Sessile epifauna indirect
SesEpi_indir<-read.csv("CPTs/Sessile-epifauna_indirect2.csv", header=TRUE,fill=TRUE, sep=',', stringsAsFactors = FALSE)

SesEpi_id<-as.matrix(SesEpi_indir[,4:8])
colnames(SesEpi_id) <- NULL
# OBS. add parent nodes in reverse order than in the table
SesEpi_idpr<-array(t(SesEpi_id),dim=c(5,2,3,3),dimnames = list(Sessile_epifauna_indir=SEpi_indir,Contaminant_release = CRel,Sediment_deposition=SDep,Suspended_sediment_bottom=SS)) 
SesEpi_idpr<-SesEpi_idpr/100

# Mobile epifauna indirect

MoEpi_indir<-read.csv("CPTs/Mobile_epifauna_indirect.csv", header=TRUE,fill=TRUE, sep=',', stringsAsFactors = FALSE)

MoEpi_indir<-as.matrix(MoEpi_indir[,4:8])
colnames(MoEpi_indir) <- NULL
MoEpi_idpr<-array(t(MoEpi_indir),dim=c(5,2,3,3),dimnames = list(Mobile_epifauna_indir=MEpi_indir,Contaminant_release = CRel,Sediment_deposition=SDep,Suspended_sediment_bottom=SS)) 
MoEpi_idpr<-MoEpi_idpr/100

# Infauna indirect
Infa_indir<-read.csv("CPTs/Infauna_indirect.csv", header=TRUE,fill=TRUE, sep=',', stringsAsFactors = FALSE)
Infa_indir<-as.matrix(Infa_indir[,4:8])
colnames(Infa_indir) <- NULL
#In_idpr<-array(t(Infa_indir),dim=c(5,2,3,3),dimnames = list(Infauna=In,Contaminant_release = CRel,Sediment_deposition=SDep,Suspended_sediment_bottom=SS)) 
Inpr<-array(t(Infa_indir),dim=c(5,3,2,3),dimnames = list(Infauna_indir=In_indir, Sediment_deposition=SDep, Contaminant_release = CRel,  Suspended_sediment_bottom=SS))
In_idpr<-Inpr/100


# Sessile epifauna direct

SesEpi_dir<-read.csv("CPTs/Sessile_epifauna_direct.csv", header=TRUE,fill=TRUE, sep=',', stringsAsFactors = FALSE)
SesEpi_dir<-as.matrix(SesEpi_dir[,2:6])
colnames(SesEpi_dir) <- NULL
SesEpi_dpr<-matrix(t(SesEpi_dir),ncol=3,dimnames = list(Sessile_epifauna_dir=SEpi_dir, Concretion_removal=CR))
SesEpi_dpr<-SesEpi_dpr/100

# Mobile epifauna direct

MoEpi_dir<-read.csv("CPTs/Mobile_epifauna_direct.csv", header=TRUE,fill=TRUE, sep=',', stringsAsFactors = FALSE)
MoEpi_dir<-as.matrix(MoEpi_dir[,2:6])
colnames(MoEpi_dir) <- NULL
MoEpi_dpr<-matrix(t(MoEpi_dir),ncol=3,dimnames = list(Mobile_epifauna_dir=MEpi_dir, Concretion_removal=CR))
MoEpi_dpr<-MoEpi_dpr/100

# Infauna direct
Infa_dir<-read.csv("CPTs/Infauna_direct.csv", header=TRUE,fill=TRUE, sep=',', stringsAsFactors = FALSE)
Infa_dir<-as.matrix(Infa_dir[,2:6])
colnames(Infa_dir) <- NULL
In_dpr<-matrix(t(Infa_dir),ncol=3,dimnames = list(Infauna_dir=In_dir, Concretion_removal=CR))
In_dpr<-as.matrix(In_dpr/100)

# Sessile epifauna total

SesEpi_tot<-read.csv("CPTs/TotalMortality.csv", header=TRUE,fill=TRUE, sep=',', stringsAsFactors = FALSE)
SesEpi_tot<-as.matrix(SesEpi_tot[,3:7])
colnames(SesEpi_tot) <- NULL
SesEpi_tpr<-array(t(SesEpi_tot),dim=c(5,5,5),dimnames = list(Sessile_epifauna_total=SEpi_tot,Sessile_epifauna_indir=SEpi_indir, Sessile_epifauna_dir=SEpi_dir))

# Mobile epifauna total

MoEpi_tot<-read.csv("CPTs/TotalMortality.csv", header=TRUE,fill=TRUE, sep=',', stringsAsFactors = FALSE)
MoEpi_tot<-as.matrix(MoEpi_tot[,3:7])
colnames(MoEpi_tot) <- NULL
MoEpi_tpr<-array(t(MoEpi_tot),dim=c(5,5,5),dimnames = list(Mobile_epifauna_total=MEpi_tot,Mobile_epifauna_indir=MEpi_indir, Mobile_epifauna_dir=MEpi_dir))

# Infauna total

In_total<-read.csv("CPTs/TotalMortality.csv", header=TRUE,fill=TRUE, sep=',', stringsAsFactors = FALSE)
In_total<-as.matrix(In_total[,3:7])
colnames(In_total) <- NULL
In_tpr<-array(t(In_total),dim=c(5,5,5),dimnames = list(Infauna_total=MEpi_tot, Infauna_indir=In_indir,Infauna_dir=In_dir))


# Combining CPTs

# Combine dag and the local distributions into a CPT

cpt <- list(Sediment_Type = STp,
            Sediment_contaminants =SCp,
            Plume_release = PRp,
            Concretion_removal=CRp,
            Depth_extraction=DEp,
            Volume_extraction = VolExt.pr,
            Suspended_sediment_bottom = SS.pr,
            Contaminant_release = CRel.pr,
            Sediment_deposition=SeDeppr,
            Mobile_epifauna_dir=MoEpi_dpr,
            Sessile_epifauna_dir=SesEpi_dpr,
            Infauna_dir=In_dpr,
            Mobile_epifauna_indir=MoEpi_idpr,
            Sessile_epifauna_indir=SesEpi_idpr,
            Infauna_indir=In_idpr,
            Mobile_epifauna_total=MoEpi_tpr,
            Sessile_epifauna_total=SesEpi_tpr,
            Infauna_total=In_tpr)
            
            
BSbn <- custom.fit(dagBS, cpt) # combine BN structure and joint CPT


### USING THE BN ###
# The network can be queried to give marginal probabilities

# install required packages 
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.10")
BiocManager::install(c("gRbase", "RBGL", "Rgraphviz", "gRain"))

# Exact inference
library(gRain)
BiocManager::install("RBGL")

junction <- compile(as.grain(BSbn))

# Marginal probability for a given node, examples
barplot(querygrain(junction, nodes = "Mobile_epifauna_total")$Mobile_epifauna_total, ylim=c(0,1))
barplot(querygrain(junction, nodes = "Sessile_epifauna_total")$Sessile_epifauna_total, ylim=c(0,1))
barplot(querygrain(junction, nodes = "Infauna_total")$Infauna_total, ylim=c(0,1))

# Probability of a node given evidence
# Mining efficiency
jsed <- setEvidence(junction, nodes = c("Concretion_removal","Depth_extraction"), states =c("50%", "1-10cm"))
barplot(querygrain(jsed, nodes = "Mobile_epifauna_total")$Mobile_epifauna_total, ylim=c(0,1), main="Mobile epifauna at 50% concretion removal")
barplot(querygrain(jsed, nodes = "Mobile_epifauna_total")$Mobile_epifauna_total, ylim=c(0,1), main="Mobile epifauna at 50% concretion removal")

### QUERY RESULTS ####

# SCENARIO A
Sce1 <- setEvidence(junction, nodes = c("Concretion_removal","Plume_release", "Depth_extraction"), states =c("75%", "At surface","11-30cm" ))

#Total mortality
  r11<-as.data.frame.table(querygrain(Sce1, nodes = "Mobile_epifauna_total")$Mobile_epifauna_total)
  r12<-as.data.frame.table(querygrain(Sce1, nodes = "Sessile_epifauna_total")$Sessile_epifauna_total)
  r13<-as.data.frame.table(querygrain(Sce1, nodes = "Infauna_total")$Infauna_total)
  
  #Indirect mortality
  r14<-as.data.frame.table(querygrain(Sce1, nodes = "Mobile_epifauna_indir")$Mobile_epifauna_indir)
  r15<-as.data.frame.table(querygrain(Sce1, nodes = "Sessile_epifauna_indir")$Sessile_epifauna_indir)
  r16<-as.data.frame.table(querygrain(Sce1, nodes = "Infauna_indir")$Infauna_indir)
  

# SCENARIO B
# harmful substance release
Sce2 <- setEvidence(junction, nodes = c("Concretion_removal","Plume_release", "Depth_extraction", "Contaminant_release"), states =c("50%", "At surface", "11-30cm", "Significant"))
### CHECK IF var states OK!!!

#Total mortality
r21<-as.data.frame.table(querygrain(Sce2, nodes = "Mobile_epifauna_total")$Mobile_epifauna_total)
r22<-as.data.frame.table(querygrain(Sce2, nodes = "Sessile_epifauna_total")$Sessile_epifauna_total)
r23<-as.data.frame.table(querygrain(Sce2, nodes = "Infauna_total")$Infauna_total)

#Indirect mortality
r24<-as.data.frame.table(querygrain(Sce2, nodes = "Mobile_epifauna_indir")$Mobile_epifauna_indir)
r25<-as.data.frame.table(querygrain(Sce2, nodes = "Sessile_epifauna_indir")$Sessile_epifauna_indir)
r26<-as.data.frame.table(querygrain(Sce2, nodes = "Infauna_indir")$Infauna_indir)


### GRAPHICS for paper ####


#Scenario A
  p1<-ggplot(data=r11, aes(x=Mobile_epifauna_total, y=Freq))+ geom_bar(stat="identity",fill="#FF630C")+theme_bw()+scale_y_continuous(name="Probability", limits=c(0, 1))+xlab("")
                                                                                                                                                                           
    #+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.background = element_blank())                                                                                                                                                                          
                                                                                                                                                                              
  p2<-ggplot(data=r12, aes(x=Sessile_epifauna_total, y=Freq))+ geom_bar(stat="identity",fill="#FF630C")+theme_bw()+ylim(0,1)+xlab("Total mortality")
  p3<-ggplot(data=r13, aes(x=Infauna_total, y=Freq))+ geom_bar(stat="identity",fill="#FF630C")+theme_bw()+ylim(0,1)+xlab("")
  
  p4<-ggplot(data=r14, aes(x=Mobile_epifauna_indir, y=Freq))+ geom_bar(stat="identity",fill="#355F76")+theme_bw()+scale_y_continuous(name="Probability", limits=c(0, 1))+xlab("")
  p5<-ggplot(data=r15, aes(x=Sessile_epifauna_indir, y=Freq))+ geom_bar(stat="identity",fill="#355F76")+theme_bw()+ylim(0,1)+xlab("Indirect mortality")
  p6<-ggplot(data=r16, aes(x=Infauna_indir, y=Freq))+ geom_bar(stat="identity",fill="#355F76")+theme_bw()+ylim(0,1)+xlab("")
  
  #Scenario B
  
  p21<-ggplot(data=r21, aes(x=Mobile_epifauna_total, y=Freq))+ geom_bar(stat="identity",fill="#FF630C")+theme_bw()+scale_y_continuous(name="Probability", limits=c(0, 1))+xlab("")
  p22<-ggplot(data=r22, aes(x=Sessile_epifauna_total, y=Freq))+ geom_bar(stat="identity",fill="#FF630C")+theme_bw()+ylim(0,1)+xlab("Total mortality")
  p23<-ggplot(data=r23, aes(x=Infauna_total, y=Freq))+ geom_bar(stat="identity",fill="#FF630C")+theme_bw()+ylim(0,1)+xlab("")
  
  p24<-ggplot(data=r24, aes(x=Mobile_epifauna_indir, y=Freq))+ geom_bar(stat="identity",fill="#355F76")+theme_bw()+scale_y_continuous(name="Probability", limits=c(0, 1))+xlab("")
  p25<-ggplot(data=r25, aes(x=Sessile_epifauna_indir, y=Freq))+ geom_bar(stat="identity",fill="#355F76")+theme_bw()+ylim(0,1)+xlab("Indirect mortality")
  p26<-ggplot(data=r26, aes(x=Infauna_indir, y=Freq))+ geom_bar(stat="identity",fill="#355F76")+theme_bw()+ylim(0,1)+xlab("")
  

## save gridplot
  install.packages("egg")
  library(egg)

  all_comb<-ggarrange(p1+theme(text=element_text(size=26)), 
                    p2 + 
                      theme(axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.title.y = element_blank(),text=element_text(size=28) ), 
                    p3 + 
                      theme(axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.title.y = element_blank(),text=element_text(size=28) ),
                    p4+theme(text=element_text(size=24)), 
                    p5 + 
                      theme(axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.title.y = element_blank(),text=element_text(size=28) ), 
                    p6 + 
                      theme(axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.title.y = element_blank(),text=element_text(size=28)),
                    p21+theme(text=element_text(size=24)), 
                    p22 + 
                      theme(axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.title.y = element_blank(),text=element_text(size=28) ), 
                    p23 + 
                      theme(axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.title.y = element_blank(),text=element_text(size=28) ),
                    p24+theme(text=element_text(size=24)), 
                    p25 + 
                      theme(axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.title.y = element_blank(),text=element_text(size=28) ), 
                    p26 + 
                      theme(axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.title.y = element_blank(),text=element_text(size=28)),
                    nrow = 4, widths = c(1, 1, 1))
  

### FULL CONCEPTUAL MODEL (for visualization only)
  
  # Import node names and states
  
  nodes2<-read.table("Node_BalticSeaERA_all.csv", header=F,fill=TRUE, sep=',', stringsAsFactors = FALSE)
  
  nodes22<-as.matrix(nodes2)
  
  
  # Import network as matrix
  
  edge<- read.table(Edge_BalticSeaERA_all.csv", header=TRUE,fill=TRUE, sep=',', stringsAsFactors = FALSE) #read edges
  
  #data<- read.table("clipboard", header=TRUE,fill=TRUE, sep='\t', stringsAsFactors = FALSE) #read edges
  
  arc.set =as.matrix(edge)
  
  dagBS_all<-empty.graph(nodes22) # create empty dag with nodes in the data
  arcs(dagBS_all) = arc.set # create DAG
  plot(dagBS_all) #check DAG structure
  
  g <- Rgraphviz::layoutGraph(bnlearn::as.graphNEL(dagBS_all))
  graph::nodeRenderInfo(g) <- list(fontsize=140)
  Rgraphviz::renderGraph(g)
  
  
