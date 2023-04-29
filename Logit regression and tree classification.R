############################
#### DATASET SOCTELEPAG ####
############################

SocTelePag<-file.choose()
SocTelePag<-read.csv2(SocTelePag,header=TRUE,sep=";") 

attach(SocTelePag) # esplicitazione delle colonne (variabili) del dataset
dim(SocTelePag) # dimensione del dataset
colnames(SocTelePag) # identificazione delle variiabili del dataset (colonne)
summary(SocTelePag) # statistiche descrittive delle variabili presenti nel dataset
summary(SocTelePag$durata) # analisi della variabile durata (espressa in mesi)

# poichè la variabile durata assume valore min=0 e valore max=167, si procede al
# raggruppamento della variabile durata
durata_gruppo<-function(durata){
  if (durata>=0 & durata<=3){return('0-3 Mesi')} 
  # se durata>=0 e durata <=3, restituisci come categoria 0-3 Mesi
  else if (durata>3 & durata<=6){return('3-6 Mesi')} 
  # altrimenti se durata>3 e durata<=6, restituisci come categoria 3-6 Mesi
  else if (durata>6 & durata<=12){return('6-12 Mesi')} 
  # altrimenti se durata>6 e durata<=12, restituisci come categoria 6-12 Mesi
  else if (durata>12 & durata<=18){return('12-18 Mesi')} 
  # altrimenti se durata>12 e durata<=18, restituisci come categoria 12-18 Mesi
  else if (durata>18 & durata<=24){return('18-24 Mesi')} 
  # altrimenti se durata>18 e durata<=24, restituisci come categoria 18-24 Mesi
  else if (durata>24 & durata<=30){return('24-30 Mesi')} 
  # altrimenti se durata>24 e durata<=30, restituisci come categoria 24-30 Mesi
  else if (durata>30 & durata<=36){return('30-36 Mesi')} 
  # altrimenti se durata>30 e durata <=36, restituisci come categoria 30-36 Mesi
  else if (durata>36){return('> 36 Mesi') 
    # altrimenti se durata>36, restituisci come categoria >36 Mesi
  } }

# introduzione di una nuova variabile nel dataset definita in termini dei gruppi
# individuati attraverso il precedente comando
SocTelePag$durata_gruppo<-sapply(SocTelePag$durata,durata_gruppo)

# trasformazione della variabile durata_gruppo in factor
SocTelePag$durata_gruppo<-as.factor(SocTelePag$durata_gruppo)

# eliminazione della colonna del dataset non necessaria per la costruzione del
modello di regressione logistica
SocTelePag$durata<-NULL
dim(SocTelePag) # verifica della dimensione del dataset

# trasformazione della variabileMOP_PO in factor
SocTelePag$MOP_PO<-as.factor(SocTelePag$MOP_PO) 

# trasformazione della variabile ESG_ATTIVAZIONI in factor
SocTelePag$ESG_ATTIVAZIONI<-as.factor(SocTelePag$ESG_ATTIVAZIONI) 


##########################################
#### MODELLO DI REGRESSIONE LOGISTICA ####
##########################################

# implementazione del modello logit (regressione logistica)
Logit<-glm(stato~.,data=SocTelePag,family=binomial) 
summary(Logit) 

# metodo per la determinazione degli odds-ratios con relativi p-values
library(mfx) # la detrminazione degli odds-ratios e degli effetti marginali
logitor(stato~.,data=SocTelePag)
logitmfx(stato~.,data=SocTelePag) # effetti marginali

library(pscl) # per la determinazione dello pseudo-R^2
# (valutazione della bontà del modello)
pseudoR2<-pR2(Logit) # calcolo dell'indice pseudo-R^2 di Mc-Fadden 
pseudoR2 


###############################################################
#### MATRICE DI CONFUSIONE E VALUTAZIONE DEL MODELLO LOGIT ####
###############################################################

# calcolo delle probabilità di abbandono
prob_logit<-predict(Logit,SocTelePag,type="response") 

library(InformationValue) # per la visualizzazione grafica della curva ROC 
# e la determinazione dell'indice AUC (AUROC nel grafico)

print("Matrice di confusione per il modello logit");table(SocTelePag
$stato,prob_logit>0.65) # matrice di confusione con threshold>cutoff=0.65
ROC_logit<-plotROC(SocTelePag$stato,prob_logit) # curva ROC con valore AUROC


###########################################################################
## ALBERO DI CLASSIFICAZIONE COSTRUITO SU TUTTE LE VARIABILI ESPLICATIVE ##
###########################################################################

# identificazione delle modalità con cui si esprimono le variabili esplicative 
# con maggior potere predittivo

levels(SocTelePag$durata_gruppo)

SocTelePag$MOP_PO<-as.factor(SocTelePag$MOP_PO) 
levels(SocTelePag $MOP_PO)

SocTelePag$ESG_ATTIVAZIONI<-as.factor(SocTelePag$ESG_ATTIVAZIONI) 
levels(SocTelePag $ESG_ATTIVAZIONI)

library(rpart) # per l'implementazione degli alberi di classificazione

# implementazione dell'albero di classificazione costruito in funzione 
# di tutte le variabili
tree<-rpart(stato~.,data=SocTelePag,method="class") 
tree

summary(tree) # risultati

library(rattle) # per la visualizzazione grafica dell'albero di classificazione
fancyRpartPlot(tree) # rappresentazione grafica dell'albero

# validazione dell'albero di classificazione mediante l'utilizzo del parametro di
# complessità (CP) e l'errore di validazione
printcp(tree)
# rappresentazione grafica dell'andamento del parametro di complessità 
# rispetto all'xerror
plotcp(tree) 
# identificazione del valore del parametro di complessità a cui è associato 
# il minimo errore di validazione
tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"] 


############################################################################
#### MATRICE DI CONFUSIONE E VALUTAZIONE DELL'ABLERO DI CLASSIFICAZIONE ####
############################################################################

# probabilità stimate per ogni cliente (prob di non abbandono=0, prob di abbandono=1)
prob_tree<-predict(tree,type='prob') 
# probabilità di abbandono stimate e associate ad ogni cliente
prob_tree_1<-prob_tree[,2] 

library(InformationValue) # per la visualizzazione grafica della curva ROC 
# e la determinazione dell'indice AUC (AUROC nel grafico)
print("Matrice di confusione per gli alberi di classificazione");table(SocTelePag
$stato,prob_tree_1>0.65) # matrice di confusione con threshold>cutoff=0.65
ROC_tree<-plotROC(SocTelePag$stato,prob_tree_1) # grafico della curva ROC e 
# relativo indice AUROC