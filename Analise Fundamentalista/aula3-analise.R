shhh <- suppressPackageStartupMessages # não mostra  mensagens

shhh(library(data.table))

options(scipen=99) # no scientic notation
options(digits = 5)

data.bulk=shhh(fread("data/usiminas-ano10-18.csv",sep=";", header=TRUE, dec=",", colClasses=c("character", rep("numeric", 10),"character", "character", "character")))

data.bulk$V14=NULL

data.bulk=as.data.frame(data.bulk)


data.bulk=data.bulk[,1:13]
#achar o numero de nomes repetidos
col1=data.bulk[,1] # nome da conta

col12=data.bulk[,13] # simbolo da demonstração
clounique=unique(col1)
notuniq=sapply(clounique, function(x)(length(which(col1 == x))))
# notuniq


# Alterar os registros com codigo de classe de contas

rownovo = paste(col1,col12, sep=" - ")

names.rep=data.frame(nam=names(notuniq), x = notuniq)
names.rep




a=length(rownovo)
notuniq=sapply(clounique, function(x)(length(which(col1 == x))))

row.names(data.bulk)=rownovo

b=length(row.names(data.bulk))

if (a==b) {print("Linhas resolvidas")
  # renomeia linhas dos dados com nomes unicos
}else
{
  print("Linhas nao resolvidas")
  stop()
}


names(data.bulk)
str(data.bulk)


head(data.bulk)
tail(data.bulk)


###################################################


#class(data.bulk)
#rownames(data.bulk)
#colnames(data.bulk)

# Funcoes

saveplot <- function(myPlot, filename) {
  png(filename,width=1800 ,heigh=1200  ,res=180)
  print(myPlot)
  dev.off()
}


f.dec = function(x, k) trimws(format(round(x, k), nsmall=k))

f.perc= function(x) paste0((x*100), "%")






############# INICIO ANALISE FUNDAMENTALISTA ##############
header.Ativo= as.numeric(which(rownames(data.bulk) == "Ativo - AT"))


header.Passivo=as.numeric(which(rownames(data.bulk) =="Passivo - PT"))


header.DRE=as.numeric(which(rownames(data.bulk) == "DRE - DRE"))


header.DRA=as.numeric(which(rownames(data.bulk) == "DRA - DRA"))


header.DFC=as.numeric(which(rownames(data.bulk) == "DFC - DFC"))


header.DVA=as.numeric(which(rownames(data.bulk) == "DVA - DVA"))


header.MERC=as.numeric(which(rownames(data.bulk) =="Mercado - MERC"))


header.FIM=as.numeric(which(rownames(data.bulk) =="FIM - FIM"))



# Separar os conjuntos de contas Ativo, Passivo, DRE, RA, DFC...

# ATIVO
Data.Ativo=data.bulk[header.Ativo:(header.Passivo-1),]
head(Data.Ativo)
tail(Data.Ativo)

  # Identificar Index divisorias dentro do Ativo: Ativo Circulante, Ativo Nao Circulante, RLP, Perm
  header.AC=as.numeric(which(rownames(Data.Ativo) == "Ativo Circulante - AC"))
  header.ANC=as.numeric(which(rownames(Data.Ativo) == "Ativo Nao Circulante - ANC"))
  header.INV=as.numeric(which(rownames(Data.Ativo) == "Investimentos - ANC"))
  header.PERM=as.numeric(which(rownames(Data.Ativo) == "Imobilizado - ANC"))
  # Separar os subconjuntos de contas Ativo: Ativo Circulante, Ativo Nao Circulante, RLP, Perm
  end.Ativo=nrow(Data.Ativo)
  ncol(Data.Ativo)
  
  # Ativo Circulante
  Data.AC=Data.Ativo[header.AC:(header.ANC-1),]
  head(Data.AC)
  tail(Data.AC)
  
  # Ativo Nao Circulante
  Data.ANC=Data.Ativo[header.ANC:(header.Passivo-1),]
  head(Data.ANC)
  tail(Data.ANC)
  
  # RLP
  Data.RLP=Data.Ativo[header.ANC:(header.INV-1),]
  head(Data.RLP)
  tail(Data.RLP)
  
  # PERM
  Data.PERM=Data.Ativo[header.INV:(end.Ativo),]
  head(Data.PERM)
  tail(Data.PERM)



# PASSIVO
Data.Passivo=data.bulk[header.Passivo:(header.DRE-1),]
head(Data.Passivo)
tail(Data.Passivo)

  # Identificar Index divisorias dentro do Passivo: Passivo Circulante, Passivo Nao Circulante, PL
  header.PC=as.numeric(which(rownames(Data.Passivo) == "Passivo Circulante - PC"))
  header.PNC=as.numeric(which(rownames(Data.Passivo) == "Passivo Nao Circulante - PNC"))
  header.PL=as.numeric(which(rownames(Data.Passivo) == "Patrimonio Liquido Consolidado - PL"))
  # Separar os subconjuntos de contas Passivo: Passivo Circulante, Passivo Nao Circulante, PL
  end.Passivo=nrow(Data.Passivo)
  ncol(Data.Ativo)
  
  # Passivo Circulante
  Data.PC=Data.Passivo[header.PC:(header.PNC-1),]
  head(Data.PC)
  tail(Data.PC)
  
  # Passivo nao circulante
  Data.PNC=Data.Passivo[header.PNC:(header.PL-1),]
  head(Data.PNC)
  tail(Data.PNC)
  
  # Patrimonio Liquido
  Data.PL=Data.Passivo[header.PL:end.Passivo,]
  head(Data.PL)
  tail(Data.PL)

# DRE
Data.DRE=data.bulk[header.DRE:(header.DRA-1),]
head(Data.DRE)
tail(Data.DRE)


# DRA
Data.DRA=data.bulk[header.DRA:(header.DFC-1),]
head(Data.DRA)
tail(Data.DRA)


# DFC
Data.DFC=data.bulk[header.DFC:(header.DVA-1),]
head(Data.DFC)
tail(Data.DFC)


# DVA
Data.DVA=data.bulk[header.DVA:(header.MERC-1),]
head(Data.DVA)
tail(Data.DVA)


# MERC
Data.MERC=data.bulk[header.MERC:(header.FIM-1),]
head(Data.MERC)
tail(Data.MERC)




## CONSOLIDANDO DADOS

# head(Data.Ativo)

m.Ativo= subset(Data.Ativo, select= -c(Conta, chk, Classe))
m.Passivo= subset(Data.Passivo, select= -c(Conta, chk, Classe ))
m.DRE= subset(Data.DRE, select= -c(Conta, chk, Classe ))
m.DRA= subset(Data.DRA, select= -c(Conta, chk, Classe ))
m.DFC= subset(Data.DFC, select= -c(Conta, chk , Classe ))
m.DVA= subset(Data.DVA, select= -c(Conta, chk, Classe ))
m.MERC= subset(Data.MERC, select= -c(Conta, chk, Classe ))
m.AC= subset(Data.AC, select= -c(Conta, chk, Classe ))
m.ANC= subset(Data.ANC, select= -c(Conta, chk, Classe ))
m.RLP= subset(Data.RLP, select= -c(Conta, chk, Classe ))
m.PERM= subset(Data.PERM, select= -c(Conta, chk, Classe ))
m.PC= subset(Data.PC, select= -c(Conta, chk, Classe ))
m.PNC= subset(Data.PNC, select= -c(Conta, chk, Classe ))
m.PL= subset(Data.PL, select= -c(Conta, chk, Classe ))


head(m.PL)
tail(m.PL)


###########################
#  APLICANDO CORRECAO MONETARIA
##########################

anos_uso=c("2018","2017","2016","2015","2014","2013","2012","2011","2010","2009")

#Ativo
v.CM.aj=m.MERC["Fator de Correcao - MERC",] # linha de correção monetária
v.IGPDI.aj=m.MERC["IGP-DI - MERC",] # linha de IGP-DI

m.Ativo.aj.cm= m.Ativo
rownomes=rownames(m.Ativo)

m.Ativo.aj.cm=as.data.frame(sweep(m.Ativo.aj.cm, 2, unlist(v.CM.aj), `*`))
colnames(m.Ativo.aj.cm)=anos_uso

# Passivo
m.Passivo.aj.cm= m.Passivo
rownomes=rownames(m.Ativo)

m.Passivo.aj.cm=as.data.frame(sweep(m.Passivo.aj.cm, 2, unlist(v.CM.aj), `*`))
colnames(m.Passivo.aj.cm)=anos_uso

# DRE

m.DRE.aj.cm= m.DRE
rownomes=rownames(m.DRE)

m.DRE.aj.cm=as.data.frame(sweep(m.DRE.aj.cm, 2, unlist(v.CM.aj), `*`))
colnames(m.DRE.aj.cm)=anos_uso

# DRA

m.DRA.aj.cm= m.DRA
rownomes=rownames(m.DRA)

m.DRA.aj.cm=as.data.frame(sweep(m.DRA.aj.cm, 2, unlist(v.CM.aj), `*`))
colnames(m.DRA.aj.cm)=anos_uso


# DFC

m.DFC.aj.cm= m.DFC
rownomes=rownames(m.DFC)

m.DFC.aj.cm=as.data.frame(sweep(m.DFC.aj.cm, 2, unlist(v.CM.aj), `*`))
colnames(m.DFC.aj.cm)=anos_uso

# DVA

m.DVA.aj.cm= m.DVA
rownomes=rownames(m.DVA)

m.DVA.aj.cm=as.data.frame(sweep(m.DVA.aj.cm, 2, unlist(v.CM.aj), `*`))
colnames(m.DVA.aj.cm)=anos_uso

# PC

m.PC.aj.cm= m.PC
rownomes=rownames(m.PC)

m.PC.aj.cm=as.data.frame(sweep(m.PC.aj.cm, 2, unlist(v.CM.aj), `*`))
colnames(m.PC.aj.cm)=anos_uso

# PNC

m.PNC.aj.cm= m.PNC
rownomes=rownames(m.PNC)

m.PNC.aj.cm=as.data.frame(sweep(m.PNC.aj.cm, 2, unlist(v.CM.aj), `*`))
colnames(m.PNC.aj.cm)=anos_uso

# PL

m.PL.aj.cm= m.PL
rownomes=rownames(m.PC)

m.PL.aj.cm=as.data.frame(sweep(m.PL.aj.cm, 2, unlist(v.CM.aj), `*`))
colnames(m.PL.aj.cm)=anos_uso


# AC

m.AC.aj.cm= m.AC
rownomes=rownames(m.AC)

m.AC.aj.cm=as.data.frame(sweep(m.AC.aj.cm, 2, unlist(v.CM.aj), `*`))
colnames(m.AC.aj.cm)=anos_uso

# ANC

m.ANC.aj.cm= m.ANC
rownomes=rownames(m.ANC)

m.ANC.aj.cm=as.data.frame(sweep(m.ANC.aj.cm, 2, unlist(v.CM.aj), `*`))
colnames(m.ANC.aj.cm)=anos_uso

# RLP

m.RLP.aj.cm= m.RLP
rownomes=rownames(m.RLP)

m.RLP.aj.cm=as.data.frame(sweep(m.RLP.aj.cm, 2, unlist(v.CM.aj), `*`))
colnames(m.RLP.aj.cm)=anos_uso

# PERM

m.PERM.aj.cm= m.PERM
rownomes=rownames(m.PERM)

m.PERM.aj.cm=as.data.frame(sweep(m.PERM.aj.cm, 2, unlist(v.CM.aj), `*`))
colnames(m.PERM.aj.cm)=anos_uso



##########################################
#     Ajuste de Contas sem NE
#########################################

# Ajuste AC e ANC: retirada das contas sem NE e com possibilidades de gerenciamento

aj1.ac=as.numeric(m.Ativo.aj.cm["Adiantamentos a Fornecedores - AC",])
aj2.ac=as.numeric(m.Ativo.aj.cm["Outras Contas a Receber - AC",])
is.data.frame(aj2.ac)

aj.ac=aj1.ac + aj2.ac

aj1.anc=as.numeric(m.Ativo.aj.cm["Estoques - ANC",])
aj2.anc=as.numeric(m.Ativo.aj.cm["Outras Contas a Receber - ANC",])
aj3.anc=as.numeric(m.Ativo.aj.cm["Outros - ANC",])

aj.anc=aj1.anc+aj2.anc+aj3.anc



# # Ajuste PC e PNC: retirada das contas sem NE e com possibilidades de gerenciamento
aj1.pc=as.numeric(m.Passivo.aj.cm["Adiantamentos de Clientes - PC",])
aj2.pc=as.numeric(m.Passivo.aj.cm["Contas a Pagar - PC",])
aj.pc=aj1.pc+aj2.pc
aj1.pnc=as.numeric(m.Passivo.aj.cm["Outros - PNC",])
aj.pnc=aj1.pnc



# Ajuste: retirada das contas sem NE e com possibilidades de gerenciamento

m.Ativo.aj=as.data.frame(m.Ativo.aj.cm)
m.Ativo.aj["Ativo Total - AT",]=as.numeric(m.Ativo.aj.cm["Ativo Total - AT",])-aj.ac-aj.anc
m.Ativo.aj["Ativo Circulante - AC",]=as.numeric(m.Ativo.aj.cm["Ativo Circulante - AC",])-aj.ac
m.Ativo.aj["Ativo Nao Circulante - ANC",]=as.numeric(m.Ativo.aj.cm["Ativo Nao Circulante - ANC",])-aj.anc
rownomes1=rownames(m.Ativo)
row.names(m.Ativo.aj)=rownomes1


m.Passivo.aj=as.data.frame(m.Passivo.aj.cm)
m.Passivo.aj["Passivo Total - PT",]=as.numeric(m.Passivo.aj.cm["Passivo Total - PT",])-aj.pc-aj.pnc
m.Passivo.aj["Passivo Circulante - PC",]=as.numeric(m.Passivo.aj.cm["Passivo Circulante - PC",])-aj.pc
m.Passivo.aj["Passivo Nao Circulante - PNC",]=as.numeric(m.Passivo.aj.cm["Passivo Nao Circulante - PNC",])-aj.pnc
rownomes=rownames(m.Passivo)
row.names(m.Passivo.aj)=rownomes

m.AC.aj=as.data.frame(m.AC.aj.cm)
m.AC.aj["Ativo Circulante - AC",]=as.numeric(m.AC.aj.cm["Ativo Circulante - AC",])-aj.ac
rownomes=rownames(m.AC)
row.names(m.AC.aj)=rownomes


m.ANC.aj=as.data.frame(m.ANC.aj.cm)
m.ANC.aj["Ativo Nao Circulante - ANC",]=as.numeric(m.ANC.aj.cm["Ativo Nao Circulante - ANC",])-aj.anc
rownomes=rownames(m.ANC)
row.names(m.ANC.aj)=rownomes

m.PC.aj=as.data.frame(m.PC.aj.cm)
m.PC.aj["Passivo Circulante - PC",]=as.numeric(m.PC.aj.cm["Passivo Circulante - PC",])-aj.pc
rownomes=rownames(m.PC)
row.names(m.PC.aj)=rownomes


m.PNC.aj=as.data.frame(m.PNC.aj.cm)
m.PNC.aj["Passivo Nao Circulante - PNC",]=as.numeric(m.PNC.aj.cm["Passivo Nao Circulante - PNC",])-aj.pnc
rownomes=rownames(m.PNC)
row.names(m.PNC.aj)=rownomes


m.PL.aj=as.data.frame(m.PL.aj.cm)
m.PERM.aj=as.data.frame(m.PERM.aj.cm)
m.RLP.aj=as.data.frame(m.RLP.aj.cm)

#########################################
# SMOOTHIE DE DADOS PARA SUAVIZAR MEDIAS
#########################################

# Valores medios dos anos no BP

anos.med= c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
anos_uso = c("2018","2017","2016","2015","2014","2013","2012","2011","2010","2009")
rownomes=rownames(m.Ativo)
m.Ativo.aj = as.data.frame(sapply(m.Ativo.aj, as.numeric))
m.Ativo.aj.med=m.Ativo.aj
m.Ativo.aj.med[ ,anos.med]


m.Ativo.aj.med=lapply(1:(ncol(m.Ativo.aj.med)-1), function(i) (m.Ativo.aj.med[,i] + m.Ativo.aj.med[,i+1])/2)

m.Ativo.aj.med= matrix(unlist(m.Ativo.aj.med), byrow = FALSE, nrow = nrow(m.Ativo.aj))
m.Ativo.aj.med=as.data.frame(m.Ativo.aj.med)
row.names(m.Ativo.aj.med)=rownomes
row.names(m.Ativo.aj)=rownomes

colnames(m.Ativo.aj.med)=anos.med


rownomes=rownames(m.Passivo)
m.Passivo.aj = as.data.frame(sapply(m.Passivo.aj, as.numeric))
m.Passivo.aj.med=m.Passivo.aj

m.Passivo.aj.med[ ,anos.med]


m.Passivo.aj.med=lapply(1:(ncol(m.Passivo.aj)-1), function(i) (m.Passivo.aj.med[,i] + m.Passivo.aj.med[,i+1])/2)
m.Passivo.aj.med= matrix(unlist(m.Passivo.aj.med), byrow = FALSE, nrow = nrow(m.Passivo.aj))
m.Passivo.aj.med=as.data.frame(m.Passivo.aj.med)
row.names(m.Passivo.aj.med)=rownomes
row.names(m.Passivo.aj)=rownomes

colnames(m.Passivo.aj.med)=anos.med

rownomes=rownames(m.PL)
m.PL.aj = as.data.frame(sapply(m.PL.aj, as.numeric))
m.PL.aj.med=m.PL.aj

m.PL.aj.med[ ,anos.med]

m.PL.aj.med=lapply(1:(ncol(m.PL.aj.med)-1), function(i) (m.PL.aj.med[,i] + m.PL.aj.med[,i+1])/2)
m.PL.aj.med= matrix(unlist(m.PL.aj.med), byrow = FALSE, nrow = nrow(m.PL.aj))
m.PL.aj.med=as.data.frame(m.PL.aj.med)
row.names(m.PL.aj.med)=rownomes
row.names(m.PL.aj)=rownomes

colnames(m.PL.aj.med)=anos.med

rownomes=rownames(m.AC)
m.AC.aj = as.data.frame(sapply(m.AC.aj, as.numeric))
m.AC.aj.med=m.AC.aj

m.AC.aj.med[ ,anos.med]


m.AC.aj.med=lapply(1:(ncol(m.AC.aj.med)-1), function(i) (m.AC.aj.med[,i] + m.AC.aj.med[,i+1])/2)
m.AC.aj.med= matrix(unlist(m.AC.aj.med), byrow = FALSE, nrow = nrow(m.AC.aj))
m.AC.aj.med=as.data.frame(m.AC.aj.med)
row.names(m.AC.aj.med)=rownomes
row.names(m.AC.aj)=rownomes

colnames(m.AC.aj.med)=anos.med

rownomes=rownames(m.ANC)
m.ANC.aj = as.data.frame(sapply(m.ANC.aj, as.numeric))
m.ANC.aj.med=m.ANC.aj

m.ANC.aj.med[,anos.med]


m.ANC.aj.med=lapply(1:(ncol(m.ANC.aj.med)-1), function(i) (m.ANC.aj.med[,i] + m.ANC.aj.med[,i+1])/2)
m.ANC.aj.med= matrix(unlist(m.ANC.aj.med), byrow = FALSE, nrow = nrow(m.ANC.aj))
m.ANC.aj.med=as.data.frame(m.ANC.aj.med)
row.names(m.ANC.aj.med)=rownomes
row.names(m.ANC.aj)=rownomes

colnames(m.ANC.aj.med)=anos.med


rownomes=rownames(m.PC)
m.PC.aj = as.data.frame(sapply(m.PC.aj, as.numeric))
m.PC.aj.med=m.PC.aj

m.PC.aj.med[ ,anos.med]





m.PC.aj.med=lapply(1:(ncol(m.PC.aj.med)-1), function(i) (m.PC.aj.med[,i] + m.PC.aj.med[,i+1])/2)
m.PC.aj.med= matrix(unlist(m.PC.aj.med), byrow = FALSE, nrow = nrow(m.PC.aj))
m.PC.aj.med=as.data.frame(m.PC.aj.med)
row.names(m.PC.aj.med)=rownomes
row.names(m.PC.aj)=rownomes

colnames(m.PC.aj.med)=anos.med


rownomes=rownames(m.PNC)
m.PNC.aj = as.data.frame(sapply(m.PNC.aj, as.numeric))
m.PNC.aj.med=m.PNC.aj

m.PNC.aj.med[ ,anos.med]




m.PNC.aj.med=lapply(1:(ncol(m.PNC.aj.med)-1), function(i) (m.PNC.aj.med[,i] + m.PNC.aj.med[,i+1])/2)
m.PNC.aj.med= matrix(unlist(m.PNC.aj.med), byrow = FALSE, nrow = nrow(m.PNC.aj))
m.PNC.aj.med=as.data.frame(m.PNC.aj.med)
row.names(m.PNC.aj.med)=rownomes
row.names(m.PNC.aj)=rownomes

colnames(m.PNC.aj.med)=anos.med


rownomes=rownames(m.PERM)
m.PERM.aj = as.data.frame(sapply(m.PERM.aj, as.numeric))
m.PERM.aj.med=m.PERM.aj

m.PERM.aj.med=m.PERM.aj.med[ ,anos_uso]
m.PERM.aj.med=lapply(1:(ncol(m.PERM.aj.med)-1), function(i) (m.PERM.aj.med[,i] + m.PERM.aj.med[,i+1])/2)
m.PERM.aj.med=matrix(unlist(m.PERM.aj.med), byrow = FALSE, nrow = nrow(m.PERM.aj))
m.PERM.aj.med=as.data.frame(m.PERM.aj.med)
row.names(m.PERM.aj.med)=rownomes
row.names(m.PERM.aj)= rownomes

colnames(m.PERM.aj.med)= anos.med


rownomes=rownames(m.RLP)
m.RLP.aj = as.data.frame(sapply(m.RLP.aj, as.numeric))
m.RLP.aj.med=m.RLP.aj

m.RLP.aj.med[ ,anos.med]



m.RLP.aj.med=lapply(1:(ncol(m.RLP.aj.med)-1), function(i) (m.RLP.aj.med[,i] + m.RLP.aj.med[,i+1])/2)
m.RLP.aj.med=matrix(unlist(m.RLP.aj.med), byrow = FALSE, nrow = nrow(m.RLP.aj))
m.RLP.aj.med=as.data.frame(m.RLP.aj.med)
row.names(m.RLP.aj.med)=rownomes
row.names(m.RLP.aj)=rownomes

colnames(m.RLP.aj.med)=anos.med



###########################################
#     EXIBINDO DADOS COM AJUSTE
###########################################
#install.packages("ggrepel")
library(ggrepel)

# Visualizar dados Ativo original, corrigido e medio

g1.str="Ativo Total - AT"
g2.str= "Ativo Total - AT"
g3.str= "Ativo Total - AT"

Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
g1.dt = as.data.frame( t(m.Ativo[g1.str,1:9]) )
g2.dt = as.data.frame( t(m.Ativo.aj.cm[g2.str,1:9]) )
g3.dt = as.data.frame( t(m.Ativo.aj.med[g3.str,1:9]) )

g1.class=unlist(strsplit(g1.str," - ",fixed=TRUE))
g2.class=unlist(strsplit(g2.str," - ",fixed=TRUE))
g3.class=unlist(strsplit(g3.str," - ",fixed=TRUE))

g1.class[1]=paste(g1.class[1]," NOM", sep="")
g2.class[1]=paste(g2.class[1]," COR", sep="")
g3.class[1]=paste(g3.class[1]," MED", sep="")


g1.conta=c(rep(g1.class[1],9))
g2.conta=c(rep(g2.class[1],9))
g3.conta=c(rep(g3.class[1],9))

g1.dt["Contas"]=g1.conta
g2.dt["Contas"]=g2.conta
g3.dt["Contas"]=g3.conta

names(g1.dt)[1]="Valor"
names(g2.dt)[1]="Valor"
names(g3.dt)[1]="Valor"

Anos = paste(Anos,"0101", sep="")
g1.dt["Anos"]=Anos

g2.dt["Anos"]=Anos

g3.dt["Anos"]=Anos



g.dt=(rbind(g1.dt,g2.dt,g3.dt))

rownames(g.dt)=c(1:length(row.names(g.dt)))

g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")

g1.dt$Anos = as.Date(g1.dt$Anos,"%Y%m%d")
g2.dt$Anos = as.Date(g2.dt$Anos,"%Y%m%d")
g3.dt$Anos = as.Date(g3.dt$Anos,"%Y%m%d")



g.dt["Anos"]=Anos
g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")
g.dt$Contas = as.factor(g.dt$Contas)

g0 = ggplot(data=g.dt, aes(x=Anos ,y = Valor, group = Contas, colour= Contas ))+
  geom_line(col="gray70",size= 0.8, linetype = 3 )+
  labs( x="Ano", y="Ativo Total (Bilhões R$)") +
  ggtitle("Comparativo de Ativos Ajustados")+
  scale_y_continuous(labels = function(z) paste0(z/1000000, " Bi R$"), breaks=seq(20000000,50000000,by= 5000000), limits=c(20000000,50000000))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_label_repel(
    aes(x=Anos ,y = Valor, label = paste0(f.dec(Valor/1000000,3), " Bi")),
    box.padding = 0.25, point.padding = 0.25,
    size = 3.5, nudge_y = 0.20,
    segment.color = "grey70") + geom_point(aes(shape = Contas),size =4.5) +
  scale_shape_manual(values=c(15, 16, 17)) + scale_size_manual(values=c(4, 4, 6)) + theme_bw()+
  theme(panel.border = element_rect(fill=NA,color="black", size=.5, linetype="solid"),
        plot.title = element_text(color="black", size=18, face="bold.italic"),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        axis.text.x = element_text(face="plain", color="black", size=14, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=14, angle=0))
# guides(group=g.dt, col = guide_legend(nrow=1))
g0= g0 +
  # scale_colour_grey(start = 0.3, end = 0.4) +
  theme(legend.position = c(0.80, 0.75),
        legend.background = element_rect(fill="white", size=1, linetype="solid"))
print(g0)
#saveplot(g0,"g0.png") # se quiser salvar




###########################################
#       COMPARANDO DRE ENTRE OS ANOS
###########################################

f.dec = function(x, k) trimws(format(round(x, k), nsmall=k))
f.perc= function(x) paste0((x*100), "%")

g1.str="Receita de Venda de Bens e/ou Servicos - DRE"
g2.str= "Resultado Antes do Resultado Financeiro e dos Tributos - DRE"
g3.str= "Lucro/Prejuizo Consolidado do Periodo - DRE"

Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
g1.dt = as.data.frame( t(m.DRE.aj.cm[g1.str,1:9]) )
g2.dt = as.data.frame( t(m.DRE.aj.cm[g2.str,1:9]) )
g3.dt = as.data.frame( t(m.DRE.aj.cm[g3.str,1:9]) )

g1.class=unlist(strsplit(g1.str," - ",fixed=TRUE))
g2.class=unlist(strsplit(g2.str," - ",fixed=TRUE))
g3.class=unlist(strsplit(g3.str," - ",fixed=TRUE))


g1.conta=c(rep(g1.class[1],9))
g2.conta=c(rep(g2.class[1],9))
g3.conta=c(rep(g3.class[1],9))

g1.dt["Contas"]=g1.conta
g2.dt["Contas"]=g2.conta
g3.dt["Contas"]=g3.conta

names(g1.dt)[1]="Valor"
names(g2.dt)[1]="Valor"
names(g3.dt)[1]="Valor"

Anos = paste(Anos,"0101", sep="")
g1.dt["Anos"]=Anos

g2.dt["Anos"]=Anos

g3.dt["Anos"]=Anos



g.dt=(rbind(g1.dt,g2.dt,g3.dt))

rownames(g.dt)=c(1:length(row.names(g.dt)))

g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")

g1.dt$Anos = as.Date(g1.dt$Anos,"%Y%m%d")
g2.dt$Anos = as.Date(g2.dt$Anos,"%Y%m%d")
g3.dt$Anos = as.Date(g3.dt$Anos,"%Y%m%d")



g.dt["Anos"]=Anos
g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")
g.dt$Contas = as.factor(g.dt$Contas)

g01 = ggplot(data=g.dt, aes(x=Anos ,y = Valor, group = Contas, colour= Contas ))+
  geom_line(col="gray70",size= 0.8, linetype = 3 )+
  labs( x="Ano", y="DRE (Bilhões R$)") +
  ggtitle("Comparativo de Contas - DRE")+
  scale_y_continuous(labels = function(z) paste0(z/1000000, " Bi R$"),
                     breaks=seq(-5000000,20000000,by= 5000000), limits=c(-5000000,20000000))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_label_repel(
    aes(x=Anos ,y = Valor, label = paste0(f.dec(Valor/1000000,3), " Bi")),
    box.padding = 0.3, point.padding = 0.3,
    size = 3, nudge_y = 0.25,
    segment.color = "grey70") + geom_point(aes(shape = Contas),size = 4 ) +
  scale_shape_manual(values=c(15, 16, 17)) + scale_size_manual(values=c(4, 4, 6)) + theme_bw()+
  theme(panel.border = element_rect(fill=NA,color="black", size=.5, linetype="solid"),
        plot.title = element_text(color="black", size=18, face="bold.italic"),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        axis.text.x = element_text(face="plain", color="black", size=14, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=14, angle=0))
# guides(group=g.dt, col = guide_legend(nrow=1))
g01=g01 +
  # scale_colour_grey(start = 0.3, end = 0.4) +
  theme(legend.position = c(0.28, 0.55), legend.title = element_text(size = 6.5),
        legend.text = element_text(size = 6.5), legend.key.size = unit(0.5, "cm"),
        legend.background = element_rect(fill="white", size=0.8, linetype="solid"))

print(g01)
#saveplot(g01,"/g01.png")




####################################
#   Correção Monetária e IGP-DI
####################################
rm(g.dt)
rm(g1.dt)
rm(g2.dt)

f.dec = function(x, k) trimws(format(round(x, k), nsmall=k))
f.perc= function(x) paste0((x*100), "%")

Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
g1.str="Fator Correcao - MERC"
g2.str= "IGP-DI - MERC"

g1.dt = as.data.frame(t(v.CM.aj[1:9]) )
g2.dt = as.data.frame(t(v.IGPDI.aj[1:9]) )


g1.class=unlist(strsplit(g1.str," - ",fixed=TRUE))
g2.class=unlist(strsplit(g2.str," - ",fixed=TRUE))


g1.conta=c(rep(g1.class[1],9))
g2.conta=c(rep(g2.class[1],9))


g1.dt["Indice"]=g1.conta
g2.dt["Indice"]=g2.conta


names(g1.dt)[1]="Valor"
names(g2.dt)[1]="Valor"

Anos = paste(Anos,"0101", sep="")

g1.dt["Anos"] = as.Date(Anos,"%Y%m%d")
g2.dt["Anos"] = as.Date(Anos,"%Y%m%d")

g.dt=(rbind(g1.dt,g2.dt))

rownames(g.dt)=c(1:length(row.names(g.dt)))

g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")


g.dt["Anos"]=Anos
g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")
g.dt$Indice = as.factor(g.dt$Indice)

g02 = ggplot(data=g.dt, aes(x=Anos ,y = Valor, group = Indice, colour= Indice ))+
  geom_line(col="gray70",size= 0.8, linetype = 3 )+
  scale_color_manual(breaks = c("Fator Correcao", "IGP-DI"),
                     values=c("tomato", "cornflowerblue"))+
  labs( x="Ano", y="IGP-DI e CM (%)") +
  ggtitle("IGP-DI e CM")+
  scale_y_continuous(labels = function(z) paste0(z*100, "%"), breaks=seq(0,1.6,by= 0.2), limits=c(-0.1,1.6))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_label_repel(
    aes(x=Anos ,y = Valor, label = paste0(f.dec(Valor*100,2), "%")),
    box.padding = 0.15, point.padding = 0.15,
    size = 3.5, nudge_y = 0.10,
    segment.color = "grey70") + geom_point(aes(shape = Indice),size =4.5) +
  scale_shape_manual(values=c(15, 16)) + scale_size_manual(values=c(4.5, 4.5)) + theme_bw()+
  theme(panel.border = element_rect(fill=NA,color="black", size=.5, linetype="solid"),
        plot.title = element_text(color="black", size=18, face="bold.italic"),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        axis.text.x = element_text(face="plain", color="black", size=14, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=14, angle=0))
# guides(group=g.dt, col = guide_legend(nrow=1))
g02 =g02 +
  # scale_colour_grey(start = 0.3, end = 0.4) +
  theme(legend.position = c(0.15, 0.50),
        legend.background = element_rect(fill="white", size=1, linetype="solid"))

print(g02)



















####################################################
# Análise de Tendências - Análise Vertical
####################################################

# Fazer analise vertical
Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")

# Ativo
rownomes=rownames(m.Ativo)
m.Ativo.aj.av= m.Ativo.aj.med
v.AT.aj=m.Ativo.aj.med["Ativo Total - AT",] # linha do Ativo Total ajustado
rownomes=rownames(m.Ativo)
row.names(m.Ativo.aj.med)=rownomes
m.Ativo.aj.av= m.Ativo.aj.med
m.Ativo.aj.av=as.data.frame(sweep(m.Ativo.aj.av, 2, unlist(v.AT.aj), `/`))
colnames(m.Ativo.aj.av)=Anos
row.names(m.Ativo.aj.av)=rownomes
colnames(v.AT.aj)=Anos

# Passivo
rownomes=rownames(m.Passivo)
m.Passivo.aj.av= m.Passivo.aj.med
v.PT.aj=m.Passivo.aj.med["Passivo Total - PT",] # linha do Passivo Total ajustado
m.Passivo.aj.av=as.data.frame(sweep(m.Passivo.aj.av, 2, unlist(v.PT.aj), `/`))
colnames(m.Passivo.aj.av)=Anos
row.names(m.Passivo.aj.av)=rownomes
colnames(v.PT.aj)=Anos

# DRE
m.DRE.aj.av= as.data.frame(m.DRE.aj.cm[,1:9])
colnames(m.Ativo.aj.av)=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
rownomes=rownames(m.DRE.aj.cm)
m.DRE.aj.av= m.DRE.aj.cm
v.DRE.aj=m.DRE.aj.cm["Receita de Venda de Bens e/ou Servicos - DRE",] # linha da Receita Total ajustado
m.DRE.aj.av=as.data.frame(sweep(m.DRE.aj.av, 2, unlist(v.DRE.aj), `/`))
colnames(m.DRE.aj.av)=Anos
row.names(m.DRE.aj.av)=rownomes
colnames(v.DRE.aj)=Anos

###################################################
# Visualizar Análise Vertical: Passivos e Ativos
###################################################
Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")

f.dec = function(x, k) trimws(format(round(x, k), nsmall=k))
f.perc= function(x) paste0((x*100), "%")

g1.str="Passivo Circulante - PC"
g2.str= "Passivo Nao Circulante - PNC"
g3.str= "Patrimonio Liquido Consolidado - PL"


g1.dt =as.data.frame(t(subset(m.Passivo.aj.av[g1.str,],select=Anos)))
g2.dt =as.data.frame(t(subset(m.Passivo.aj.av[g2.str,],select=Anos)))
g3.dt =as.data.frame(t(subset(m.Passivo.aj.av[g3.str,],select=Anos)))

g1.class=unlist(strsplit(g1.str," - ",fixed=TRUE))
g2.class=unlist(strsplit(g2.str," - ",fixed=TRUE))
g3.class=unlist(strsplit(g3.str," - ",fixed=TRUE))

g1.conta=c(rep(g1.class[1],9))
g2.conta=c(rep(g2.class[1],9))
g3.conta=c(rep(g3.class[1],9))

g1.dt["Contas"]=g1.conta
g2.dt["Contas"]=g2.conta
g3.dt["Contas"]=g3.conta

names(g1.dt)[1]="Valor"
names(g2.dt)[1]="Valor"
names(g3.dt)[1]="Valor"

Anos = paste(Anos,"0101", sep="")
g1.dt["Anos"]=Anos

g2.dt["Anos"]=Anos

g3.dt["Anos"]=Anos



g.dt=(rbind(g1.dt,g2.dt,g3.dt))

rownames(g.dt)=c(1:length(row.names(g.dt)))

g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")

g1.dt$Anos = as.Date(g1.dt$Anos,"%Y%m%d")
g2.dt$Anos = as.Date(g2.dt$Anos,"%Y%m%d")
g3.dt$Anos = as.Date(g3.dt$Anos,"%Y%m%d")



g.dt["Anos"]=Anos
g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")
g.dt$Contas = as.factor(g.dt$Contas)

g03 = ggplot(data=g.dt, aes(x=Anos ,y = Valor, group = Contas, colour= Contas ))+
  geom_line(col="gray70",size= 0.8, linetype = 3 )+
  labs( x="Ano", y="Passivos & PL %") +
  ggtitle("An. Vertical (% do Passivo Total)")+
  scale_y_continuous(labels = function(z) paste0(z*100, "%"), breaks=seq(0,1,by=0.20), limits=c(0,1))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_label_repel(
    aes(x=Anos ,y = Valor, label = paste0(f.dec(Valor*100,2), "%")),
    box.padding = 0.0, point.padding = 0.0,
    size = 3 , nudge_y = 0.05,
    segment.color = "grey70") + geom_point(aes(shape = Contas),size = 3) +
  scale_shape_manual(values=c(15, 16, 17)) + scale_size_manual(values=c(4, 4, 6)) + theme_bw()+
  theme(panel.border = element_rect(fill=NA,color="black", size=.5, linetype="solid"),
        plot.title = element_text(color="black", size=18, face="bold.italic"),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        axis.text.x = element_text(face="plain", color="black", size=14, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=14, angle=0))
# guides(group=g.dt, col = guide_legend(nrow=1))
g03 = g03 +
  #   scale_colour_grey(start = 0.3, end = 0.4) +
  theme(legend.position = c(0.20, 0.85), legend.title = element_text(size = 5.5),
        legend.text = element_text(size = 5.5), legend.key.size = unit(0.5, "cm"),
        legend.background = element_rect(fill="white", size=1, linetype="solid"))

print(g03)
#saveplot(g03,"/g03.png")




########################################################
# VISUALIZAR ANALISE DO ATIVO
#######################################################
#Visualizar dados do Ativo: AV

rm(g1.dt)
rm(g2.dt)
rm(g3.dt)
rm(g4.dt)
rm(g5.dt)



g1.str="Caixa e Equivalentes de Caixa - AC"
g2.str= "Ativo Circulante - AC"
g3.str= "Investimentos - ANC"
g4.str="Imobilizado - ANC"
g5.str= "Ativo Nao Circulante - ANC"


Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
g1.dt =as.data.frame(t(subset(m.Ativo.aj.av[g1.str,],select=Anos)))
g2.dt =as.data.frame(t(subset(m.Ativo.aj.av[g2.str,],select=Anos)))
g3.dt =as.data.frame(t(subset(m.Ativo.aj.av[g3.str,],select=Anos)))
g4.dt =as.data.frame(t(subset(m.Ativo.aj.av[g4.str,],select=Anos)))
g5.dt =as.data.frame(t(subset(m.Ativo.aj.av[g5.str,],select=Anos)))

g1.class=unlist(strsplit(g1.str," - ",fixed=TRUE))
g2.class=unlist(strsplit(g2.str," - ",fixed=TRUE))
g3.class=unlist(strsplit(g3.str," - ",fixed=TRUE))
g4.class=unlist(strsplit(g4.str," - ",fixed=TRUE))
g5.class=unlist(strsplit(g5.str," - ",fixed=TRUE))

g1.conta=c(rep(g1.class[1],9))
g2.conta=c(rep(g2.class[1],9))
g3.conta=c(rep(g3.class[1],9))
g4.conta=c(rep(g4.class[1],9))
g5.conta=c(rep(g5.class[1],9))


g1.dt["Contas"]=g1.conta
g2.dt["Contas"]=g2.conta
g3.dt["Contas"]=g3.conta
g4.dt["Contas"]=g4.conta
g5.dt["Contas"]=g5.conta


names(g1.dt)[1]="Valor"
names(g2.dt)[1]="Valor"
names(g3.dt)[1]="Valor"
names(g4.dt)[1]="Valor"
names(g5.dt)[1]="Valor"



Anos = paste(Anos,"0101", sep="")
g1.dt["Anos"]=Anos
g2.dt["Anos"]=Anos
g3.dt["Anos"]=Anos
g4.dt["Anos"]=Anos
g5.dt["Anos"]=Anos


g.dt=(rbind(g1.dt,g2.dt,g3.dt,g4.dt,g5.dt))

rownames(g.dt)=c(1:length(row.names(g.dt)))

g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")

g1.dt$Anos = as.Date(g1.dt$Anos,"%Y%m%d")
g2.dt$Anos = as.Date(g2.dt$Anos,"%Y%m%d")
g3.dt$Anos = as.Date(g3.dt$Anos,"%Y%m%d")
g4.dt$Anos = as.Date(g4.dt$Anos,"%Y%m%d")
g5.dt$Anos = as.Date(g5.dt$Anos,"%Y%m%d")


g.dt["Anos"]=Anos
g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")
g.dt$Contas = as.factor(g.dt$Contas)

g04 =ggplot(data=g.dt, aes(x=Anos ,y = Valor, group = Contas, colour= Contas, shape= Contas))+
  geom_line(col="gray70",size= 0.8, linetype = 3 )+
  # scale_color_manual(breaks = c(g1.class[1], g2.class[1],
  #                               g3.class[1],g4.class[1], g5.class[1]),
  scale_color_manual(values=c("tomato", "cornflowerblue","springgreen3","chocolate1","orchid"))+
  labs( x="Ano", y="Ativos (%)") +
  ggtitle("An. Vertical (% do Ativo Total)")+
  scale_y_continuous(labels = function(z) paste0(z*100, "%"), breaks=seq(-0.1,1.1,by= 0.2), limits=c(-0.1,1.1))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_label_repel(
    aes(x=Anos ,y = Valor, label = paste0(f.dec(Valor*100,2), "%")),
    box.padding = 0.35, point.padding = 0.35,
    size = 3, nudge_y = 0.05,
    segment.color = "grey70") +
  geom_point(aes(shape = Contas),size = 3 , show.legend = TRUE) +
  scale_shape_manual(values=c(15, 16, 17, 18, 20))+
  theme_bw()+
  theme(panel.border = element_rect(fill=NA,color="black", size=.5, linetype="solid"),
        plot.title = element_text(color="black", size=18, face="bold.italic"),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        axis.text.x = element_text(face="plain", color="black", size=14, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=14, angle=0))
# guides(group=g.dt, col = guide_legend(nrow=1))
g04 = g04 +
  theme(legend.position = c(0.20, 0.85), legend.title = element_text(size = 7),
        legend.text = element_text(size = 5.5), legend.key.size = unit(0.5, "cm"),
        legend.background = element_rect(fill="white", size=1, linetype="solid"))

print(g04)
#saveplot(g04,"/g04.png")



########################################################
# VISUALIZAR ANALISE DA RECEITA LIQUIDA
#######################################################
#FICOU COMO DESAFIO



########################################################
# VISUALIZAR ANALISE HORIZONTAL
#######################################################
#Ativo
m.Ativo.aj.med = as.data.frame(sapply(m.Ativo.aj.med, as.numeric))
colnames(m.Ativo.aj.med)=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
m.Ativo.aj.ah = m.Ativo.aj.med
m.Ativo.aj.ah = as.data.frame(apply(m.Ativo.aj.med, 2, "/", m.Ativo.aj.med[,"2010"]))
rownomes=rownames(m.Ativo)
row.names(m.Ativo.aj.ah)=rownomes
row.names(m.Ativo.aj.med)=rownomes
#m.Ativo.aj.ah[,"2010"]


# Passivo
m.Passivo.aj.med = as.data.frame(sapply(m.Passivo.aj.med, as.numeric))
colnames(m.Passivo.aj.med)=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
m.Passivo.aj.ah = m.Passivo.aj.med
m.Passivo.aj.ah = as.data.frame(apply(m.Passivo.aj.med, 2, "/", m.Passivo.aj.med[,"2010"]))
rownomes=rownames(m.Passivo)
row.names(m.Passivo.aj.ah)=rownomes
row.names(m.Passivo.aj.med)=rownomes
# m.Passivo.aj.ah[,"2010"]


#DRE
m.DRE.aj.cm= m.DRE.aj.cm[,1:9]
m.DRE.aj.ah = m.DRE.aj.cm[,1:9]
colnames(m.DRE.aj.cm)=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
m.DRE.aj.ah = as.data.frame(apply(m.DRE.aj.ah, 2, "/", m.DRE.aj.cm[,"2010"]))
rownomes=rownames(m.DRE)
row.names(m.DRE.aj.ah)=rownomes
row.names(m.DRE.aj.cm)=rownomes

# Visualizar dados Passivos AH

rm(g1.dt)
rm(g2.dt)
rm(g3.dt)
rm(g4.dt)
rm(g5.dt)


g1.str="Passivo Circulante - PC"
g2.str= "Passivo Nao Circulante - PNC"
g3.str= "Patrimonio Liquido Consolidado - PL"
Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
g1.dt =as.data.frame(t(subset(m.Passivo.aj.ah[g1.str,],select=Anos)))
g2.dt =as.data.frame(t(subset(m.Passivo.aj.ah[g2.str,],select=Anos)))
g3.dt =as.data.frame(t(subset(m.Passivo.aj.ah[g3.str,],select=Anos)))

g1.class=unlist(strsplit(g1.str," - ",fixed=TRUE))
g2.class=unlist(strsplit(g2.str," - ",fixed=TRUE))
g3.class=unlist(strsplit(g3.str," - ",fixed=TRUE))

g1.conta=c(rep(g1.class[1],9))
g2.conta=c(rep(g2.class[1],9))
g3.conta=c(rep(g3.class[1],9))

g1.dt["Contas"]=g1.conta
g2.dt["Contas"]=g2.conta
g3.dt["Contas"]=g3.conta

names(g1.dt)[1]="Valor"
names(g2.dt)[1]="Valor"
names(g3.dt)[1]="Valor"

Anos = paste(Anos,"0101", sep="")
g1.dt["Anos"]=Anos

g2.dt["Anos"]=Anos

g3.dt["Anos"]=Anos



g.dt=(rbind(g1.dt,g2.dt,g3.dt))

rownames(g.dt)=c(1:length(row.names(g.dt)))

g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")

g1.dt$Anos = as.Date(g1.dt$Anos,"%Y%m%d")
g2.dt$Anos = as.Date(g2.dt$Anos,"%Y%m%d")
g3.dt$Anos = as.Date(g3.dt$Anos,"%Y%m%d")



g.dt["Anos"]=Anos
g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")
g.dt$Contas = as.factor(g.dt$Contas)

g06 = ggplot(data=g.dt, aes(x=Anos ,y = Valor, group = Contas, colour= Contas ))+
  geom_line(col="gray70",size= 0.8, linetype = 3 )+
  labs( x="Ano", y="Passivos & PL %") +
  ggtitle("An. Horizontal (% de 2010)")+
  scale_y_continuous(labels = function(z) paste0(z*100, "%"), breaks=seq(0.4,1.4,by=0.20), limits=c(0.4,1.4))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_label_repel(
    aes(x=Anos ,y = Valor, label = paste0(f.dec(Valor*100,2), "%")),
    box.padding = 0.2, point.padding = 0.2,
    size = 3, nudge_y = 0.05,
    segment.color = "grey70") + geom_point(aes(shape = Contas),size = 3) +
  scale_shape_manual(values=c(15, 16, 17)) + scale_size_manual(values=c(4, 4, 6)) + theme_bw()+
  theme(panel.border = element_rect(fill=NA,color="black", size=.5, linetype="solid"),
        plot.title = element_text(color="black", size=18, face="bold.italic"),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        axis.text.x = element_text(face="plain", color="black", size=14, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=14, angle=0))
# guides(group=g.dt, col = guide_legend(nrow=1))

g06= g06 +
  theme(legend.position = c(0.20, 0.20), legend.title = element_text(size = 7),
        legend.text = element_text(size = 7), legend.key.size = unit(0.5, "cm"),
        legend.background = element_rect(fill="white", size=1, linetype="solid"))


print(g06)
#saveplot(g06,"/g06.png")


#Visualizar dados do Ativo: AH

rm(g1.dt)
rm(g2.dt)
rm(g3.dt)
rm(g4.dt)
rm(g5.dt)


g1.str="Caixa e Equivalentes de Caixa - AC"
g2.str= "Ativo Circulante - AC"
g3.str= "Investimentos - ANC"
g4.str="Imobilizado - ANC"
g5.str= "Ativo Nao Circulante - ANC"


Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
g1.dt =as.data.frame(t(subset(m.Ativo.aj.ah[g1.str,],select=Anos)))
g2.dt =as.data.frame(t(subset(m.Ativo.aj.ah[g2.str,],select=Anos)))
g3.dt =as.data.frame(t(subset(m.Ativo.aj.ah[g3.str,],select=Anos)))
g4.dt =as.data.frame(t(subset(m.Ativo.aj.ah[g4.str,],select=Anos)))
g5.dt =as.data.frame(t(subset(m.Ativo.aj.ah[g5.str,],select=Anos)))

g1.class=unlist(strsplit(g1.str," - ",fixed=TRUE))
g2.class=unlist(strsplit(g2.str," - ",fixed=TRUE))
g3.class=unlist(strsplit(g3.str," - ",fixed=TRUE))
g4.class=unlist(strsplit(g4.str," - ",fixed=TRUE))
g5.class=unlist(strsplit(g5.str," - ",fixed=TRUE))

g1.conta=c(rep(g1.class[1],9))
g2.conta=c(rep(g2.class[1],9))
g3.conta=c(rep(g3.class[1],9))
g4.conta=c(rep(g4.class[1],9))
g5.conta=c(rep(g5.class[1],9))


g1.dt["Contas"]=g1.conta
g2.dt["Contas"]=g2.conta
g3.dt["Contas"]=g3.conta
g4.dt["Contas"]=g4.conta
g5.dt["Contas"]=g5.conta


names(g1.dt)[1]="Valor"
names(g2.dt)[1]="Valor"
names(g3.dt)[1]="Valor"
names(g4.dt)[1]="Valor"
names(g5.dt)[1]="Valor"



Anos = paste(Anos,"0101", sep="")
g1.dt["Anos"]=Anos
g2.dt["Anos"]=Anos
g3.dt["Anos"]=Anos
g4.dt["Anos"]=Anos
g5.dt["Anos"]=Anos


g.dt=(rbind(g1.dt,g2.dt,g3.dt,g4.dt,g5.dt))

rownames(g.dt)=c(1:length(row.names(g.dt)))

g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")

g1.dt$Anos = as.Date(g1.dt$Anos,"%Y%m%d")
g2.dt$Anos = as.Date(g2.dt$Anos,"%Y%m%d")
g3.dt$Anos = as.Date(g3.dt$Anos,"%Y%m%d")
g4.dt$Anos = as.Date(g4.dt$Anos,"%Y%m%d")
g5.dt$Anos = as.Date(g5.dt$Anos,"%Y%m%d")


g.dt["Anos"]=Anos
g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")
g.dt$Contas = as.factor(g.dt$Contas)

g07 =ggplot(data=g.dt, aes(x=Anos ,y = Valor, group = Contas, colour= Contas, shape= Contas))+
  geom_line(col="gray70",size= 0.8, linetype = 3 )+
  # scale_color_manual(breaks = c(g1.class[1], g2.class[1],
  #                               g3.class[1],g4.class[1], g5.class[1]),
  scale_color_manual(values=c("tomato", "cornflowerblue","springgreen3","chocolate1","orchid"))+
  labs( x="Ano", y="Ativos (%)") +
  ggtitle("An. Horizontal (% de 2010)")+
  scale_y_continuous(labels = function(z) paste0(z*100, "%"), breaks=seq(0.0,1.4,by= 0.2), limits=c(0.0,1.4))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_label_repel(
    aes(x=Anos ,y = Valor, label = paste0(f.dec(Valor*100,2), "%")),
    box.padding = 0.35, point.padding = 0.15,
    size = 3, nudge_y = 0.25,
    segment.color = "grey70") +
  geom_point(aes(shape = Contas),size = 3, show.legend = TRUE) +
  scale_shape_manual(values=c(15, 16, 17, 18, 20))+
  theme_bw()+
  theme(panel.border = element_rect(fill=NA,color="black", size=.5, linetype="solid"),
        plot.title = element_text(color="black", size=18, face="bold.italic"),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        axis.text.x = element_text(face="plain", color="black", size=14, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=14, angle=0))
# guides(group=g.dt, col = guide_legend(nrow=1))
g07 = g07 +
  theme(legend.position = c(0.12, 0.15), legend.title = element_text(size = 6),
        legend.text = element_text(size = 6), legend.key.size = unit(0.5, "cm"),
        legend.background = element_rect(fill="white", size=1, linetype="solid"))

print(g07)
#saveplot(g07, 'g07')



#########################
# Analise DRE
#Visualizar dados do DRE: AH

rm(g1.dt)
rm(g2.dt)
rm(g3.dt)
rm(g4.dt)
rm(g5.dt)

g1.str= "Resultado Bruto - DRE"
g2.str= "Resultado Antes do Resultado Financeiro e dos Tributos - DRE"
g3.str="Resultado Antes dos Tributos sobre o Lucro - DRE"
g4.str= "Lucro/Prejuizo Consolidado do Periodo - DRE"
g5.str= "Custo dos Bens e/ou Servicos Vendidos - DRE"

Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
g1.dt =as.data.frame(t(m.DRE.aj.ah[g1.str,1:9]))
g2.dt =as.data.frame(t(m.DRE.aj.ah[g2.str,1:9]))
g3.dt =as.data.frame(t(m.DRE.aj.ah[g3.str,1:9]))
g4.dt =as.data.frame(t(m.DRE.aj.ah[g4.str,1:9]))
g5.dt =as.data.frame(t(m.DRE.aj.ah[g5.str,1:9]))


#g5.dt=-1*g5.dt

g1.class=unlist(strsplit(g1.str," - ",fixed=TRUE))
g2.class=unlist(strsplit(g2.str," - ",fixed=TRUE))
g3.class=unlist(strsplit(g3.str," - ",fixed=TRUE))
g4.class=unlist(strsplit(g4.str," - ",fixed=TRUE))
g5.class=unlist(strsplit(g5.str," - ",fixed=TRUE))

g1.conta=c(rep(g1.class[1],9))
g2.conta=c(rep(g2.class[1],9))
g3.conta=c(rep(g3.class[1],9))
g4.conta=c(rep(g4.class[1],9))
g5.conta=c(rep(g5.class[1],9))

g1.dt["Contas"]=g1.conta
g2.dt["Contas"]=g2.conta
g3.dt["Contas"]=g3.conta
g4.dt["Contas"]=g4.conta
g5.dt["Contas"]=g5.conta


names(g1.dt)[1]="Valor"
names(g2.dt)[1]="Valor"
names(g3.dt)[1]="Valor"
names(g4.dt)[1]="Valor"
names(g5.dt)[1]="Valor"



Anos = paste(Anos,"0101", sep="")
g1.dt["Anos"]=Anos
g2.dt["Anos"]=Anos
g3.dt["Anos"]=Anos
g4.dt["Anos"]=Anos
g5.dt["Anos"]=Anos


g.dt=(rbind(g1.dt,g2.dt,g3.dt,g4.dt,g5.dt))

rownames(g.dt)=c(1:length(row.names(g.dt)))

g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")

g1.dt$Anos = as.Date(g1.dt$Anos,"%Y%m%d")
g2.dt$Anos = as.Date(g2.dt$Anos,"%Y%m%d")
g3.dt$Anos = as.Date(g3.dt$Anos,"%Y%m%d")
g4.dt$Anos = as.Date(g4.dt$Anos,"%Y%m%d")
g5.dt$Anos = as.Date(g5.dt$Anos,"%Y%m%d")


g.dt["Anos"]=Anos
g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")
g.dt$Contas = as.factor(g.dt$Contas)

g08 =ggplot(data=g.dt, aes(x=Anos ,y = Valor, group = Contas, colour= Contas, shape= Contas))+
  geom_line(col="gray70",size= 0.8, linetype = 3 )+
  # scale_color_manual(breaks = c(g1.class[1], g2.class[1],
  #                               g3.class[1],g4.class[1], g5.class[1]),
  scale_color_manual(values=c("tomato", "cornflowerblue","springgreen3","chocolate1","orchid"))+
  labs( x="Ano", y="DRE (%)") +
  ggtitle("An. Horizontal (% de 2010)")+
  scale_y_continuous(labels = function(z) paste0(z*100, "%"), breaks=seq(-2.0,1.5,by= 0.5), limits=c(-2.0,1.5))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_label_repel(
    aes(x=Anos ,y = Valor, label = paste0(f.dec(Valor*100,2), "%")),
    box.padding = 0.2, point.padding = 0.2,
    size = 3 , nudge_y = 0.20,
    segment.color = "grey70") +
  geom_point(aes(shape = Contas),size = 3 , show.legend = TRUE) +
  scale_shape_manual(values=c(15, 16, 17, 18, 20))+
  theme_bw()+
  theme(panel.border = element_rect(fill=NA,color="black", size=.5, linetype="solid"),
        plot.title = element_text(color="black", size=18, face="bold.italic"),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        axis.text.x = element_text(face="plain", color="black", size=14, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=14, angle=0))
# guides(group=g.dt, col = guide_legend(nrow=1))

g08= g08 +
  theme(legend.position = c(0.25, 0.25), legend.title = element_text(size = 7),
        legend.text = element_text(size = 7), legend.key.size = unit(0.4, "cm"),
        legend.background = element_rect(fill="white", size=1, linetype="solid"))


print(g08)
# saveplot(g08, 'g08')



















#########################################
# Análise de Tendências (VIII)
# Contas da Dinâmica Operacional
########################################
# Capital de Giro Liquido
Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")

cgl.ac=as.numeric(m.Ativo.aj.med["Ativo Circulante - AC",])
cgl.pc=as.numeric(m.Passivo.aj.med["Passivo Circulante - PC",])

cgl=cgl.ac - cgl.pc
cgl=as.data.frame(cgl)
colnames(cgl)=c("Capital de Giro Liquido")
rownames(cgl)= Anos

# at.ciclico
at.ciclico.caixa=as.numeric(m.Ativo.aj.med["Caixa e Equivalentes de Caixa - AC",])
at.ciclico.apfinan=as.numeric(m.Ativo.aj.med["Aplicacoes Financeiras - AC",])

at.ciclico=cgl.ac-at.ciclico.caixa-at.ciclico.apfinan
at.ciclico=as.data.frame(at.ciclico)
colnames(at.ciclico)=c("Ativo Ciclico")
rownames(at.ciclico)= Anos


#pa.ciclico

pa.ciclico.empfinan=as.numeric(m.Passivo.aj.med["Emprestimos e Financiamentos - PC",])
pa.ciclico.intfinan=as.numeric(m.Passivo.aj.med["Instrumentos Financeiros - PC",])
pa.ciclico.forfaiting=as.numeric(m.Passivo.aj.med["Titulos a pagar - Forfaiting - PC",])


pa.ciclico=cgl.pc-pa.ciclico.empfinan-pa.ciclico.intfinan-pa.ciclico.forfaiting
pa.ciclico=as.data.frame(pa.ciclico)
colnames(pa.ciclico)=c("Passivo Ciclico")
rownames(pa.ciclico)= Anos
pa.ciclico=as.data.frame(pa.ciclico)

#at.financ
at.finan=cgl.ac-at.ciclico
at.finan=as.data.frame(at.finan)
colnames(at.finan)=c("Ativo Financeiro")
rownames(at.finan)= Anos
at.finan=as.data.frame(at.finan)


#pa.financ
pa.finan=cgl.pc-pa.ciclico
pa.finan=as.data.frame(pa.finan)
colnames(pa.finan)=c("Passivo Financeiro")
rownames(pa.finan)= Anos
pa.finan=as.data.frame(pa.finan)


# necessidade de investimento de giro

nig=at.ciclico-pa.ciclico
nig=as.data.frame(nig)
colnames(nig)=c("Necessidade de Investimento de Giro")
rownames(nig)= Anos
nig=as.data.frame(nig)


# saldo disponivel

sdisp=cgl-nig
sdisp=as.data.frame(sdisp)
colnames(sdisp)=c("Saldo de Disponivel")
rownames(sdisp)= Anos
sdisp=as.data.frame(sdisp)

#  Necessidade Total de Financiamento Permanente - NTFP
at.perm=as.numeric(m.Ativo.aj.med["Imobilizado - ANC",])+as.numeric(m.Ativo.aj.med["Investimentos - ANC",])
ntfp=nig+at.perm
ntfp=as.data.frame(ntfp)
colnames(ntfp)=c("Necessidade Total de Financiamento Permanente")
rownames(ntfp)= Anos
ntfp=as.data.frame(ntfp)


colnames(at.ciclico)=c("Ativo Ciclico")
rownames(at.ciclico)= Anos
at.ciclico=as.data.frame(at.ciclico)

colnames(pa.ciclico)=c("Passivo Ciclico")
rownames(pa.ciclico)= Anos
pa.ciclico=as.data.frame(pa.ciclico)

#### Visualizar COmpartivos entre Ativo Circulante, Ciclico e Financeiro
# Visualizar AC ciclico e financeiro

rm(g1.dt)
rm(g2.dt)
rm(g3.dt)
rm(g4.dt)
rm(g5.dt)

g1.str= "Ativo Circulante - AC"
g2.str= "Ativo Ciclico"
g3.str= "Ativo Financeiro"

Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
g1.dt = as.data.frame( t(m.Ativo.aj.med[g1.str,1:9]) )
g2.dt = at.ciclico
g3.dt = at.finan


g1.class[1]=g1.str
g2.class[1]=g2.str
g3.class[1]=g3.str


g1.conta=c(rep(g1.class[1],9))
g2.conta=c(rep(g2.class[1],9))
g3.conta=c(rep(g3.class[1],9))

g1.dt["Contas"]=g1.conta
g2.dt["Contas"]=g2.conta
g3.dt["Contas"]=g3.conta

names(g1.dt)[1]="Valor"
names(g2.dt)[1]="Valor"
names(g3.dt)[1]="Valor"

Anos = paste(Anos,"0101", sep="")
g1.dt["Anos"]=Anos

g2.dt["Anos"]=Anos

g3.dt["Anos"]=Anos

g.dt=(rbind(g1.dt,g2.dt,g3.dt))

rownames(g.dt)=c(1:length(row.names(g.dt)))

g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")

g1.dt$Anos = as.Date(g1.dt$Anos,"%Y%m%d")
g2.dt$Anos = as.Date(g2.dt$Anos,"%Y%m%d")
g3.dt$Anos = as.Date(g3.dt$Anos,"%Y%m%d")

g.dt["Anos"]=Anos
g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")
g.dt$Contas = as.factor(g.dt$Contas)

g09 = ggplot(data=g.dt, aes(x=Anos ,y = Valor, group = Contas, colour= Contas ))+
  geom_line(col="gray70",size= 0.8, linetype = 3 )+
  labs( x="Ano", y="Ativos (Bilhões R$)") +
  ggtitle("Comparativo de Ativos Circulante, Ciclico e Financeiro")+
  scale_y_continuous(labels = function(z) paste0(z/1000000, " Bi R$"), breaks=seq(0,20000000,by= 2500000), limits=c(0,20000000))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_label_repel(
    aes(x=Anos ,y = Valor, label = paste0(f.dec(Valor/1000000,3), " Bi")),
    box.padding = 0.2, point.padding = 0.2,
    size = 3, nudge_y = 0.15,
    segment.color = "grey70") + geom_point(aes(shape = Contas),size =3) +
  scale_shape_manual(values=c(15, 16, 17)) + scale_size_manual(values=c(4, 4, 6)) + theme_bw()+
  theme(panel.border = element_rect(fill=NA,color="black", size=.5, linetype="solid"),
        plot.title = element_text(color="black", size=18, face="bold.italic"),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        axis.text.x = element_text(face="plain", color="black", size=14, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=14, angle=0))
# guides(group=g.dt, col = guide_legend(nrow=1))
g09= g09 +
  # scale_colour_grey(start = 0.3, end = 0.4) +
  theme(legend.position = c(0.80, 0.80), legend.title = element_text(size = 8),
        legend.text = element_text(size = 8), legend.key.size = unit(0.5, "cm"),
        legend.background = element_rect(fill="white", size=1, linetype="solid"))

print(g09)
saveplot(g09, 'g09')




##############################################
# COMPARACAO PASSIVO
#


# Visualizar PC, cíclico e financeiro

rm(g1.dt)
rm(g2.dt)
rm(g3.dt)
rm(g4.dt)
rm(g5.dt)


g1.str= "Passivo Circulante - PC"
g2.str= "Passivo Ciclico"
g3.str= "Passivo Financeiro"

Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
g1.dt = as.data.frame( t(m.Passivo.aj.med[g1.str,1:9]) )
g2.dt = pa.ciclico
g3.dt = pa.finan

g1.class[1]=g1.str
g2.class[1]=g2.str
g3.class[1]=g3.str

g1.conta=c(rep(g1.class[1],9))
g2.conta=c(rep(g2.class[1],9))
g3.conta=c(rep(g3.class[1],9))

g1.dt["Indicadores"]=g1.conta
g2.dt["Indicadores"]=g2.conta
g3.dt["Indicadores"]=g3.conta

names(g1.dt)[1]="Valor"
names(g2.dt)[1]="Valor"
names(g3.dt)[1]="Valor"

Anos = paste(Anos,"0101", sep="")
g1.dt["Anos"]=Anos

g2.dt["Anos"]=Anos

g3.dt["Anos"]=Anos

g.dt=(rbind(g1.dt,g2.dt,g3.dt))

rownames(g.dt)=c(1:length(row.names(g.dt)))

g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")

g1.dt$Anos = as.Date(g1.dt$Anos,"%Y%m%d")
g2.dt$Anos = as.Date(g2.dt$Anos,"%Y%m%d")
g3.dt$Anos = as.Date(g3.dt$Anos,"%Y%m%d")


g.dt["Anos"]=Anos
g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")
# g.dt$Contas = as.factor(g.dt$Contas)

g10 = ggplot(data=g.dt, aes(x=Anos ,y = Valor, group = Indicadores, colour= Indicadores ))+
  geom_line(col="gray70",size= 0.8, linetype = 3 )+
  labs( x="Ano", y="Passivos (Bilhões R$)") +
  ggtitle("Comparativo de Passivos Circulante, Ciclico e Financeiro")+
  scale_y_continuous(labels = function(z) paste0(z/1000000, " Bi R$"), breaks=seq(0,7000000,by= 1000000), limits=c(0,7000000))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_label_repel(
    aes(x=Anos ,y = Valor, label = paste0(f.dec(Valor/1000000,3), " Bi")),
    box.padding = 0.2, point.padding = 0.2,
    size = 3, nudge_y = 0.15,
    segment.color = "grey70") + geom_point(aes(shape = Indicadores),size =3.5) +
  scale_shape_manual(values=c(15, 16, 17)) + scale_size_manual(values=c(4, 4, 6)) + theme_bw()+
  theme(panel.border = element_rect(fill=NA,color="black", size=.5, linetype="solid"),
        plot.title = element_text(color="black", size=18, face="bold.italic"),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        axis.text.x = element_text(face="plain", color="black", size=14, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=14, angle=0))
# guides(group=g.dt, col = guide_legend(nrow=1))
g10= g10 +
  # scale_colour_grey(start = 0.3, end = 0.4) +
  theme(legend.position = c(0.85, 0.80), legend.title = element_text(size = 8),
        legend.text = element_text(size = 8), legend.key.size = unit(0.5, "cm"),
        legend.background = element_rect(fill="white", size=1, linetype="solid"))

print(g10)
saveplot(g10, 'g10')


###########################################
#  Análise de Tendências (X)
# Contas da Dinâmica Operacional
###########################################
# Visualizar CGL, NIG e SD

rm(g1.dt)
rm(g2.dt)
rm(g3.dt)
rm(g4.dt)
rm(g5.dt)


g1.str= "Capital de Giro Liquido"
g2.str= "Necessidade de Investimento de Giro"
g3.str= "Saldo de Disponivel"
g4.str= "Necessidade Total de Financiamento Permanente"


Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
g1.dt = cgl
g2.dt = nig
g3.dt = sdisp
g4.dt = ntfp


g1.class[1]=g1.str
g2.class[1]=g2.str
g3.class[1]=g3.str
g4.class[1]=g4.str


g1.conta=c(rep(g1.class[1],9))
g2.conta=c(rep(g2.class[1],9))
g3.conta=c(rep(g3.class[1],9))
g4.conta=c(rep(g4.class[1],9))

g1.dt["Contas"]=g1.conta
g2.dt["Contas"]=g2.conta
g3.dt["Contas"]=g3.conta
g4.dt["Contas"]=g4.conta

names(g1.dt)[1]="Valor"
names(g2.dt)[1]="Valor"
names(g3.dt)[1]="Valor"
names(g4.dt)[1]="Valor"


Anos = paste(Anos,"0101", sep="")
g1.dt["Anos"]=Anos

g2.dt["Anos"]=Anos

g3.dt["Anos"]=Anos

g4.dt["Anos"]=Anos


g.dt=(rbind(g1.dt,g2.dt,g3.dt,g4.dt))

rownames(g.dt)=c(1:length(row.names(g.dt)))

g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")

g1.dt$Anos = as.Date(g1.dt$Anos,"%Y%m%d")
g2.dt$Anos = as.Date(g2.dt$Anos,"%Y%m%d")
g3.dt$Anos = as.Date(g3.dt$Anos,"%Y%m%d")
g4.dt$Anos = as.Date(g4.dt$Anos,"%Y%m%d")


g.dt["Anos"]=Anos
g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")
g.dt$Contas = as.factor(g.dt$Contas)

g11 = ggplot(data=g.dt, aes(x=Anos ,y = Valor, group = Contas, colour= Contas ))+
  geom_line(col="gray70",size= 0.8, linetype = 3 )+
  scale_color_manual(values=c("tomato", "cornflowerblue","springgreen3","chocolate1"))+
  labs( x="Ano", y="Ativos (Bilhões R$)") +
  ggtitle("Comparativo de Indicadores de Giro")+
  scale_y_continuous(labels = function(z) paste0(z/1000000, " Bi R$"), breaks=seq(-5000000,34000000,by= 5000000), limits=c(-5000000,34000000))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_label_repel(
    aes(x=Anos ,y = Valor, label = paste0(f.dec(Valor/1000000,3), " Bi")),
    box.padding = 0.2, point.padding = 0.2,
    size = 3 , nudge_y = 0.25,
    segment.color = "grey70") + geom_point(aes(shape = Contas),size =3) +
  scale_shape_manual(values=c(15, 16, 17, 18)) + scale_size_manual(values=c(4, 4, 6, 6)) + theme_bw()+
  theme(panel.border = element_rect(fill=NA,color="black", size=.5, linetype="solid"),
        plot.title = element_text(color="black", size=18, face="bold.italic"),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        axis.text.x = element_text(face="plain", color="black", size=14, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=14, angle=0))
# guides(group=g.dt, col = guide_legend(nrow=1))
g11= g11 +
  # scale_colour_grey(start = 0.3, end = 0.4) +
  theme(legend.position = c(0.81, 0.86), legend.title = element_text(size = 7),
        legend.text = element_text(size = 6), legend.key.size = unit(0.4, "cm"),
        legend.background = element_rect(fill="white", size=1, linetype="solid"))

print(g11)
saveplot(g11,'g11')







####################################################
#    Análise da Estrutura de Capital (I)
#    Indicadores do Endividamento
####################################################
#Índices de endividamento

Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")

# Capital de Terceiros
ind.cap.terc=(m.Passivo.aj.med["Passivo Circulante - PC",]+m.Passivo.aj.med["Passivo Nao Circulante - PNC",])/(m.Passivo.aj.med["Passivo Total - PT",])
ind.cap.terc=as.data.frame(t(ind.cap.terc))
colnames(ind.cap.terc)=c("Capital de Terceiros")
rownames(ind.cap.terc)= Anos
ind.cap.terc=as.data.frame(ind.cap.terc)

# Capital Proprio
ind.cap.prop=m.Passivo.aj.med["Patrimonio Liquido Consolidado - PL",]/(m.Passivo.aj.med["Passivo Total - PT",])
ind.cap.prop=as.data.frame(t(ind.cap.prop))
colnames(ind.cap.prop)=c("Capital Proprio")
rownames(ind.cap.prop)= Anos
ind.cap.prop=as.data.frame(ind.cap.prop)


# Qualidade do vencimento
ind.qual.vencim=m.Passivo.aj.med["Passivo Circulante - PC",]/m.Passivo.aj.med["Patrimonio Liquido Consolidado - PL",]
ind.qual.vencim=as.data.frame(t(ind.qual.vencim))
colnames(ind.qual.vencim)=c("Qualidade do Vencimento")
rownames(ind.qual.vencim)= Anos
ind.qual.vencim=as.data.frame(ind.qual.vencim)

# Passivo Oneroso - Qualidade do Passivo Oneroso
pa.oneroso=m.Passivo.aj.med["Passivo Nao Circulante - PNC",]+m.Passivo.aj.med["Emprestimos e Financiamentos - PC",]+m.Passivo.aj.med["Instrumentos Financeiros - PC",]+m.Passivo.aj.med["Titulos a pagar - Forfaiting - PC",]
ind.pa.onero=pa.oneroso/m.Passivo.aj.med["Passivo Total - PT",]
ind.pa.onero=as.data.frame(t(ind.pa.onero))
colnames(ind.pa.onero)=c("Qualidade do Passivo Oneroso")
rownames(ind.pa.onero)= Anos
ind.pa.onero=as.data.frame(ind.pa.onero)

#Grau de Endividamento
ind.grau.end=(m.Passivo.aj.med["Passivo Circulante - PC",]+m.Passivo.aj.med["Passivo Nao Circulante - PNC",])/m.Passivo.aj.med["Patrimonio Liquido Consolidado - PL",]
ind.grau.end=as.data.frame(t(ind.grau.end))
colnames(ind.grau.end)=c("Grau de Endividamento")
rownames(ind.grau.end)= Anos
ind.grau.end=as.data.frame(ind.grau.end)


#Grau de Endividamento Oneroso
ind.grau.end.onero=pa.oneroso/m.Passivo.aj.med["Patrimonio Liquido Consolidado - PL",]
ind.grau.end.onero=as.data.frame(t(ind.grau.end.onero))
colnames(ind.grau.end.onero)=c("Grau de Endividamento Oneroso")
rownames(ind.grau.end.onero)= Anos
ind.grau.end.onero=as.data.frame(ind.grau.end.onero)


v.PL.aj=m.Passivo.aj.med["Patrimonio Liquido Consolidado - PL",]
rownames(v.PL.aj)=c("Patrimonio Liquido Consolidado - PL")

v.juros=m.DFC.aj.cm["Despesas de Juros - DFC-O",1:9]
v.pa.onero.emp=m.Passivo.aj.med["Emprestimos e Financiamentos - PNC",]+
  m.Passivo.aj.med["Emprestimos e Financiamentos - PC",]+
  m.Passivo.aj.med["Instrumentos Financeiros - PC",]+
  m.Passivo.aj.med["Titulos a pagar - Forfaiting - PC",]
v.invest=v.pa.onero.emp+v.PL.aj

rownames(v.juros)=c("Juros")


##################################
# Visualização do endividamento
##################################
rm(g1.dt)
rm(g2.dt)
rm(g3.dt)
rm(g4.dt)
rm(g5.dt)



g1.str="Capital de Terceiros"
g2.str= "Capital Proprio"
g3.str= "Qualidade do Vencimento"
g4.str="Qualidade do Passivo Oneroso"
g5.str= "Grau de Endividamento"
g6.str= "Grau de Endividamento Oneroso"


Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
g1.dt =as.data.frame(ind.cap.terc)
g2.dt =as.data.frame(ind.cap.prop)
g3.dt =as.data.frame(ind.qual.vencim)
g4.dt =as.data.frame(ind.pa.onero)
g5.dt =as.data.frame(ind.grau.end)
g6.dt =as.data.frame(ind.grau.end.onero)

g1.conta=c(rep(g1.str,9))
g2.conta=c(rep(g2.str,9))
g3.conta=c(rep(g3.str,9))
g4.conta=c(rep(g4.str,9))
g5.conta=c(rep(g5.str,9))
g6.conta=c(rep(g6.str,9))


g1.dt["Endiv."]=g1.conta
g2.dt["Endiv."]=g2.conta
g3.dt["Endiv."]=g3.conta
g4.dt["Endiv."]=g4.conta
g5.dt["Endiv."]=g5.conta
g6.dt["Endiv."]=g6.conta

names(g1.dt)[1]="Valor"
names(g2.dt)[1]="Valor"
names(g3.dt)[1]="Valor"
names(g4.dt)[1]="Valor"
names(g5.dt)[1]="Valor"
names(g6.dt)[1]="Valor"


Anos = paste(Anos,"0101", sep="")
g1.dt["Anos"]=Anos
g2.dt["Anos"]=Anos
g3.dt["Anos"]=Anos
g4.dt["Anos"]=Anos
g5.dt["Anos"]=Anos
g6.dt["Anos"]=Anos

g.dt=(rbind(g1.dt,g2.dt,g3.dt,g4.dt,g5.dt,g6.dt))

rownames(g.dt)=c(1:length(row.names(g.dt)))

g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")

g1.dt$Anos = as.Date(g1.dt$Anos,"%Y%m%d")
g2.dt$Anos = as.Date(g2.dt$Anos,"%Y%m%d")
g3.dt$Anos = as.Date(g3.dt$Anos,"%Y%m%d")
g4.dt$Anos = as.Date(g4.dt$Anos,"%Y%m%d")
g5.dt$Anos = as.Date(g5.dt$Anos,"%Y%m%d")
g6.dt$Anos = as.Date(g6.dt$Anos,"%Y%m%d")

g.dt["Anos"]= Anos
g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")
#g.dt$Contas = as.factor(g.dt$Contas)

g13 =ggplot(data=g.dt, aes(x=Anos ,y = Valor, group = Endiv., colour= Endiv., shape= Endiv.))+
  geom_line(col="gray70",size= 0.8, linetype = 3 )+
  # scale_color_manual(breaks = c(g1.class[1], g2.class[1],
  #                               g3.class[1],g4.class[1], g5.class[1]),
  scale_color_manual(values=c("tomato", "cornflowerblue","springgreen3","chocolate1","orchid","cyan4"))+
  labs( x="Ano", y="Indice (%)") +
  ggtitle("An. de Endividamento (Estrutura de Capital)")+
  scale_y_continuous(labels = function(z) paste0(z*100, " %"), breaks=seq(0,1.2,by= 0.2), limits=c(0,1.2))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_label_repel(
    aes(x=Anos ,y = Valor, label = paste0(f.dec(Valor*100,2), " %")),
    box.padding = 0.25, point.padding = 0.25,
    size = 2.5, nudge_y = 0.05,
    segment.color = "grey70") +
  geom_point(aes(shape = Endiv.),size = 3.5, show.legend = TRUE) +
  scale_shape_manual(values=c(15, 16, 17, 18, 19, 20))+
  theme_bw()+
  theme(panel.border = element_rect(fill=NA,color="black", size=.5, linetype="solid"),
        plot.title = element_text(color="black", size=18, face="bold.italic"),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        axis.text.x = element_text(face="plain", color="black", size=14, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=14, angle=0))
# guides(group=g.dt, col = guide_legend(nrow=1))
g13 = g13 +
  
  theme(legend.position = c(0.30, 0.83), legend.title = element_text(size = 5.5),
        legend.text = element_text(size = 5.5), legend.key.size = unit(0.5, "cm"),
        legend.background = element_rect(fill="white", size=1, linetype="solid"))


print(g13)
saveplot(g13, 'g13.png')


#############################################
# Visualização de Juros

# Visualizar Juros, Passivo Oneroso e Investimento

rm(g1.dt)
rm(g2.dt)
rm(g3.dt)
rm(g4.dt)
rm(g1.conta)
rm(g2.conta)
rm(g3.conta)
rm(g4.conta)

rownames(v.juros)=c("Juros")
rownames(v.pa.onero.emp)=c("Passivo Oneroso")
rownames(v.invest)=c("Investimento (Terc + Proprio)")  

g1.str= "Juros"
g2.str= "Passivo Oneroso"
g3.str= "Investimento (Terc + Proprio)"

Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
g1.dt = as.data.frame(t(v.juros))
g2.dt = as.data.frame(t(v.pa.onero.emp))
g3.dt = as.data.frame(t(v.invest))

g1.conta=c(rep(g1.str,9))
g2.conta=c(rep(g2.str,9))
g3.conta=c(rep(g3.str,9))

g1.dt["Valores"]=g1.conta
g2.dt["Valores"]=g2.conta
g3.dt["Valores"]=g3.conta

names(g1.dt)[1]="Valor"
names(g2.dt)[1]="Valor"
names(g3.dt)[1]="Valor"

Anos = paste(Anos,"0101", sep="")
g1.dt["Anos"]=Anos

g2.dt["Anos"]=Anos

g3.dt["Anos"]=Anos

g.dt=(rbind(g1.dt,g2.dt,g3.dt))

rownames(g.dt)=c(1:length(row.names(g.dt)))

g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")

g1.dt$Anos = as.Date(g1.dt$Anos,"%Y%m%d")
g2.dt$Anos = as.Date(g2.dt$Anos,"%Y%m%d")
g3.dt$Anos = as.Date(g3.dt$Anos,"%Y%m%d")

g.dt["Anos"]=Anos
g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")
g.dt$Valores = as.factor(g.dt$Valores)

g14 = ggplot(data=g.dt, aes(x=Anos ,y = Valor, group = Valores, colour= Valores ))+
  geom_line(col="gray70",size= 0.8, linetype = 3 )+
  labs( x="Ano", y="Valores (Bilhões R$)") +
  ggtitle("Indicadores Econômicos do Passivo da Empresa")+
  scale_y_continuous(labels = function(z) paste0(z/1000000, " Bi R$"), breaks=seq(0,45000000,by= 10000000), limits=c(0,45000000))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_label_repel(
    aes(x=Anos ,y = Valor, label = paste0(f.dec(Valor/1000000,3), " Bi")),
    box.padding = 0.35, point.padding = 0.35,
    size = 3, nudge_y = 0.25,
    segment.color = "grey70") + geom_point(aes(shape = Valores),size = 3.5) +
  scale_shape_manual(values=c(15, 16, 17)) + scale_size_manual(values=c(4, 4, 6)) + theme_bw()+
  theme(panel.border = element_rect(fill=NA,color="black", size=.5, linetype="solid"),
        plot.title = element_text(color="black", size=18, face="bold.italic"),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        axis.text.x = element_text(face="plain", color="black", size=14, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=14, angle=0))
# guides(group=g.dt, col = guide_legend(nrow=1))
g14= g14 +
  # scale_colour_grey(start = 0.3, end = 0.4) +
  theme(legend.position = c(0.80, 0.85), legend.title = element_text(size = 8),
        legend.text = element_text(size = 8), legend.key.size = unit(0.5, "cm"),
        legend.background = element_rect(fill="white", size=1, linetype="solid"))

print(g14)
saveplot(g14, 'g14.png')












##########################################################
#         Análise de Lucratividade (I)
##########################################################
# Índices de Lucratividade
Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
v.deprec = m.DFC.aj.cm["Depreciacao, Amortizacao e Exaustao - DFC-O",1:9]
rownames(v.deprec)=c("Depreciacao, Amortizacao e Exaustao")
v.vendas = m.DRE.aj.cm["Receita de Venda de Bens e/ou Servicos - DRE",]
rownames(v.vendas)=c("Receita de Venda de Bens e/ou Servicos")
ind.ebit= m.DRE.aj.cm["Resultado Antes do Resultado Financeiro e dos Tributos - DRE",]-
  m.DRE.aj.cm["Resultado de Equivalencia Patrimonial - DRE",]-
  m.DRE.aj.cm["Outras Despesas Operacionais - DRE",]-
  m.DRE.aj.cm["Outras Receitas Operacionais - DRE",]
ind.ebitda= ind.ebit+v.deprec
ind.nopat= ind.ebit*(1-0.34)

rownames(ind.ebit)=c("EBIT")
rownames(ind.ebitda)=c("EBITA")
rownames(ind.nopat)=c("NOPAT")

ind.noplat= ind.nopat+v.juros*0.34 # NOPLAT= Net operating profit less adjusted taxes
rownames(ind.noplat)=c("NOPLAT")


#Margem do EBIT, EBITDA e NOPAT

v.marg.ebit=ind.ebit/v.vendas
v.marg.ebitda=ind.ebitda/v.vendas  
v.marg.nopat=ind.nopat/v.vendas 

rownames(v.marg.ebit)=c("Margem EBIT")
rownames(v.marg.ebitda)=c("Margem EBITA")
rownames(v.marg.nopat)=c("Margem NOPAT")

rownames(v.deprec)=c("Depreciacao, Amortizacao e Exaustao")
v.lliq = m.DRE.aj.cm["Lucro/Prejuizo Consolidado do Periodo - DRE",]

v.marg.liq=v.lliq/v.vendas 
rownames(v.marg.liq)=c("Margem Liquida")


# VISUALIZANDO -Análise de Lucratividade (II) - Indicadores  
# Visualizar EBIT, EBITDA, NOPAT, NOPLAT

rm(g1.dt)
rm(g2.dt)
rm(g3.dt)
rm(g4.dt)
rm(g1.conta)
rm(g2.conta)
rm(g3.conta)
rm(g4.conta)


g1.str= "EBIT"
g2.str= "EBITDA"
g3.str= "NOPAT"
g4.str= "NOPLAT"


Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
g1.dt = as.data.frame(t(ind.ebit))
g2.dt = as.data.frame(t(ind.ebitda))
g3.dt = as.data.frame(t(ind.nopat))
g4.dt = as.data.frame(t(ind.noplat))


g1.conta=c(rep(g1.str,9))
g2.conta=c(rep(g2.str,9))
g3.conta=c(rep(g3.str,9))
g4.conta=c(rep(g4.str,9))

g1.dt["Indicadores"]=g1.conta
g2.dt["Indicadores"]=g2.conta
g3.dt["Indicadores"]=g3.conta
g4.dt["Indicadores"]=g4.conta

names(g1.dt)[1]="Valor"
names(g2.dt)[1]="Valor"
names(g3.dt)[1]="Valor"
names(g4.dt)[1]="Valor"


Anos = paste(Anos,"0101", sep="")
g1.dt["Anos"]=Anos

g2.dt["Anos"]=Anos

g3.dt["Anos"]=Anos

g4.dt["Anos"]=Anos


g.dt=(rbind(g1.dt,g2.dt,g3.dt,g4.dt))

rownames(g.dt)=c(1:length(row.names(g.dt)))

g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")

g1.dt$Anos = as.Date(g1.dt$Anos,"%Y%m%d")
g2.dt$Anos = as.Date(g2.dt$Anos,"%Y%m%d")
g3.dt$Anos = as.Date(g3.dt$Anos,"%Y%m%d")
g4.dt$Anos = as.Date(g4.dt$Anos,"%Y%m%d")


g.dt["Anos"]=Anos
g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")
g.dt$Indicadores = as.factor(g.dt$Indicadores)

g15 = ggplot(data=g.dt, aes(x=Anos ,y = Valor, group = Indicadores, colour= Indicadores ))+
  geom_line(col="gray70",size= 0.8, linetype = 3 )+
  labs( x="Ano", y="Indicadores (Bilh?es R$)") +
  ggtitle("Comparativo de Indicadores de Lucratividade")+
  scale_y_continuous(labels = function(z) paste0(z/1000000, " Bi R$"), breaks=seq(-1000000,3800000,by= 500000), limits=c(-1000000,3800000))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_label_repel(
    aes(x=Anos ,y = Valor, label = paste0(f.dec(Valor/1000000,3), " Bi")),
    box.padding = 0.2, point.padding = 0.2,
    size = 3.5, nudge_y = 0.15,
    segment.color = "grey70") + geom_point(aes(shape = Indicadores),size =4.5) +
  scale_shape_manual(values=c(15, 16, 17, 18)) + scale_size_manual(values=c(4, 4, 6, 6)) + theme_bw()+
  theme(panel.border = element_rect(fill=NA,color="black", size=.5, linetype="solid"),
        plot.title = element_text(color="black", size=18, face="bold.italic"),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        axis.text.x = element_text(face="plain", color="black", size=14, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=14, angle=0))
# guides(group=g.dt, col = guide_legend(nrow=1))
g15= g15 +
  # scale_colour_grey(start = 0.3, end = 0.4) +
  theme(legend.position = c(0.60, 0.80),
        legend.background = element_rect(fill="white", size=1, linetype="solid"))

print(g15)
saveplot(g14, 'g15.png')









#################################################
#     Análise da Lucratividade (III) - Margens
#################################################
rm(g1.dt)
rm(g2.dt)
rm(g3.dt)

g1.str="Margem EBIT"
g2.str= "Margem EBITDA"
g3.str= "Margem NOPAT"
g4.str= "Margem Líquida"

Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
g1.dt =as.data.frame(t(v.marg.ebit))
g2.dt =as.data.frame(t(v.marg.ebitda))
g3.dt =as.data.frame(t(v.marg.nopat))
g4.dt =as.data.frame(t(v.marg.liq))

g1.conta=c(rep(g1.str,9))
g2.conta=c(rep(g2.str,9))
g3.conta=c(rep(g3.str,9))
g4.conta=c(rep(g4.str,9))

g1.dt["Margem"]=g1.conta
g2.dt["Margem"]=g2.conta
g3.dt["Margem"]=g3.conta
g4.dt["Margem"]=g4.conta

names(g1.dt)[1]="Valor"
names(g2.dt)[1]="Valor"
names(g3.dt)[1]="Valor"
names(g4.dt)[1]="Valor"

Anos = paste(Anos,"0101", sep="")
g1.dt["Anos"]=Anos
g2.dt["Anos"]=Anos
g3.dt["Anos"]=Anos
g4.dt["Anos"]=Anos


g.dt=(rbind(g1.dt,g2.dt,g3.dt,g4.dt))

rownames(g.dt)=c(1:length(row.names(g.dt)))

g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")

g1.dt$Anos = as.Date(g1.dt$Anos,"%Y%m%d")
g2.dt$Anos = as.Date(g2.dt$Anos,"%Y%m%d")
g3.dt$Anos = as.Date(g3.dt$Anos,"%Y%m%d")
g4.dt$Anos = as.Date(g4.dt$Anos,"%Y%m%d")


g.dt["Anos"]=Anos
g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")
g.dt$Contas = as.factor(g.dt$Margem)

g16 = ggplot(data=g.dt, aes(x=Anos ,y = Valor, group = Margem, colour= Margem ))+
  geom_line(col="gray70",size= 0.8, linetype = 3 )+
  labs( x="Ano", y="Margens %") +
  ggtitle("Margem na Lucratividade")+
  scale_y_continuous(labels = function(z) paste0(z*100, "%"), breaks=seq(-0.15,0.3,by=0.10), limits=c(-0.15,0.3))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_label_repel(
    aes(x=Anos ,y = Valor, label = paste0(f.dec(Valor*100,2), "%")),
    box.padding = 0.2, point.padding = 0.2,
    size = 3.5, nudge_y = 0.05,
    segment.color = "grey70") + geom_point(aes(shape = Margem),size =4.0) +
  scale_shape_manual(values=c(15, 16, 17,18)) + scale_size_manual(values=c(4, 4, 6, 4)) + theme_bw()+
  theme(panel.border = element_rect(fill=NA,color="black", size=.5, linetype="solid"),
        plot.title = element_text(color="black", size=18, face="bold.italic"),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        axis.text.x = element_text(face="plain", color="black", size=14, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=14, angle=0))
# guides(group=g.dt, col = guide_legend(nrow=1))

g16= g16 +
  #   scale_colour_grey(start = 0.3, end = 0.4) +
  theme(legend.position = c(0.13, 0.17), legend.key.size = unit(0.5, "cm"),
        legend.background = element_rect(fill="white", size=1, linetype="solid"))


print(g16)
saveplot(g16, 'g16.png')






##########################################################
#           Análise de Rentabilidade
##########################################################
v.PL.aj=m.Passivo.aj.med["Patrimonio Liquido Consolidado - PL",]
rownames(v.PL.aj)=c("Patrimonio Liquido Consolidado - PL")
at.ciclico=t(at.ciclico)


ind.roa=v.lliq/v.AT.aj
ind.roe=v.lliq/v.PL.aj
ind.roi=ind.ebit/at.ciclico

rownames(ind.roa)=c("ROA")
rownames(ind.roe)=c("ROE")
rownames(ind.roi)=c("ROI")


ind.giro.op=v.vendas/at.ciclico
ind.giro.at=v.vendas/v.AT.aj
ind.giro.pl=v.vendas/v.PL.aj

rownames(ind.giro.op)=c("Giro Operacional")
rownames(ind.giro.at)=c("Giro do Ativo")
rownames(ind.giro.pl)=c("Giro do PL")



# Visualizando indicadores
rm(g1.dt)
rm(g2.dt)
rm(g3.dt)

g1.str="ROA"
g2.str= "ROE"
g3.str= "ROI"

Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
g1.dt =as.data.frame(t(ind.roa))
g2.dt =as.data.frame(t(ind.roe))
g3.dt =as.data.frame(t(ind.roi))

g1.conta=c(rep(g1.str,9))
g2.conta=c(rep(g2.str,9))
g3.conta=c(rep(g3.str,9))

g1.dt["Indicadores"]=g1.conta
g2.dt["Indicadores"]=g2.conta
g3.dt["Indicadores"]=g3.conta

names(g1.dt)[1]="Valor"
names(g2.dt)[1]="Valor"
names(g3.dt)[1]="Valor"

Anos = paste(Anos,"0101", sep="")
g1.dt["Anos"]=Anos
g2.dt["Anos"]=Anos
g3.dt["Anos"]=Anos

g.dt=(rbind(g1.dt,g2.dt,g3.dt))

rownames(g.dt)=c(1:length(row.names(g.dt)))

g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")

g1.dt$Anos = as.Date(g1.dt$Anos,"%Y%m%d")
g2.dt$Anos = as.Date(g2.dt$Anos,"%Y%m%d")
g3.dt$Anos = as.Date(g3.dt$Anos,"%Y%m%d")

g.dt["Anos"]=Anos
g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")
g.dt$Contas = as.factor(g.dt$Indicadores)

g17 = ggplot(data=g.dt, aes(x=Anos ,y = Valor, group = Indicadores, colour= Indicadores ))+
  geom_line(col="gray70",size= 0.8, linetype = 3 )+
  labs( x="Ano", y="Indicadores %") +
  ggtitle("Indicadores de Rentabilidade")+
  scale_y_continuous(labels = function(z) paste0(z*100, "%"), breaks=seq(-0.25,0.3,by=0.10), limits=c(-0.25,0.3))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_label_repel(
    aes(x=Anos ,y = Valor, label = paste0(f.dec(Valor*100,2), "%")),
    box.padding = 0.2, point.padding = 0.2,
    size = 3.5, nudge_y = 0.05,
    segment.color = "grey70") + geom_point(aes(shape = Indicadores),size =4.5) +
  scale_shape_manual(values=c(15, 16, 17,18)) + scale_size_manual(values=c(4, 4, 6, 4)) + theme_bw()+
  theme(panel.border = element_rect(fill=NA,color="black", size=.5, linetype="solid"),
        plot.title = element_text(color="black", size=18, face="bold.italic"),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        axis.text.x = element_text(face="plain", color="black", size=14, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=14, angle=0))
# guides(group=g.dt, col = guide_legend(nrow=1))

g17= g17 +
  #   scale_colour_grey(start = 0.3, end = 0.4) +
  theme(legend.position = c(0.15, 0.20),
        legend.background = element_rect(fill="white", size=1, linetype="solid"))


print(g17)
saveplot(g17, 'g17.png')


# Visualizando margem

rm(g1.dt)
rm(g2.dt)
rm(g3.dt)

g1.str="Margem EBIT"
g2.str= "Margem EBITDA"
g3.str= "Margem NOPAT"
g4.str= "Margem Líquida"
Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
g1.dt =as.data.frame(t(v.marg.ebit))
g2.dt =as.data.frame(t(v.marg.ebitda))
g3.dt =as.data.frame(t(v.marg.nopat))
g4.dt =as.data.frame(t(v.marg.liq))

g1.conta=c(rep(g1.str,9))
g2.conta=c(rep(g2.str,9))
g3.conta=c(rep(g3.str,9))
g4.conta=c(rep(g4.str,9))

g1.dt["Margem"]=g1.conta
g2.dt["Margem"]=g2.conta
g3.dt["Margem"]=g3.conta
g4.dt["Margem"]=g4.conta

names(g1.dt)[1]="Valor"
names(g2.dt)[1]="Valor"
names(g3.dt)[1]="Valor"
names(g4.dt)[1]="Valor"

Anos = paste(Anos,"0101", sep="")
g1.dt["Anos"]=Anos
g2.dt["Anos"]=Anos
g3.dt["Anos"]=Anos
g4.dt["Anos"]=Anos


g.dt=(rbind(g1.dt,g2.dt,g3.dt,g4.dt))

rownames(g.dt)=c(1:length(row.names(g.dt)))

g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")

g1.dt$Anos = as.Date(g1.dt$Anos,"%Y%m%d")
g2.dt$Anos = as.Date(g2.dt$Anos,"%Y%m%d")
g3.dt$Anos = as.Date(g3.dt$Anos,"%Y%m%d")
g4.dt$Anos = as.Date(g4.dt$Anos,"%Y%m%d")


g.dt["Anos"]=Anos
g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")
g.dt$Contas = as.factor(g.dt$Margem)

g18 = ggplot(data=g.dt, aes(x=Anos ,y = Valor, group = Margem, colour= Margem ))+
  geom_line(col="gray70",size= 0.8, linetype = 3 )+
  labs( x="Ano", y="Margens %") +
  ggtitle("Margem da Rentabilidade")+
  scale_y_continuous(labels = function(z) paste0(z*100, "%"), breaks=seq(-0.15,0.3,by=0.10), limits=c(-0.15,0.3))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_label_repel(
    aes(x=Anos ,y = Valor, label = paste0(f.dec(Valor*100,2), "%")),
    box.padding = 0.2, point.padding = 0.2,
    size = 3.5, nudge_y = 0.05,
    segment.color = "grey70") + geom_point(aes(shape = Margem),size =4.5) +
  scale_shape_manual(values=c(15, 16, 17,18)) + scale_size_manual(values=c(4, 4, 6, 4)) + theme_bw()+
  theme(panel.border = element_rect(fill=NA,color="black", size=.5, linetype="solid"),
        plot.title = element_text(color="black", size=18, face="bold.italic"),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        axis.text.x = element_text(face="plain", color="black", size=14, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=14, angle=0))
# guides(group=g.dt, col = guide_legend(nrow=1))

g18= g18 +
  #   scale_colour_grey(start = 0.3, end = 0.4) +
  theme(legend.position = c(0.55, 0.80), legend.title = element_text(size = 7),
        legend.text = element_text(size = 7), legend.key.size = unit(0.6, "cm"),
        legend.background = element_rect(fill="white", size=1, linetype="solid"))

print(g18)
saveplot(g18, 'g18.png')

# Visualizando Giros
rm(g1.dt)
rm(g2.dt)
rm(g3.dt)

g1.str="Giro do Ativo"
g2.str= "Giro Operacional"
g3.str= "Giro do PL"

Anos=c("2018","2017","2016","2015","2014","2013","2012","2011","2010")
g1.dt =as.data.frame(t(ind.giro.at))
g2.dt =as.data.frame(t(ind.giro.op))
g3.dt =as.data.frame(t(ind.giro.pl))


g1.conta=c(rep(g1.str,9))
g2.conta=c(rep(g2.str,9))
g3.conta=c(rep(g3.str,9))


g1.dt["Indicadores"]=g1.conta
g2.dt["Indicadores"]=g2.conta
g3.dt["Indicadores"]=g3.conta


names(g1.dt)[1]="Valor"
names(g2.dt)[1]="Valor"
names(g3.dt)[1]="Valor"


Anos = paste(Anos,"0101", sep="")
g1.dt["Anos"]=Anos
g2.dt["Anos"]=Anos
g3.dt["Anos"]=Anos



g.dt=(rbind(g1.dt,g2.dt,g3.dt))

rownames(g.dt)=c(1:length(row.names(g.dt)))

g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")

g1.dt$Anos = as.Date(g1.dt$Anos,"%Y%m%d")
g2.dt$Anos = as.Date(g2.dt$Anos,"%Y%m%d")
g3.dt$Anos = as.Date(g3.dt$Anos,"%Y%m%d")

g.dt["Anos"]=Anos
g.dt$Anos = as.Date(g.dt$Anos,"%Y%m%d")
g.dt$Contas = as.factor(g.dt$Indicadores)

g19 = ggplot(data=g.dt, aes(x=Anos ,y = Valor, group = Indicadores, colour= Indicadores ))+
  geom_line(col="gray70",size= 0.8, linetype = 3 )+
  labs( x="Ano", y="Giro Operacional, do Ativo e do PL") +
  ggtitle("Indicadores de Desempenho Econômico")+
  scale_y_continuous(labels = function(z) paste0(z*1, " "), breaks=seq(0.0,3,by=0.5), limits=c(0.0,3))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  geom_label_repel(
    aes(x=Anos ,y = Valor, label = paste0(f.dec(Valor*1,2), " ")),
    box.padding = 0.2, point.padding = 0.2,
    size = 3.0, nudge_y = 0.05,
    segment.color = "grey70") + geom_point(aes(shape = Indicadores),size =4.0) +
  scale_shape_manual(values=c(15, 16, 17,18)) + scale_size_manual(values=c(4, 4, 6, 4)) + theme_bw()+
  theme(panel.border = element_rect(fill=NA,color="black", size=.5, linetype="solid"),
        plot.title = element_text(color="black", size=18, face="bold.italic"),
        axis.title.x = element_text(color="black", size=16, face="bold"),
        axis.title.y = element_text(color="black", size=16, face="bold"),
        axis.text.x = element_text(face="plain", color="black", size=14, angle=0),
        axis.text.y = element_text(face="plain", color="black", size=14, angle=0))
# guides(group=g.dt, col = guide_legend(nrow=1))

g19= g19 +
  #   scale_colour_grey(start = 0.3, end = 0.4) +
  theme(legend.position = c(0.25, 0.85), legend.title = element_text(size = 7),
        legend.text = element_text(size = 7), legend.key.size = unit(0.6, "cm"),
        legend.background = element_rect(fill="white", size=1, linetype="solid"))

print(g19)
saveplot(g19,'g19.png')
