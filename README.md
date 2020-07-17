### Dados Coronavírus Espirito Santo 

library(ggplot2)
library(plotrix)
library(RColorBrewer)

url = "https://bi.static.es.gov.br/covid19/MICRODADOS.csv" ### não necessário

dados.covid = read.csv2("https://bi.static.es.gov.br/covid19/MICRODADOS.csv", header = T, stringsAsFactors = F)

dados.covid = dados.covid[grepl("Confirmados", dados.covid$Classificacao),] # ler somente casos confirmados

View(dados.covid)

# Anular colunas 

dados.covid$ViagemBrasil = NULL
dados.covid$ProfissionalSaude = NULL
dados.covid$ViagemInternacional = NULL
dados.covid$FicouInternado = NULL
dados.covid$Escolaridade = NULL
dados.covid$RacaCor = NULL
dados.covid$Evolucao = NULL
dados.covid$StatusNotificacao = NULL
dados.covid$Municipio = NULL
dados.covid$Bairro = NULL
dados.covid$Classificacao = NULL
dados.covid$CriterioConfirmacao = NULL
dados.covid$StatusNotificacao = NULL
dados.covid$FaixaEtaria = NULL
dados.covid$IdadeNaDataNotificacao = NULL
dados.covid$Sexo = NULL

# Converter colunas

dados.covid$DataNotificacao = as.Date(dados.covid$DataNotificacao, "%Y-%m-%d")
dados.covid$DataCadastro = as.Date(dados.covid$DataCadastro, "%Y-%m-%d")
dados.covid$DataDiagnostico = as.Date(dados.covid$DataDiagnostico, "%Y-%m-%d")
dados.covid$DataColeta_RT_PCR = as.Date(dados.covid$DataColeta_RT_PCR, "%Y-%m-%d")
dados.covid$DataColetaTesteRapido = as.Date(dados.covid$DataColetaTesteRapido, "%Y-%m-%d")
dados.covid$DataEncerramento = as.Date(dados.covid$DataEncerramento, "%Y-%m-%d")
dados.covid$DataObito = as.Date(dados.covid$DataObito, "%Y-%m-%d")

# Substituindo variavies qualitativas por quantitativas (opcional)

dados.covid$Febre = as.numeric(gsub("Sim", "1", gsub("Não", "0", dados.covid$Febre)))

dados.covid$DificuldadeRespiratoria = as.numeric(gsub("Sim", "1", gsub("Não", "0", dados.covid$DificuldadeRespiratoria)))

dados.covid$Tosse = as.numeric(gsub("Sim", "1", gsub("Não", "0", dados.covid$Tosse)))

dados.covid$Coriza = as.numeric(gsub("Sim", "1", gsub("Não", "0", dados.covid$Coriza)))

dados.covid$DorGarganta = as.numeric(gsub("Sim", "1", gsub("Não", "0", dados.covid$DorGarganta)))

dados.covid$Diarreia = as.numeric(gsub("Sim", "1", gsub("Não", "0", dados.covid$Diarreia)))

dados.covid$Cefaleia = as.numeric(gsub("Sim", "1", gsub("Não", "0", dados.covid$Cefaleia)))

dados.covid$ComorbidadePulmao = as.numeric(gsub("Sim", "1", gsub("Não", "0", dados.covid$ComorbidadePulmao)))

dados.covid$ComorbidadeCardio = as.numeric(gsub("Sim", "1", gsub("Não", "0", dados.covid$ComorbidadeCardio)))

dados.covid$ComorbidadeRenal = as.numeric(gsub("Sim", "1", gsub("Não", "0", dados.covid$ComorbidadeRenal)))

dados.covid$ComorbidadeDiabetes = as.numeric(gsub("Sim", "1", gsub("Não", "0", dados.covid$ComorbidadeDiabetes)))

dados.covid$ComorbidadeTabagismo = as.numeric(gsub("Sim", "1", gsub("Não", "0", dados.covid$ComorbidadeTabagismo)))

dados.covid$ComorbidadeObesidade = as.numeric(gsub("Sim", "1", gsub("Não", "0", dados.covid$ComorbidadeObesidade)))

### Plotando e fazendo graficos 

?barplot

head(dados.covid)

# Febre

dados.covid$Febre = gsub("-", "N/A", gsub("-", "N/A", dados.covid$Febre))

febre = dados.covid$Febre
sint.febre = as.data.frame(table(febre), stringsAsFactors = F)

Frequencia = sint.febre$Freq
TeveFebre = sint.febre$febre

pctfebre = round(Frequencia / sum(Frequencia) * 100) #porcentagem
TeveFebre = paste(TeveFebre, pctfebre)
TeveFebre = paste(TeveFebre, "%", sep = "")

ggplot(sint.febre, aes(x = TeveFebre, y = Frequencia)) +
  geom_bar(color = "blue", fill = rgb(0.1, 0.3, 0.5, 0.7), stat = "identity", width = 0.5) +
  scale_fill_brewer(palette = 1) +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Frequência") +
  ylim(c(0,80000))+
  ggtitle("Coronavírus-ES: o caso confirmado apresentou febre?")+
  labs(caption = "Dados: Painel COVID-19, Espírito Santo. Elaboração por: @henriqvecast e @renanrotorres. Data: 17/07/2020.")
  

# Tosse

dados.covid$Tosse = gsub("-", "N/A", gsub("-", "N/A", dados.covid$Tosse))

tosse = dados.covid$Tosse
sint.tosse = as.data.frame(table(tosse), stringsAsFactors = F)

FrequenciaTosse = sint.tosse$Freq
TeveTosse = sint.tosse$tosse

pcttosse = round(FrequenciaTosse / sum(FrequenciaTosse) * 100) #porcentagem
TeveTosse = paste(TeveTosse, pcttosse)
TeveTosse = paste(TeveTosse, "%", sep = "")

ggplot(sint.tosse, aes(x = TeveTosse, y = FrequenciaTosse)) +
  geom_bar(color = "blue", fill = rgb(0.1, 0.3, 0.5, 0.7), stat = "identity", width = 0.5) +
  scale_fill_brewer(palette = 1) +
  theme(legend.position="none") +
  xlab("") +
  ylab("Frequência") +
  ylim(c(0,80000))+
  ggtitle("Coronavírus-ES: o caso confirmado apresentou tosse?") +
  labs(caption = "Dados: Painel COVID-19, Espírito Santo. Elaboração por: @henriqvecast e @renanrotorres. Data: 17/07/2020.")


# Dificuldade Respiratória

dados.covid$DificuldadeRespiratoria = gsub("-", "N/A", gsub("-", "N/A", dados.covid$DificuldadeRespiratoria))

dif.resp = dados.covid$DificuldadeRespiratoria
dr = as.data.frame(table(dif.resp), stringsAsFactors = F)

FrequenciaDR = dr$Freq
TeveDR = dr$dif.resp

pctDR = round(FrequenciaDR / sum(FrequenciaDR) * 100) #porcentagem
TeveDR = paste(TeveDR, pctDR)
TeveDR = paste(TeveDR, "%", sep = "")

ggplot(dr, aes(x = TeveDR, y = FrequenciaDR)) +
  geom_bar(color = "blue", fill = rgb(0.1, 0.3, 0.5, 0.7), stat = "identity", width = 0.5) +
  scale_fill_brewer(palette = 1) +
  theme(legend.position="none") +
  xlab("") +
  ylab("Frequência") +
  ylim(c(0,80000))+
  ggtitle("Coronavírus-ES: o caso confirmado apresentou dificuldade respiratória?") +
  labs(caption = "Dados: Painel COVID-19, Espírito Santo. Elaboração por: @henriqvecast e @renanrotorres. Data: 17/07/2020.")


# Coriza

dados.covid$Coriza = gsub("-", "N/A", gsub("-", "N/A", dados.covid$Coriza))

coriza = dados.covid$Coriza
sint.coriza = as.data.frame(table(coriza), stringsAsFactors = F)

FrequenciaCori = sint.coriza$Freq
TeveCori = sint.coriza$coriza

pctCori = round(FrequenciaCori / sum(FrequenciaCori) * 100) #porcentagem
TeveCori = paste(TeveCori, pctCori)
TeveCori = paste(TeveCori, "%", sep = "")

ggplot(sint.coriza, aes(x = TeveCori, y = FrequenciaCori)) +
  geom_bar(color = "blue", fill = rgb(0.1, 0.3, 0.5, 0.7), stat = "identity", width = 0.5) +
  scale_fill_brewer(palette = 1) +
  theme(legend.position = "none") +
  xlab("") +
  ylab("Frequência") +
  ylim(c(0,80000))+
  ggtitle("Coronavírus-ES: o caso confirmado apresentou coriza?") +
  labs(caption = "Dados: Painel COVID-19, Espírito Santo. Elaboração por: @henriqvecast e @renanrotorres. Data: 17/07/2020.")


# Dor de garganta

dados.covid$DorGarganta = gsub("-", "N/A", gsub("-", "N/A", dados.covid$DorGarganta))

DG = dados.covid$DorGarganta
sint.DG = as.data.frame(table(DG), stringsAsFactors = F)

FrequenciaDG = sint.DG$Freq
TeveDG = sint.DG$DG

pctDG = round(FrequenciaDG / sum(FrequenciaDG) * 100) #porcentagem
TeveDG = paste(TeveDG, pctDG)
TeveDG = paste(TeveDG, "%", sep = "")

ggplot(sint.DG, aes(x = TeveDG, y = FrequenciaDG)) +
  geom_bar(color = "blue", fill = rgb(0.1, 0.3, 0.5, 0.7), stat = "identity", width = 0.5) +
  scale_fill_brewer(palette = 1) +
  theme(legend.position="none") +
  xlab("") +
  ylab("Frequência") +
  ylim(c(0,80000))+
  ggtitle("Coronavírus-ES: o caso confirmado apresentou dor de garganta?")+
  labs(caption = "Dados: Painel COVID-19, Espírito Santo. Elaboração por: @henriqvecast e @renanrotorres. Data: 17/07/2020.")


# Cefaleia 
  
  dados.covid$Cefaleia = gsub("-", "N/A", gsub("-", "N/A", dados.covid$Cefaleia))
  
  cefaleia = dados.covid$Cefaleia
  sint.cefaleia = as.data.frame(table(cefaleia), stringsAsFactors = F)
  
  FrequenciaCefal = sint.cefaleia$Freq
  TeveCefal = sint.cefaleia$cefaleia
  
  pctcefal = round(FrequenciaCefal / sum(FrequenciaCefal) * 100) #porcentagem
  TeveCefal = paste(TeveCefal, pctcefal)
  TeveCefal = paste(TeveCefal, "%", sep = "")
  
  ggplot(sint.cefaleia, aes(x = TeveCefal, y = FrequenciaCefal)) +
    geom_bar(color = "blue", fill = rgb(0.1, 0.3, 0.5, 0.7), stat = "identity", width = 0.5) +
    scale_fill_brewer(palette = 1) +
    theme(legend.position="none") +
    xlab("") +
    ylab("Frequência") +
    ylim(c(0,80000))+
    ggtitle("Coronavírus-ES: o caso confirmado apresentou cefaleia?")+
    labs(caption = "Dados: Painel COVID-19, Espírito Santo. Elaboração por: @henriqvecast e @renanrotorres. Data: 17/07/2020.")
  

# Diarreia

dados.covid$Diarreia = gsub("-", "N/A", gsub("-", "N/A", dados.covid$Diarreia))

diarreia = dados.covid$Diarreia
sint.diarreia = as.data.frame(table(diarreia), stringsAsFactors = F)

FrequenciaDiarreia = sint.diarreia$Freq
TeveDiarreia = sint.diarreia$diarreia

pctdiarreia = round(FrequenciaDiarreia / sum(FrequenciaDiarreia) * 100) #porcentagem
TeveDiarreia = paste(TeveDiarreia, pctdiarreia)
TeveDiarreia = paste(TeveDiarreia, "%", sep = "")

ggplot(sint.diarreia, aes(x = TeveDiarreia, y = FrequenciaDiarreia)) +
  geom_bar(color = "blue", fill = rgb(0.1, 0.3, 0.5, 0.7), stat = "identity", width = 0.5) +
  scale_fill_brewer(palette = 1) +
  theme(legend.position="none") +
  xlab("") +
  ylab("Frequência") +
  ylim(c(0,80000))+
  ggtitle("Coronavírus-ES: o caso confirmado apresentou diarreia?")+
  labs(caption = "Dados: Painel COVID-19, Espírito Santo. Elaboração por: @henriqvecast e @renanrotorres. Data: 17/07/2020.")

