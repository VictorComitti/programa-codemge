library(jrvFinance)

fluxo_caixa <- read.xlsx("hw_01.xlsx")

# Definindo a taxa de desconto
taxa_desconto <- 0.10

# Calculando o VPL
VPL <- sum(fluxo_caixa$Fluxo.de.Caixa / (1 + taxa_desconto)^(1:(length(fluxo_caixa$Fluxo.de.Caixa))))

VPL

# Alternativamente 

VPL <- npv(rate = taxa_desconto, cf = fluxo_caixa$Fluxo.de.Caixa)
VPL
