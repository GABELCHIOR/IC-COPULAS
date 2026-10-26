# =============================================================================
#  R_usado.R
#
#  Código efetivamente utilizado nas análises do Relatório Final de Iniciação
#  Científica (PIBIC/CNPq) "Modelagem da Função de Distribuição Conjunta de
#  Variáveis Aleatórias por Meio da Teoria de Cópulas".
#
#  Bolsista:    Gabriel de Andrade Belchior — IMECC/Unicamp
#  Orientadora: Profa. Dra. Verónica Andrea González-López — IMECC/Unicamp
#
#  Este arquivo é um recorte de `analise_final.qmd`: mantém apenas o que produz
#  números, tabelas ou figuras que aparecem no relatório, e descarta as figuras
#  exploratórias que ficaram de fora por limite de páginas (séries no tempo,
#  histogramas, correlogramas, correlação móvel, dispersões, simulação da
#  cópula, distribuição da carteira, PIT e comparação das amostras em painéis).
#  Nenhum cálculo foi alterado: as funções e os valores são os mesmos.
#
#  Saídas geradas:
#    tabelas_relatorio/tab01..tab27 (.csv e .tex)
#    figuras_relatorio/fig00, fig12, fig17, fig18, fig20, fig22, fig23, fig26
#
#  Execução:
#    Rscript R_usado.R
#  Tempo aproximado: 10 a 15 minutos, dominados pelo bootstrap do teste de
#  aderência e pelo backtest. Definir a variável de ambiente IC_RAPIDO reduz
#  o número de réplicas do bootstrap de 200 para 30 (uso apenas em edição).
#
#  Ambiente de referência: R 4.4.2, copula 1.1-4, ExtDist 0.7-2,
#  fGarch 4033.92, quantmod 0.4.29.
# =============================================================================


# =============================================================================
# 1. PREPARAÇÃO DO AMBIENTE
# =============================================================================

library(copula)     # ajuste, simulação e diagnóstico de cópulas
library(ExtDist)    # famílias marginais de dois a quatro parâmetros
library(fGarch)     # t de Student e t assimétrica em locação e escala
library(quantmod)   # download das séries no Yahoo Finance

# Há simulação em vários pontos (bootstrap do teste de aderência, VaR de
# carteira, intervalo do tau). A semente fixa garante que os números citados no
# relatório sejam os mesmos a cada execução.
set.seed(2026)


## ---- Padrão gráfico ---------------------------------------------------------

vinho    <- "#5C1010"   # curvas ajustadas, linhas de referência, títulos
vermelho <- "#A31621"   # cor principal das séries e destaques
tijolo   <- "#C7563F"   # segunda série quando há duas no mesmo eixo
rosa     <- "#E3A6A1"   # preenchimento de histogramas e barras
areia    <- "#F6E7E4"   # fundos e faixas de contexto
grafite  <- "#3D3D3D"   # eixos, rótulos e anotações neutras

# Única cor fora da família dos vermelhos. Reservada ao modelo concorrente na
# comparação final: dois tons de vermelho lado a lado ficam indistinguíveis
# quando as duas curvas se cruzam, e é exatamente ali que a figura precisa
# separar um método do outro.
azul     <- "#1F5673"

# Versão translúcida de uma cor. Necessária nas nuvens de pontos, em que mais de
# mil observações se sobrepõem e uma cor sólida esconde a densidade.
transp <- function(cor, alpha = 0.35) {
  canal <- grDevices::col2rgb(cor)
  grDevices::rgb(canal[1], canal[2], canal[3],
                 alpha = alpha * 255, maxColorValue = 255)
}

rampa_vermelha <- grDevices::colorRampPalette(c(rosa, tijolo, vermelho, vinho))

tema_ic <- function() {
  par(mar = c(4.2, 4.4, 2.8, 1.4),  # margens: inferior, esquerda, superior, direita
      mgp = c(2.6, 0.7, 0),         # distância de título do eixo, rótulos e linha
      las = 1,                      # rótulos do eixo y na horizontal
      bty = "n",                    # sem caixa em volta do painel
      cex.main = 1.05, font.main = 2, col.main = vinho,
      col.axis = grafite, col.lab = grafite, fg = grafite)
}


## ---- Exportação de figuras e tabelas ---------------------------------------

# Cada figura sai em PDF vetorial (para o LaTeX do relatório) e em PNG (para
# conferência); cada tabela sai em CSV e em um fragmento `tabular` pronto para
# ser incluído com \input no relatório.

dir_fig <- "figuras_relatorio"
dir_tab <- "tabelas_relatorio"
for (d in c(dir_fig, dir_tab)) if (!dir.exists(d)) dir.create(d)

salvar_figura <- function(nome, desenho, largura = 6.5, altura = 4.4) {
  caminho <- file.path(dir_fig, nome)

  # cairo_pdf em vez de pdf(): trata acentos e transparência corretamente
  cairo_pdf(paste0(caminho, ".pdf"), width = largura, height = altura)
  tema_ic(); desenho(); dev.off()

  png(paste0(caminho, ".png"), width = largura, height = altura,
      units = "in", res = 200, type = "cairo")
  tema_ic(); desenho(); dev.off()

  invisible(caminho)
}

salvar_tabela <- function(nome, tab, digitos = 3) {
  tab <- as.data.frame(tab)
  numericas <- vapply(tab, is.numeric, logical(1))
  tab[numericas] <- lapply(tab[numericas], round, digitos)

  write.csv(tab, file.path(dir_tab, paste0(nome, ".csv")), fileEncoding = "UTF-8")

  # Caracteres de controle do LaTeX. O % é o mais perigoso: sem escape ele
  # comenta o resto da linha e a tabela sai truncada, sem erro de compilação.
  escapar <- function(s) {
    s <- gsub("\\\\", "\\\\textbackslash{}", s)
    s <- gsub("([&%$#_{}])", "\\\\\\1", s)
    s
  }
  celulas <- cbind(escapar(rownames(tab)),
                   escapar(sapply(tab, format, trim = TRUE)))
  writeLines(
    c(paste0("\\begin{tabular}{l", strrep("r", ncol(tab)), "}"),
      "\\hline",
      paste0(paste(c("", escapar(colnames(tab))), collapse = " & "), " \\\\"),
      "\\hline",
      paste0(apply(celulas, 1, paste, collapse = " & "), " \\\\"),
      "\\hline",
      "\\end{tabular}"),
    file.path(dir_tab, paste0(nome, ".tex")), useBytes = TRUE)

  tab
}


## ---- Diagrama do procedimento (Figura 1 do relatório) ----------------------
# Não depende dos dados: é o mapa das etapas e das ferramentas usadas em cada
# uma, colocado no início do relatório para orientar a leitura.

diagrama_procedimento <- function() {
  par(mar = c(0.2, 0.2, 0.2, 0.2))
  plot(NA, xlim = c(0, 100), ylim = c(-1, 103), axes = FALSE,
       xlab = "", ylab = "", xaxs = "i", yaxs = "i")

  etapas <- list(
    c("",  "Dados",
      "1205 log-retornos diários pareados,\nIbovespa e USD/BRL, 2020–2025"),
    c("1", "Distribuições marginais",
      "Ajustar F ao Ibovespa e G ao dólar,\ncada uma separadamente"),
    c("2", "Cópula",
      "Ajustar C, que descreve apenas\na dependência entre os dois"),
    c("3", "Distribuição conjunta",
      "Recompor H(x,y) = C(F(x), G(y))\npelo Teorema de Sklar"),
    c("4", "Value at Risk",
      "Perda de 1%: do Ibovespa isolado,\ncondicionada ao dólar, e da carteira"),
    c("5", "Validação fora da amostra",
      "Reestimar tudo em 80% dos dias\ne avaliar nos 20% finais")
  )

  ferramentas <- c(
    "fechamento ajustado, pareado por data",
    "seis famílias candidatas\nmáxima verossimilhança\nseleção por AIC e BIC",
    paste("dez cópulas candidatas\nmáxima pseudo-verossimilhança\nseleção por AIC",
          "teste de Vuong: a vantagem é real?\nteste Sn: a vencedora é adequada?",
          sep = "\n"),
    "verificação por retângulos:\nprobabilidade do modelo contra\nfrequência observada",
    "quantil da marginal\nderivada da cópula\nsimulação de 100 mil pares",
    paste("Kupiec: frequência de violações",
          "Rosenblatt: especificação condicional",
          "comparação com regressão linear", sep = "\n")
  )

  xl <- 3; xr <- 47; xf <- 52
  alt <- 11; passo <- 17

  for (i in seq_along(etapas)) {
    yt <- 98 - (i - 1) * passo
    yb <- yt - alt
    ym <- (yt + yb) / 2

    rect(xl, yb, xr, yt, col = areia, border = vinho, lwd = 1.5)

    # Disco numerado no canto superior esquerdo, exceto na caixa de dados.
    if (nzchar(etapas[[i]][1])) {
      points(xl, yt, pch = 21, bg = vinho, col = "white", cex = 2.3)
      text(xl, yt, etapas[[i]][1], col = "white", cex = 0.70, font = 2)
    }

    text(xl + 2.4, yt - 2.8, etapas[[i]][2], adj = 0, font = 2,
         cex = 0.84, col = vinho)
    text(xl + 2.4, yb + 3.4, etapas[[i]][3], adj = c(0, 0.5),
         cex = 0.70, col = grafite)

    segments(xr + 0.8, ym, xf - 1.0, ym, lty = 3, col = tijolo, lwd = 1.2)
    text(xf, ym, ferramentas[i], adj = c(0, 0.5), cex = 0.66, col = grafite)

    if (i < length(etapas)) {
      arrows((xl + xr) / 2, yb - 0.5, (xl + xr) / 2, yb - passo + alt + 0.5,
             length = 0.06, lwd = 2, col = vermelho)
    }
  }

  text(xf, 101, "Ferramentas empregadas", adj = c(0, 0.5), font = 2,
       cex = 0.74, col = vermelho)
}

# tema_ic() não se aplica aqui: o diagrama define suas próprias margens.
salvar_figura("fig00_procedimento", diagrama_procedimento,
              largura = 6.5, altura = 5.2)


# =============================================================================
# 2. DADOS
# =============================================================================
# Seção 2.2 do relatório. Ativos: Ibovespa (^BVSP) e dólar comercial (BRL=X),
# fechamento ajustado, Yahoo Finance.

data_inicio <- "2020-01-01"
data_fim    <- "2025-06-30"

# O download é gravado em disco na primeira execução; nas seguintes o arquivo
# local é reaproveitado. Isso torna a análise reproduzível sem conexão e impede
# que uma revisão de série pelo provedor altere números já discutidos. O que se
# guarda em cache são as séries BRUTAS, não o resultado do pareamento, de modo
# que o funil de observações pode ser reconstruído a qualquer momento.
arquivo_cache <- file.path("dados_ic", "precos_yahoo_final.rds")

if (file.exists(arquivo_cache)) {
  bruto <- readRDS(arquivo_cache)
} else {
  if (!dir.exists("dados_ic")) dir.create("dados_ic")
  bruto <- list(
    IBOV   = getSymbols("^BVSP", src = "yahoo", from = data_inicio,
                        to = data_fim, auto.assign = FALSE),
    USDBRL = getSymbols("BRL=X", src = "yahoo", from = data_inicio,
                        to = data_fim, auto.assign = FALSE)
  )
  saveRDS(bruto, arquivo_cache)
}

# Critério de seleção, nesta ordem: fechamento ajustado; data com negociação nos
# dois mercados (inner join); ausência de dado faltante, por descarte e não por
# interpolação; e existência do pregão anterior pareado, já que o log-retorno
# consome uma observação.
precos_ibov  <- Ad(bruto$IBOV)
precos_dolar <- Ad(bruto$USDBRL)

pareados <- merge(precos_ibov, precos_dolar, join = "inner")
colnames(pareados) <- c("IBOV", "USDBRL")
precos <- na.omit(pareados)

funil <- data.frame(
  observações = c(nrow(precos_ibov), nrow(precos_dolar), nrow(pareados),
                  nrow(precos), nrow(precos) - 1),
  row.names = c(
    "Pregões brutos do Ibovespa",
    "Pregões brutos do USD/BRL",
    "Datas com negociação simultânea (inner join)",
    "Após descarte de fechamentos ausentes",
    "Log-retornos bivariados disponíveis"
  )
)
salvar_tabela("tab01_observacoes", funil, digitos = 0)     # Tabela 1


## ---- Log-retornos e as duas amostras ---------------------------------------

retornos_todos <- na.omit(diff(log(precos)))

# O corte descarta o ano de 2020 inteiro. Não é uma data de conveniência: em
# março de 2020 o Ibovespa acumulou quedas diárias acima de 15%, magnitude que
# não se repete em nenhum outro ponto da série, e o segundo semestre ainda
# carrega a recuperação, que é o mesmo evento com o sinal trocado.
data_corte <- "2021-01-01"

amostras <- list(
  completa  = retornos_todos,
  sem_2020  = retornos_todos[paste0(data_corte, "/")]
)

rotulos <- c(completa = "2020–2025 (completa)",
             sem_2020 = "2021–2025 (sem 2020)")

# Estrutura de trabalho: cada amostra vira uma lista com os dois vetores, as
# datas e o tamanho. Todas as funções deste arquivo consomem esse formato.
preparar <- function(ret) list(
  x = as.numeric(ret$IBOV), y = as.numeric(ret$USDBRL),
  datas = index(ret), n = nrow(ret)
)

am <- lapply(amostras, preparar)

# Os objetos sem qualificação referem-se sempre à amostra completa.
x <- am$completa$x; y <- am$completa$y
datas <- am$completa$datas; n <- am$completa$n


# =============================================================================
# 3. ANÁLISE EXPLORATÓRIA
# =============================================================================
# Seção 3.1 do relatório.

assimetria <- function(z) mean((z - mean(z))^3) / sd(z)^3
curtose_ex <- function(z) mean((z - mean(z))^4) / sd(z)^4 - 3

descritivas <- function(a) {
  linha <- function(z) c(n = length(z), média = mean(z), `desvio-padrão` = sd(z),
                         assimetria = assimetria(z), `curtose em excesso` = curtose_ex(z),
                         mínimo = min(z), máximo = max(z))
  rbind(Ibovespa = linha(a$x), `USD/BRL` = linha(a$y))
}

salvar_tabela("tab02_descritivas_completa", descritivas(am$completa), digitos = 4)  # Tabela 2
salvar_tabela("tab03_descritivas_sem2020",  descritivas(am$sem_2020), digitos = 4)


## ---- Dependência serial: Ljung-Box -----------------------------------------
# Toda a inferência deste trabalho supõe observações independentes ao longo do
# tempo. Convém medir o quanto essa hipótese é violada antes de usá-la.

ljung <- function(a, defasagem = 10) {
  banda <- 1.96 / sqrt(a$n)          # banda de significância usual do correlograma
  linha <- function(z) {
    b <- Box.test(z, lag = defasagem, type = "Ljung-Box")
    r <- acf(z, lag.max = 30, plot = FALSE)$acf[-1]   # [-1] descarta a defasagem 0
    c(`Q(10)` = unname(b$statistic), `p-valor` = b$p.value,
      `1ª autocorrelação` = r[1], `fora da banda (de 30)` = sum(abs(r) > banda))
  }
  rbind(`Ibovespa — retornos`  = linha(a$x),
        `USD/BRL — retornos`   = linha(a$y),
        `Ibovespa — quadrados` = linha(a$x^2),
        `USD/BRL — quadrados`  = linha(a$y^2))
}

tab_ljung <- ljung(am$completa)

# O p-valor vai para a tabela como texto: arredondado a quatro casas, 4e-15
# apareceria como 0,0000 e o leitor não saberia distinguir "muito pequeno" de
# "exatamente zero", que é um valor que um p-valor não assume. Nos quadrados a
# estatística é grande o bastante para o p-valor sofrer underflow.
tab_ljung_fmt <- data.frame(
  `Q(10)` = round(tab_ljung[, 1], 1),
  `p-valor` = ifelse(tab_ljung[, 2] < .Machine$double.eps,
                     "< 2e-16", sprintf("%.3g", tab_ljung[, 2])),
  `1ª autocorrelação` = round(tab_ljung[, 3], 3),
  `fora da banda (de 30)` = tab_ljung[, 4],
  row.names = rownames(tab_ljung), check.names = FALSE
)
salvar_tabela("tab04_ljung_box", tab_ljung_fmt, digitos = 3)   # Tabela 3


## ---- Dependência contemporânea ---------------------------------------------

dependencia <- function(a) {
  m <- function(met) cor(a$x, a$y, method = met)
  # Intervalo bootstrap para o tau: os pares são reamostrados inteiros, de modo
  # que a incerteza reportada é a amostral. Ela ignora a dependência serial dos
  # retornos e é, portanto, um limite inferior para a incerteza verdadeira.
  b <- replicate(1000, {
    i <- sample.int(a$n, a$n, replace = TRUE)
    cor(a$x[i], a$y[i], method = "kendall")
  })
  c(`Pearson r` = m("pearson"), `Spearman rho` = m("spearman"),
    `Kendall tau` = m("kendall"),
    `tau — limite inf. 95%` = unname(quantile(b, 0.025)),
    `tau — limite sup. 95%` = unname(quantile(b, 0.975)))
}

tab_dep <- rbind(completa = dependencia(am$completa),
                 sem_2020 = dependencia(am$sem_2020))
rownames(tab_dep) <- rotulos[rownames(tab_dep)]
salvar_tabela("tab05_dependencia", tab_dep, digitos = 4)       # Tabela 4

tau_emp <- cor(x, y, method = "kendall")


## ---- Dispersão em postos (Figura 2 do relatório) ---------------------------
# Cada retorno substituído pelo seu posto normalizado. Por construção as
# marginais aqui são uniformes; o que resta na figura é só a dependência.

salvar_figura("fig12_dispersao_postos", function() {
  P <- pobs(cbind(x, y))
  plot(P, pch = 16, cex = 0.5, col = transp(tijolo, 0.4),
       xlim = c(0, 1), ylim = c(0, 1),
       xlab = "posto normalizado do Ibovespa",
       ylab = "posto normalizado do USD/BRL",
       main = "Dependência isolada das marginais")
  abline(h = c(0.05, 0.95), v = c(0.05, 0.95), lty = 3, col = grafite)
})


# =============================================================================
# 4. DISTRIBUIÇÕES MARGINAIS
# =============================================================================
# Seção 2.3 e 3.2 do relatório. Seis candidatas, em progressão de flexibilidade:
# normal, logística, Laplace, Johnson SU, t de Student e t assimétrica.

# O clamp mantém a imagem da FDA estritamente dentro de (0,1). Nas caudas a
# avaliação numérica satura em 0 ou 1 e, se isso chegar ao ajuste da cópula, a
# log-pseudo-verossimilhança vai a -Inf e a estimação falha.
apara <- function(p, e = 1e-8) pmin(pmax(p, e), 1 - e)

# Famílias do ExtDist: a estimação é feita por e<Nome>() e as funções d/p/q
# recebem o objeto ajustado pelo argumento `params`.
via_extdist <- function(nome) function(z) {
  est <- get(paste0("e", nome))(z)
  list(par = unlist(est),
       d = function(w) get(paste0("d", nome))(w, params = est),
       p = function(w) apara(get(paste0("p", nome))(w, params = est)),
       q = function(u) get(paste0("q", nome))(u, params = est))
}

# Para as duas famílias do fGarch a estimação NÃO usa os ajustadores do pacote:
# stdFit() aplicado aos retornos do Ibovespa para no limite de iterações sem
# convergir, devolvendo exatamente o valor inicial de busca de nu, o que
# penalizaria a t numa comparação de AIC decidida por poucas unidades.
#
# Desvio, graus de liberdade e assimetria são reparametrizados no log para que o
# otimizador trabalhe sem restrições: exp(.) garante positividade e 2 + exp(.)
# mantém nu > 2, condição para a variância existir. O retorno de 1e10 em caso de
# densidade não finita evita que o BFGS siga um passo inválido.
ml_direto <- function(z, densidade, inicio) {
  nll <- function(th) {
    v <- densidade(z, th)
    if (any(!is.finite(v)) || any(v <= 0)) return(1e10)
    -sum(log(v))
  }
  o <- optim(inicio, nll, method = "BFGS",
             control = list(maxit = 2000, reltol = 1e-12))
  if (o$convergence != 0) warning("otimização não convergiu: ", o$convergence)
  o$par
}

via_t <- function(z) {
  th <- ml_direto(z, function(w, t) dstd(w, t[1], exp(t[2]), 2 + exp(t[3])),
                  c(mean(z), log(sd(z)), log(2)))
  # pa[[i]] em vez de pa[i]: o colchete simples carregaria o nome do parâmetro
  # para o resultado, e esse nome reapareceria nos rótulos das tabelas.
  pa <- c(mean = th[1], sd = exp(th[2]), nu = 2 + exp(th[3]))
  list(par = pa,
       d = function(w) dstd(w, pa[[1]], pa[[2]], pa[[3]]),
       p = function(w) apara(pstd(w, pa[[1]], pa[[2]], pa[[3]])),
       q = function(u) qstd(u, pa[[1]], pa[[2]], pa[[3]]))
}

via_t_assim <- function(z) {
  th <- ml_direto(z, function(w, t)
                    dsstd(w, t[1], exp(t[2]), 2 + exp(t[3]), exp(t[4])),
                  c(mean(z), log(sd(z)), log(2), 0))
  pa <- c(mean = th[1], sd = exp(th[2]), nu = 2 + exp(th[3]), xi = exp(th[4]))
  list(par = pa,
       d = function(w) dsstd(w, pa[[1]], pa[[2]], pa[[3]], pa[[4]]),
       p = function(w) apara(psstd(w, pa[[1]], pa[[2]], pa[[3]], pa[[4]])),
       q = function(u) qsstd(u, pa[[1]], pa[[2]], pa[[3]], pa[[4]]))
}

candidatas <- list(
  Normal          = via_extdist("Normal"),
  Logística       = via_extdist("Logistic"),
  Laplace         = via_extdist("Laplace"),
  `Johnson SU`    = via_extdist("JohnsonSU"),
  `t de Student`  = via_t,
  `t assimétrica` = via_t_assim
)

# A log-verossimilhança é recalculada de modo uniforme, como sum(log(f(z))), em
# vez de extraída de cada pacote: isso elimina dúvida sobre convenções internas
# — constantes omitidas, sinais invertidos — e garante que os seis números sejam
# comparáveis entre si.
avaliar_marginais <- function(z) {
  ajustes <- lapply(candidatas, function(f) f(z))
  ll <- vapply(ajustes, function(a) sum(log(a$d(z))), numeric(1))
  k  <- vapply(ajustes, function(a) length(a$par), integer(1))

  tabela <- data.frame(k = k, logLik = ll,
                       AIC = -2 * ll + 2 * k,
                       BIC = -2 * ll + k * log(length(z)))
  list(ajustes = ajustes, tabela = tabela[order(tabela$AIC), ])
}

# A família adotada é a de menor AIC. Ela pode diferir entre as séries e entre
# as amostras; a tabela completa é sempre reportada para que a diferença fique
# visível, e o `nu` da t é guardado à parte como medida de cauda comparável.
escolher_marginal <- function(z) {
  a <- avaliar_marginais(z)
  nome <- rownames(a$tabela)[1]
  list(nome = nome, ajuste = a$ajustes[[nome]], tabela = a$tabela,
       nu_t = a$ajustes[["t de Student"]]$par[["nu"]])
}

marg <- lapply(am, function(a) list(x = escolher_marginal(a$x),
                                    y = escolher_marginal(a$y)))

salvar_tabela("tab06_criterios_marginal_ibov",  marg$completa$x$tabela, digitos = 1)  # Tabela 5
salvar_tabela("tab07_criterios_marginal_dolar", marg$completa$y$tabela, digitos = 1)

familias_adotadas <- data.frame(
  `família adotada` = c(marg$completa$x$nome, marg$completa$y$nome,
                        marg$sem_2020$x$nome, marg$sem_2020$y$nome),
  `nu da t`         = c(marg$completa$x$nu_t, marg$completa$y$nu_t,
                        marg$sem_2020$x$nu_t, marg$sem_2020$y$nu_t),
  row.names = c("Ibovespa — completa", "USD/BRL — completa",
                "Ibovespa — sem 2020", "USD/BRL — sem 2020"),
  check.names = FALSE
)
salvar_tabela("tab08_familias_marginais", familias_adotadas, digitos = 2)   # Tabela 6


## ---- Verificação do ajuste (Figura 2 do relatório) -------------------------

# Nomes curtos para o resto do arquivo. Fx e Gy são as FDA estimadas, fx e gy
# as densidades, Qx e Qy as quasi-inversas (Definição 2 do projeto), necessárias
# para converter quantis da cópula de volta à escala de retornos.
Fx <- marg$completa$x$ajuste$p; fx <- marg$completa$x$ajuste$d
Qx <- marg$completa$x$ajuste$q
Gy <- marg$completa$y$ajuste$p; gy <- marg$completa$y$ajuste$d
Qy <- marg$completa$y$ajuste$q

nome_marg_x <- marg$completa$x$nome
nome_marg_y <- marg$completa$y$nome

qq_ajustado <- function(z, Q, titulo, cor) {
  # Quantis teóricos nas posições de plotagem (i - 1/2)/n, convenção que evita
  # as extremidades exatas 0 e 1, onde a quasi-inversa diverge.
  u <- ppoints(length(z))
  plot(100 * Q(u), 100 * sort(z), pch = 16, cex = 0.5, col = transp(cor, 0.5),
       xlab = "quantis teóricos (%)", ylab = "quantis observados (%)", main = titulo)
  abline(0, 1, col = vinho, lwd = 2)
}

salvar_figura("fig17_qq_marginal_ibov", function()
  qq_ajustado(x, Qx, paste("QQ-plot —", nome_marg_x, "— Ibovespa"), vermelho))

salvar_figura("fig18_qq_marginal_dolar", function()
  qq_ajustado(y, Qy, paste("QQ-plot —", nome_marg_y, "— USD/BRL"), tijolo))


# =============================================================================
# 5. PSEUDO-OBSERVAÇÕES
# =============================================================================
# Seção 2.5 do relatório. Duas construções, com papéis distintos: a paramétrica
# torna a conjunta coerente e permite avaliar quantis em escala de retornos; a
# por postos isola a seleção da família de qualquer erro de especificação
# marginal, e é a via canônica para SELECIONAR a cópula.

uv     <- cbind(Fx(x), Gy(y))    # via paramétrica
postos <- pobs(cbind(x, y))      # via postos

u <- uv[, 1]; v <- uv[, 2]

uniformidade <- function(a, b) rbind(
  `u = F(x)` = c(`média` = mean(a), `KS p-valor` = ks.test(a, "punif")$p.value),
  `v = G(y)` = c(mean(b), ks.test(b, "punif")$p.value)
)
salvar_tabela("tab09_uniformidade", uniformidade(u, v), digitos = 4)


# =============================================================================
# 6. AJUSTE DA CÓPULA
# =============================================================================
# Seções 2.4, 2.5 e 3.3 do relatório. Dez famílias em três grupos: elípticas
# (normal e t), arquimedianas diretas (Frank, AMH) e arquimedianas rotacionadas
# (Clayton, Gumbel e Joe, nas duas orientações antidiagonais).
#
# Cada família é rotulada pelo CANTO QUE EFETIVAMENTE OCUPA, e não pelo
# argumento `flip`, porque a correspondência entre os dois não é a que a
# intuição sugere e o erro seria silencioso: o ajuste roda, o AIC sai, e só a
# interpretação econômica fica invertida. Refletir o eixo v leva o canto (1,1)
# da família de origem para (1,0); refletir u leva-o para (0,1). Logo a Gumbel,
# que tem dependência na cauda superior, vai para o canto inferior direito com
# flip = c(FALSE, TRUE) e para o superior esquerdo com flip = c(TRUE, FALSE) —
# o oposto da Clayton.
#
# Convenção: com u no eixo horizontal e v no vertical, o canto superior esquerdo
# (0,1) é o de CRISE (Ibovespa em queda extrema com dólar em alta extrema) e o
# inferior direito (1,0) é o BENIGNO.

familias_copula <- list(
  `Normal`          = normalCopula(dim = 2),
  `t`               = tCopula(dim = 2, df = 4, df.fixed = FALSE),
  `Frank`           = frankCopula(dim = 2),
  `AMH direta`      = amhCopula(dim = 2),
  `Clayton sup-esq` = rotCopula(claytonCopula(dim = 2), flip = c(FALSE, TRUE)),
  `Clayton inf-dir` = rotCopula(claytonCopula(dim = 2), flip = c(TRUE, FALSE)),
  `Gumbel sup-esq`  = rotCopula(gumbelCopula(dim = 2),  flip = c(TRUE, FALSE)),
  `Gumbel inf-dir`  = rotCopula(gumbelCopula(dim = 2),  flip = c(FALSE, TRUE)),
  `Joe sup-esq`     = rotCopula(joeCopula(dim = 2),     flip = c(TRUE, FALSE)),
  `Joe inf-dir`     = rotCopula(joeCopula(dim = 2),     flip = c(FALSE, TRUE))
)

# O limite inferior da AMH é (5 - 8 ln 2)/3, valor fechado e surpreendentemente
# próximo de zero: a família quase não acomoda dependência negativa. O valor
# citado no relatório vem daqui.
limite_amh <- c(`tau mínimo da AMH` = tau(amhCopula(-1)), `tau empírico` = tau_emp)
print(limite_amh)


## ---- Dependência de cauda por canto ----------------------------------------

# Os graus de liberdade da t não ficam num slot próprio: o vetor `parameters`
# de uma t bivariada é sempre c(rho, df), esteja o df fixo ou livre. Ler pelo
# índice é o único acesso que funciona nos dois casos.
gl_t <- function(cp) unname(cp@parameters[2])

lambda_cantos <- function(cp) {
  saida <- c(`inf-esq` = 0, `sup-dir` = 0, `sup-esq` = 0, `inf-dir` = 0)

  # Elípticas. A normal é assintoticamente independente nos quatro cantos. Para
  # a t, refletir uma coordenada equivale a trocar o sinal de rho, e leva os
  # cantos diagonais nos antidiagonais: o lambda antidiagonal é, portanto, o
  # lambda da mesma t com -rho. A t é radialmente simétrica, de modo que os dois
  # cantos de cada par têm o mesmo valor.
  if (is(cp, "tCopula")) {
    rho <- cp@parameters[1]; nu <- gl_t(cp)
    diag <- lambda(tCopula(rho,  df = nu))[["lower"]]
    anti <- lambda(tCopula(-rho, df = nu))[["lower"]]
    saida[] <- c(diag, diag, anti, anti)
    return(saida)
  }
  if (is(cp, "normalCopula")) return(saida)

  base <- if (is(cp, "rotCopula")) cp@copula else cp
  flip <- if (is(cp, "rotCopula")) cp@flip  else c(FALSE, FALSE)
  lam  <- tryCatch(lambda(base), error = function(e) c(lower = 0, upper = 0))

  # Onde cada canto da família de origem vai parar depois da reflexão.
  destino <- function(pt) {
    q <- c(if (flip[1]) 1 - pt[1] else pt[1], if (flip[2]) 1 - pt[2] else pt[2])
    if      (all(q == c(0, 0))) "inf-esq"
    else if (all(q == c(1, 1))) "sup-dir"
    else if (all(q == c(0, 1))) "sup-esq"
    else                        "inf-dir"
  }
  saida[destino(c(0, 0))] <- saida[destino(c(0, 0))] + unname(lam[["lower"]])
  saida[destino(c(1, 1))] <- saida[destino(c(1, 1))] + unname(lam[["upper"]])
  saida
}


## ---- Estimação e comparação ------------------------------------------------
# Máxima pseudo-verossimilhança (método "mpl"), apropriada quando as marginais
# já foram tratadas separadamente.

# O tryCatch protege o laço: a AMH, em particular, costuma esbarrar na fronteira
# do seu parâmetro, e uma falha numérica isolada não deve interromper a comparação.
ajustar_copulas <- function(U) {
  ajs <- lapply(familias_copula, function(cp)
    tryCatch(fitCopula(cp, data = U, method = "mpl"), error = function(e) NULL))
  Filter(Negate(is.null), ajs)
}

tabela_copulas <- function(ajs, N) {
  lam <- t(sapply(ajs, function(a) lambda_cantos(a@copula)))
  ll  <- sapply(ajs, function(a) as.numeric(logLik(a)))
  k   <- sapply(ajs, function(a) length(coef(a)))

  tab <- data.frame(
    parâmetros = sapply(ajs, function(a) paste(sprintf("%.3f", coef(a)), collapse = ", ")),
    k = k, logLik = ll, AIC = -2 * ll + 2 * k, BIC = -2 * ll + k * log(N),
    `lambda sup-esq` = lam[, "sup-esq"], `lambda inf-dir` = lam[, "inf-dir"],
    row.names = names(ajs), check.names = FALSE
  )
  tab[order(tab$AIC), ]
}

aj_postos <- ajustar_copulas(postos)
aj_param  <- ajustar_copulas(uv)

tab_cop_postos <- tabela_copulas(aj_postos, n)
tab_cop_param  <- tabela_copulas(aj_param,  n)

salvar_tabela("tab10_copulas_postos",      tab_cop_postos, digitos = 2)  # Tabela 7
salvar_tabela("tab11_copulas_parametrica", tab_cop_param,  digitos = 2)

# Gráfico de AIC (Figura 3 do relatório). O critério é negativo em toda a
# tabela, e comparar números negativos grandes em um gráfico é pouco
# informativo: as barras mostram a redução em relação à pior candidata.
grafico_aic <- function(tabela, titulo) {
  par(mar = c(4.4, 8.4, 2.8, 3.6))     # margem esquerda maior: nomes por extenso

  aic   <- setNames(tabela$AIC, rownames(tabela))
  aic   <- aic[order(aic, decreasing = TRUE)]   # pior embaixo, melhor no topo
  ganho <- max(aic) - aic
  cores <- ifelse(aic == min(aic), vinho, rosa)

  bp <- barplot(ganho, horiz = TRUE, col = cores, border = "white", las = 1,
                xlim = c(0, max(ganho) * 1.28), cex.names = 0.9,
                xlab = "redução de AIC em relação à pior candidata", main = titulo)
  text(ganho, bp, labels = sprintf("%.0f", aic), pos = 4, cex = 0.78,
       col = grafite, xpd = NA)
}

salvar_figura("fig20_aic_copulas", function()
  grafico_aic(tab_cop_postos, "Comparação entre famílias de cópulas (postos)"),
  altura = 4.8)

nome_copula  <- rownames(tab_cop_postos)[1]
segunda      <- rownames(tab_cop_postos)[2]
vantagem_aic <- tab_cop_postos$AIC[2] - tab_cop_postos$AIC[1]

print(c(vencedora_postos = nome_copula,
        vencedora_paramétrica = rownames(tab_cop_param)[1],
        vantagem_AIC = round(vantagem_aic, 1)))


## ---- Teste de Vuong entre as candidatas próximas ---------------------------
# Diferenças de AIC não vêm com medida de incerteza. Para famílias NÃO
# encaixadas com o mesmo número de parâmetros, o teste apropriado é o de Vuong:
# sob a hipótese de ajuste equivalente, a média das diferenças de log-densidade
# ponto a ponto, padronizada, é assintoticamente normal padrão.

vuong <- function(a1, a2, U) {
  d <- dCopula(U, a1@copula, log = TRUE) - dCopula(U, a2@copula, log = TRUE)
  z <- sqrt(nrow(U)) * mean(d) / sd(d)
  c(`dif. logLik` = sum(d), estatística = z, `p-valor` = 2 * pnorm(-abs(z)))
}

# Dois confrontos interessam. O primeiro é entre as duas orientações da mesma
# família assimétrica: se ele não rejeita, o canto nunca foi estimado a partir
# dos dados — foi fixado pelo `flip` escrito no código. O segundo é entre a
# vencedora e as arquimedianas melhor colocadas.
pares <- list(
  c("Gumbel sup-esq", "Gumbel inf-dir"),
  c("Clayton sup-esq", "Clayton inf-dir"),
  c(nome_copula, "Gumbel sup-esq"),
  c(nome_copula, "Gumbel inf-dir"),
  c(nome_copula, "Normal")
)
pares <- Filter(function(p) all(p %in% names(aj_postos)) && p[1] != p[2], pares)

tab_vuong <- t(sapply(pares, function(p)
  vuong(aj_postos[[p[1]]], aj_postos[[p[2]]], postos)))
rownames(tab_vuong) <- sapply(pares, function(p) paste(p, collapse = " vs "))
salvar_tabela("tab12_vuong", tab_vuong, digitos = 4)          # Tabela 8


## ---- A cópula selecionada --------------------------------------------------

ajuste_cop <- aj_param[[nome_copula]]
cop_final  <- ajuste_cop@copula

# A cópula t com graus de liberdade não inteiros não tem pCopula() implementado,
# e pCopula() é necessária em três pontos adiante: na conjunta H, na derivada
# que dá a distribuição condicional e no teste de aderência. Por isso o df é
# estimado livre e, em seguida, o modelo é reajustado com o inteiro mais
# próximo fixado.
df_livre <- NA_real_
if (is(cop_final, "tCopula") && !isTRUE(cop_final@df.fixed)) {
  df_livre   <- gl_t(cop_final)
  ajuste_cop <- fitCopula(tCopula(dim = 2, df = round(df_livre), df.fixed = TRUE),
                          data = uv, method = "mpl")
  cop_final  <- ajuste_cop@copula
}

lam_final <- lambda_cantos(cop_final)

resumo_cop <- c(
  `parâmetro estimado`        = unname(coef(ajuste_cop)[1]),
  `graus de liberdade`        = if (is.na(df_livre)) NA_real_ else round(df_livre),
  `df antes de arredondar`    = df_livre,
  `tau implicado pelo modelo` = tau(cop_final),
  `tau empírico`              = tau_emp,
  `lambda no canto sup-esq`   = unname(lam_final["sup-esq"]),
  `lambda no canto inf-dir`   = unname(lam_final["inf-dir"]),
  `lambda nos cantos diagonais` = unname(lam_final["inf-esq"])
)
salvar_tabela("tab13_copula_selecionada", as.data.frame(t(resumo_cop)), digitos = 4)


## ---- Ocupação dos cantos: modelo contra dados ------------------------------
# Conferência direta: quantos dias, de fato, caíram em cada canto de 5%, e
# quantos o modelo prevê. Leia-se como descrição, não como teste: as caixas
# contêm poucas dezenas de observações e a pseudo-verossimilhança é dominada
# pelo corpo da distribuição.

canto_se <- function(U, q = 0.05) mean(U[, 1] <= q & U[, 2] >= 1 - q)
canto_id <- function(U, q = 0.05) mean(U[, 1] >= 1 - q & U[, 2] <= q)

set.seed(2026)
sim_venc    <- rCopula(3e5, cop_final)
sim_segunda <- rCopula(3e5, aj_postos[[segunda]]@copula)
sim_normal  <- rCopula(3e5, aj_postos[["Normal"]]@copula)

cantos <- rbind(
  observado = c(canto_se(postos), canto_id(postos)),
  c(canto_se(sim_venc), canto_id(sim_venc)),
  c(canto_se(sim_segunda), canto_id(sim_segunda)),
  `Normal (sem dep. de cauda)` = c(canto_se(sim_normal), canto_id(sim_normal))
)
rownames(cantos)[2:3] <- c(nome_copula, segunda)
colnames(cantos) <- c("canto sup-esq (crise)", "canto inf-dir (benigno)")

# As contagens só fazem sentido na linha dos dados; nas linhas de modelo a
# célula leva um traço, que é mais honesto do que um NA — este último, numa
# tabela de relatório, parece conta malfeita.
dias <- c(sprintf("%d / %d", sum(postos[, 1] <= 0.05 & postos[, 2] >= 0.95),
                             sum(postos[, 1] >= 0.95 & postos[, 2] <= 0.05)),
          "—", "—", "—")
cantos <- data.frame(round(cantos, 4), `dias observados (crise / benigno)` = dias,
                     check.names = FALSE)
salvar_tabela("tab14_cantos", cantos, digitos = 4)            # Tabela 9


## ---- Aderência -------------------------------------------------------------
# Cramér-von Mises sobre a cópula empírica, versão Sn com bootstrap paramétrico.
# N modesto para a execução terminar em tempo razoável: o erro de simulação do
# p-valor é da ordem de 1/sqrt(N) e a resolução mínima é 1/(N+1). Cada chamada
# reestima a cópula N vezes e custa cerca de 0,4 s por réplica.

N_gof <- if (nzchar(Sys.getenv("IC_RAPIDO"))) 30 else 200

aderencia <- function(U, nomes, N = N_gof) {
  sapply(nomes, function(nm) {
    cp <- familias_copula[[nm]]
    # A t precisa de df inteiro fixado para que pCopula() exista.
    if (nm == "t") {
      dfl <- gl_t(aj_postos[["t"]]@copula)
      cp <- tCopula(dim = 2, df = round(dfl), df.fixed = TRUE)
    }
    tryCatch(gofCopula(cp, x = U, N = N, method = "Sn",
                       estim.method = "mpl")$p.value,
             error = function(e) NA_real_)
  })
}

set.seed(2026)
nomes_gof <- unique(c(nome_copula, segunda, "Normal", "Frank"))
tab_gof <- data.frame(`p-valor (Sn)` = aderencia(postos, nomes_gof),
                      check.names = FALSE)
salvar_tabela("tab15_aderencia", tab_gof, digitos = 4)        # Tabela 10


# =============================================================================
# 7. RECONSTRUÇÃO DA DISTRIBUIÇÃO CONJUNTA
# =============================================================================
# Seção 3.4 do relatório. Teorema de Sklar: H = C(F, G); derivando, h = c·f·g.

H_conj <- function(xq, yq) pCopula(cbind(Fx(xq), Gy(yq)), copula = cop_final)
h_conj <- function(xq, yq)
  dCopula(cbind(Fx(xq), Gy(yq)), copula = cop_final) * fx(xq) * gy(yq)

# Verificação estrutural: fazendo y -> Inf, a conjunta deve devolver a marginal.
# É uma identidade, então qualquer discrepância indicaria erro de código.
verif <- rbind(
  `H(x, Inf) e F(x), em x = 1%` = c(modelo = H_conj(0.01, 1e3), referência = Fx(0.01)),
  `H(Inf, y) e G(y), em y = 1%` = c(H_conj(1e3, 0.01), Gy(0.01)),
  `H(Inf, Inf)`                 = c(H_conj(1e3, 1e3), 1)
)
verif <- cbind(verif, diferença = verif[, 1] - verif[, 2])
salvar_tabela("tab16_verificacoes_conjunta", verif, digitos = 8)

# Verificação empírica — esta sim testa o modelo. Retângulos do tipo
# (X <= a, Y <= b), escolhidos para cobrir do centro à cauda.
ret <- list(c(0.01, 0.01), c(-0.01, -0.005), c(-0.02, 0.00), c(-0.03, 0.01))

retangulos <- t(sapply(ret, function(e) c(
  modelo   = H_conj(e[1], e[2]),
  empírico = mean(x <= e[1] & y <= e[2]),
  dias     = sum(x <= e[1] & y <= e[2])
)))
rownames(retangulos) <- sapply(ret, function(e)
  sprintf("X <= %+.1f%% , Y <= %+.1f%%", 100 * e[1], 100 * e[2]))
salvar_tabela("tab17_retangulos", retangulos, digitos = 4)    # Tabela 11

# Figura 4 do relatório.
salvar_figura("fig22_densidade_conjunta", function() {
  # A grade cobre aproximadamente 99% da massa de cada marginal.
  xs <- seq(-0.05, 0.05, length.out = 140)
  ys <- seq(-0.03, 0.03, length.out = 140)
  z  <- outer(xs, ys, h_conj)

  plot(100 * x, 100 * y, pch = 16, cex = 0.4, col = transp(grafite, 0.18),
       xlim = c(-5, 5), ylim = c(-3, 3),
       xlab = "Ibovespa (%)", ylab = "USD/BRL (%)",
       main = "Densidade conjunta estimada h(x, y)")
  contour(100 * xs, 100 * ys, z, nlevels = 11, add = TRUE, lwd = 1.6,
          col = rampa_vermelha(11), drawlabels = FALSE)
  abline(h = 0, v = 0, col = grafite, lty = 3)
}, altura = 4.8)


# =============================================================================
# 8. VALUE AT RISK
# =============================================================================
# Seções 2.6 e 3.5 do relatório.

alpha <- 0.01     # nível de risco adotado em todo o restante do arquivo
V0    <- 100000   # posição hipotética de R$ 100.000


## ---- VaR marginal ----------------------------------------------------------

q_x <- Qx(alpha)

var_marginal <- c(
  `quantil 1% (log-retorno)` = q_x,
  `perda percentual`         = 100 * (1 - exp(q_x)),
  # Arredondado já aqui: são reais, e o `digitos` do salvar_tabela é comum a
  # todas as colunas, calibrado para o quantil.
  `perda em reais`           = round(V0 * (1 - exp(q_x)), 2)
)
salvar_tabela("tab18_var_marginal", as.data.frame(t(var_marginal)), digitos = 4)


## ---- Probabilidades condicionais -------------------------------------------
# P(X <= x | Y = y) = dC(u,v)/dv em u = F(x), v = G(y). A derivada é calculada
# por diferença central: a opção pelo cálculo numérico, em vez da fórmula
# analítica de cada família, é proposital — o mesmo código serve para qualquer
# cópula da comparação, incluindo as rotacionadas, cujas derivadas não estão
# disponíveis em forma fechada no pacote.

# Os limites de v0 ± eps são presos ao intervalo [0,1]: para um y de cauda, v0
# fica muito perto da borda e a diferença central sairia do domínio. Quando isso
# acontece o passo efetivo encolhe, e é por ele que se divide.
dCdv <- function(u0, v0, cop = cop_final, eps = 1e-5) {
  v_sup <- pmin(v0 + eps, 1)
  v_inf <- pmax(v0 - eps, 0)
  (pCopula(cbind(u0, v_sup), cop) - pCopula(cbind(u0, v_inf), cop)) / (v_sup - v_inf)
}

y_baixa <- log(1 - 0.02)    # dólar recuou 2% no dia
y_alta  <- log(1 + 0.02)    # dólar avançou 2% no dia

cond <- c(
  `P(IBOV cair | dólar -2%)`   = dCdv(Fx(0), Gy(y_baixa)),
  `P(IBOV cair | dólar +2%)`   = dCdv(Fx(0), Gy(y_alta)),
  `P(IBOV cair) incondicional` = Fx(0)
)
salvar_tabela("tab19_prob_condicionais", as.data.frame(t(cond)), digitos = 4)


## ---- VaR condicional -------------------------------------------------------
# Inverte-se a relação anterior: procura-se o u que satisfaz
# dC/dv (u, G(y0)) = alpha e leva-se esse u de volta à escala de retornos pela
# quasi-inversa.

VaR_cond <- function(alpha, y0, cop = cop_final, Fq = Qx, Gp = Gy, eps = 1e-5) {
  v0 <- Gp(y0)
  equacao <- function(uu) dCdv(uu, v0, cop, eps) - alpha
  # A guarda devolve NA em vez de um número pinçado na borda do intervalo de
  # busca, caso a condicional já ultrapasse alpha no menor u pesquisável.
  tryCatch(Fq(uniroot(equacao, c(1e-6, 1 - 1e-6))$root),
           error = function(e) NA_real_)
}

comparacao_var <- c(
  `VaR 1% | dólar -2%`   = 100 * (1 - exp(VaR_cond(alpha, y_baixa))),
  `VaR 1% | dólar +2%`   = 100 * (1 - exp(VaR_cond(alpha, y_alta))),
  `VaR 1% incondicional` = 100 * (1 - exp(q_x))
)
salvar_tabela("tab20_var_condicional", as.data.frame(t(comparacao_var)), digitos = 3)  # Tabela 12

# Figura 5 do relatório.
salvar_figura("fig23_var_condicional", function() {
  grade_y   <- seq(-0.03, 0.03, length.out = 61)
  var_grade <- sapply(grade_y, function(yy) VaR_cond(alpha, yy))

  perda <- 100 * (1 - exp(var_grade))      # convertido em perda percentual
  variacao_dolar <- 100 * (exp(grade_y) - 1)

  plot(variacao_dolar, perda, type = "l", lwd = 2.6, col = vermelho,
       xlab = "variação do dólar no dia (%)",
       ylab = "VaR 1% do Ibovespa (% de perda)",
       main = "VaR condicional do Ibovespa")
  abline(h = 100 * (1 - exp(q_x)), lty = 2, lwd = 1.8, col = vinho)
  points(c(-2, 2), comparacao_var[1:2], pch = 21, bg = vinho, col = "white", cex = 1.5)
  text(c(-2, 2), comparacao_var[1:2], labels = c("-2%", "+2%"),
       pos = c(1, 3), cex = 0.85, col = vinho)
  legend("topleft", c("VaR condicional", "VaR incondicional"),
         col = c(vermelho, vinho), lwd = c(2.6, 1.8), lty = c(1, 2),
         bty = "n", cex = 0.85)
})


## ---- VaR de uma carteira ---------------------------------------------------
# Decomposição de Sklar na direção inversa: sorteiam-se pares (u,v) da cópula,
# que carregam a dependência, e cada coordenada é levada à escala de retornos
# pela quasi-inversa da respectiva marginal.
#
# A semente é fixada DENTRO da função. Sem isso, a mesma carteira calculada aqui
# e na seção de robustez sairia com números ligeiramente diferentes, por estarem
# em pontos distintos da sequência aleatória — e um relatório que apresenta dois
# valores para a mesma quantidade não tem defesa.

var_carteira_sim <- function(cop, Qa, Qb, alpha, pesos = c(0.5, 0.5),
                             n_sim = 100000, semente = 2026) {
  set.seed(semente)
  s  <- rCopula(n_sim, cop)                  # 1) estrutura de dependência
  Rc <- pesos[1] * (exp(Qa(s[, 1])) - 1) + pesos[2] * (exp(Qb(s[, 2])) - 1)

  # Contrafactual: mesma carteira, mesmas marginais, mas sob independência. A
  # diferença entre os dois números é o valor do hedge natural entre os ativos.
  Ri <- pesos[1] * (exp(Qa(runif(n_sim))) - 1) + pesos[2] * (exp(Qb(runif(n_sim))) - 1)

  list(retornos = Rc,
       VaR      = unname(-quantile(Rc, alpha)),
       VaR_ind  = unname(-quantile(Ri, alpha)))
}

cart <- var_carteira_sim(cop_final, Qx, Qy, alpha)
R_carteira   <- cart$retornos
VaR_carteira <- cart$VaR
VaR_ind      <- cart$VaR_ind

carteira <- c(`VaR 1% com a cópula (%)`      = 100 * VaR_carteira,
              `VaR 1% sob independência (%)` = 100 * VaR_ind,
              `redução (p.p.)`               = 100 * (VaR_ind - VaR_carteira))
salvar_tabela("tab21_var_carteira", as.data.frame(t(carteira)), digitos = 3)


# =============================================================================
# 9. VALIDAÇÃO FORA DA AMOSTRA
# =============================================================================
# Seções 2.7 e 3.6 do relatório. A divisão é TEMPORAL, nunca aleatória: os
# primeiros 80% dos dias formam o treino e os 20% finais o teste. Sortear
# observações misturaria futuro e passado e produziria uma avaliação otimista
# sem valor prático. Todo o procedimento é refeito com o treino apenas, da
# seleção da marginal ao ajuste da cópula.

backtest <- function(a, alpha = 0.01, prop = 0.8) {
  corte <- floor(prop * a$n)
  x_tr <- a$x[1:corte];          y_tr <- a$y[1:corte]
  x_te <- a$x[(corte + 1):a$n];  y_te <- a$y[(corte + 1):a$n]
  datas_te <- a$datas[(corte + 1):a$n]

  # --- modelo reajustado só no treino
  mx <- escolher_marginal(x_tr); my <- escolher_marginal(y_tr)
  Ft <- mx$ajuste$p; Qt <- mx$ajuste$q; Gt <- my$ajuste$p

  ajs <- ajustar_copulas(cbind(Ft(x_tr), Gt(y_tr)))
  tb  <- tabela_copulas(ajs, corte)
  nm  <- rownames(tb)[1]
  cop <- ajs[[nm]]@copula
  if (is(cop, "tCopula") && !isTRUE(cop@df.fixed))
    cop <- fitCopula(tCopula(dim = 2, df = round(gl_t(cop)), df.fixed = TRUE),
                     data = cbind(Ft(x_tr), Gt(y_tr)), method = "mpl")@copula

  # --- 1) cobertura do VaR marginal (teste binomial exato, na linha de Kupiec)
  limiar <- Qt(alpha)
  viol_m <- sum(x_te < limiar)
  p_kupiec <- binom.test(viol_m, length(x_te), p = alpha)$p.value

  # --- 2) transformada integral de probabilidade condicional (Rosenblatt)
  w_te <- dCdv(Ft(x_te), Gt(y_te), cop = cop)
  p_pit <- ks.test(w_te, "punif")$p.value

  # --- 3) concorrente linear: x = b0 + b1 y + e, com quantil EMPÍRICO dos
  # resíduos do treino. Usar os resíduos do teste seria olhar para o resultado
  # antes de fazer a previsão.
  reg <- lm(x_tr ~ y_tr)
  VaR_reg <- predict(reg, newdata = data.frame(y_tr = y_te)) +
             unname(quantile(residuals(reg), alpha))

  # --- 4) VaR condicional da cópula, recalculado dia a dia no teste
  VaR_cop <- sapply(y_te, function(yt) VaR_cond(alpha, yt, cop = cop, Fq = Qt, Gp = Gt))

  list(corte = corte, datas_te = datas_te, x_te = x_te, y_te = y_te,
       marginais = c(mx$nome, my$nome), copula = nm, cop = cop,
       limiar = limiar, viol_marginal = viol_m, esperadas = alpha * length(x_te),
       p_kupiec = p_kupiec, w_te = w_te, p_pit = p_pit,
       r2 = summary(reg)$r.squared, VaR_reg = VaR_reg, VaR_cop = VaR_cop,
       viol_reg = sum(x_te < VaR_reg),
       viol_cop = sum(x_te < VaR_cop, na.rm = TRUE))
}

bt <- backtest(am$completa, alpha)

# Dias de treino/teste, famílias selecionadas no treino e R² da regressão —
# todos citados no texto da Seção 3.6.
print(data.frame(
  valor = c(bt$corte, length(bt$x_te), paste(bt$marginais, collapse = " / "),
            bt$copula, round(bt$r2, 3)),
  row.names = c("dias de treino", "dias de teste", "marginais no treino",
                "cópula no treino", "R² da regressão")))

# Cobertura do VaR marginal (Kupiec)
cobertura <- c(`violações observadas` = bt$viol_marginal,
               `violações esperadas`  = bt$esperadas,
               `p-valor (binomial)`   = bt$p_kupiec)
salvar_tabela("tab22_cobertura_var", as.data.frame(t(cobertura)), digitos = 4)

# Comparação com a regressão linear
comp_bt <- data.frame(
  `taxa de violação` = c(mean(bt$x_te < bt$VaR_reg),
                         mean(bt$x_te < bt$VaR_cop, na.rm = TRUE), alpha),
  `VaR médio (%)`    = c(sprintf("%.2f", 100 * mean(1 - exp(bt$VaR_reg))),
                         sprintf("%.2f", 100 * mean(1 - exp(bt$VaR_cop), na.rm = TRUE)),
                         "—"),
  row.names = c("regressão linear", "cópula", "alvo"),
  check.names = FALSE
)
salvar_tabela("tab23_backtest", comp_bt, digitos = 4)         # Tabela 13

# Figura 6 do relatório. Dois painéis: no tempo as duas fronteiras andam quase
# juntas, já que ambas partem do mesmo dólar diário; a diferença entre os
# métodos só fica visível quando se troca o eixo do tempo pelo eixo do dólar.
salvar_figura("fig26_backtest", function() {
  par(mfrow = c(2, 1), mgp = c(2.5, 0.7, 0), las = 1, bty = "n",
      cex.main = 1.02, font.main = 2, col.main = vinho,
      col.axis = grafite, col.lab = grafite, fg = grafite)

  ## painel (a) — backtest no tempo
  par(mar = c(2.8, 4.4, 3.4, 1.2))
  faixa <- range(100 * c(bt$x_te, bt$VaR_reg, bt$VaR_cop), na.rm = TRUE)
  plot(bt$datas_te, 100 * bt$x_te, type = "n",
       ylim = c(faixa[1], faixa[2] + 2.4), xlab = "",
       ylab = "log-retorno do Ibovespa (%)", main = "a) Backtest no tempo")
  mtext(sprintf("violações: cópula %d, regressão %d — esperadas %.1f em %d pregões",
                bt$viol_cop, bt$viol_reg, bt$esperadas, length(bt$x_te)),
        side = 3, line = 0.3, cex = 0.72, col = grafite)

  # A faixa entre as duas fronteiras torna visível uma diferença que, de outro
  # modo, se perde na oscilação diária das duas curvas.
  polygon(c(as.numeric(bt$datas_te), rev(as.numeric(bt$datas_te))),
          c(100 * bt$VaR_cop, rev(100 * bt$VaR_reg)),
          col = transp(grafite, 0.16), border = NA)
  segments(bt$datas_te, 0, bt$datas_te, 100 * bt$x_te,
           col = transp(grafite, 0.40), lwd = 1)
  abline(h = 0, col = grafite)
  lines(bt$datas_te, 100 * bt$VaR_reg, lwd = 2.0, col = azul)
  lines(bt$datas_te, 100 * bt$VaR_cop, lwd = 2.0, col = vermelho)

  if (bt$viol_cop > 0) {
    sel <- which(bt$x_te < bt$VaR_cop)
    points(bt$datas_te[sel], 100 * bt$x_te[sel],
           pch = 21, bg = vermelho, col = "white", cex = 1.1)
  }
  legend("top", c("retorno observado", "VaR — cópula", "VaR — regressão"),
         col = c(transp(grafite, 0.55), vermelho, azul), lwd = c(1, 2, 2),
         bty = "n", cex = 0.8, horiz = TRUE)

  ## painel (b) — fronteiras em função do dólar
  par(mar = c(4.0, 4.4, 3.0, 1.2))
  var_dolar <- 100 * (exp(bt$y_te) - 1)
  o <- order(var_dolar)          # ordenar é o que transforma os pontos em curva

  plot(var_dolar, 100 * bt$x_te, pch = 16, cex = 0.5, col = transp(grafite, 0.32),
       ylim = range(100 * c(bt$x_te, bt$VaR_cop, bt$VaR_reg), na.rm = TRUE),
       xlab = "variação do dólar no dia (%)",
       ylab = "log-retorno do Ibovespa (%)",
       main = "b) As duas fronteiras em função do dólar")
  abline(h = 0, col = grafite, lty = 3)
  polygon(c(var_dolar[o], rev(var_dolar[o])),
          c(100 * bt$VaR_cop[o], rev(100 * bt$VaR_reg[o])),
          col = transp(grafite, 0.16), border = NA)
  lines(var_dolar[o], 100 * bt$VaR_reg[o], lwd = 2.4, col = azul)
  lines(var_dolar[o], 100 * bt$VaR_cop[o], lwd = 2.4, col = vermelho)
  legend("bottomleft", c("VaR — cópula", "VaR — regressão", "retorno observado"),
         col = c(vermelho, azul, transp(grafite, 0.5)), lwd = c(2.4, 2.4, NA),
         pch = c(NA, NA, 16), pt.cex = 0.7, bty = "n", cex = 0.8)
}, altura = 6.4)


# =============================================================================
# 10. ROBUSTEZ: COM E SEM O PERÍODO DE PANDEMIA
# =============================================================================
# Seção 3.7 do relatório. Toda a análise é repetida na amostra que começa em
# janeiro de 2021. A função abaixo encadeia exatamente as mesmas funções usadas
# acima — não há caminho alternativo de código, o que é o que dá sentido à
# comparação.

analisar <- function(a, alpha = 0.01, N = N_gof) {
  mx <- escolher_marginal(a$x); my <- escolher_marginal(a$y)
  Fa <- mx$ajuste$p; Qa <- mx$ajuste$q
  Gb <- my$ajuste$p; Qb <- my$ajuste$q

  U  <- cbind(Fa(a$x), Gb(a$y))
  P  <- pobs(cbind(a$x, a$y))

  ajs_P <- ajustar_copulas(P); tb_P <- tabela_copulas(ajs_P, a$n)
  ajs_U <- ajustar_copulas(U); tb_U <- tabela_copulas(ajs_U, a$n)

  nm  <- rownames(tb_P)[1]
  cop <- ajs_U[[nm]]@copula
  dfl <- NA_real_
  if (is(cop, "tCopula") && !isTRUE(cop@df.fixed)) {
    dfl <- gl_t(cop)
    cop <- fitCopula(tCopula(dim = 2, df = round(dfl), df.fixed = TRUE),
                     data = U, method = "mpl")@copula
  }
  lam <- lambda_cantos(cop)

  cp_gof <- if (nm == "t") tCopula(dim = 2, df = round(dfl), df.fixed = TRUE)
            else familias_copula[[nm]]
  p_gof <- tryCatch(gofCopula(cp_gof, x = P, N = N, method = "Sn",
                              estim.method = "mpl")$p.value,
                    error = function(e) NA_real_)

  cart <- var_carteira_sim(cop, Qa, Qb, alpha)   # VaR de carteira 50/50
  b <- backtest(a, alpha)

  list(mx = mx, my = my, tb_P = tb_P, tb_U = tb_U, cop = cop, nome = nm,
       Fa = Fa, Qa = Qa, Gb = Gb, Qb = Qb, lam = lam, df = dfl, p_gof = p_gof,
       cart = cart, bt = b,
       resumo = c(
         `n` = a$n,
         `desvio-padrão IBOV (%)`   = 100 * sd(a$x),
         `desvio-padrão USD (%)`    = 100 * sd(a$y),
         `assimetria IBOV`          = assimetria(a$x),
         `curtose em excesso IBOV`  = curtose_ex(a$x),
         `tau de Kendall`           = cor(a$x, a$y, method = "kendall"),
         `nu da t — IBOV`           = mx$nu_t,
         `nu da t — USD`            = my$nu_t,
         `tau implicado pelo modelo`= tau(cop),
         `lambda sup-esq (crise)`   = unname(lam["sup-esq"]),
         `p-valor de aderência`     = p_gof,
         `VaR 1% marginal (%)`      = 100 * (1 - exp(Qa(alpha))),
         `VaR 1% | dólar +2% (%)`   = 100 * (1 - exp(
              VaR_cond(alpha, log(1.02), cop = cop, Fq = Qa, Gp = Gb))),
         `VaR 1% carteira (%)`            = 100 * cart$VaR,
         `VaR 1% carteira sob indep. (%)` = 100 * cart$VaR_ind,
         `violações no teste`       = b$viol_marginal,
         `violações esperadas`      = b$esperadas,
         `p-valor de Kupiec`        = b$p_kupiec,
         `p-valor do PIT`           = b$p_pit
       ))
}

set.seed(2026)
res <- lapply(am, analisar)

tab_robustez <- data.frame(
  completa = res$completa$resumo,
  sem_2020 = res$sem_2020$resumo,
  check.names = FALSE
)
colnames(tab_robustez) <- unname(rotulos)

# Deliberadamente sem coluna de variação percentual: ela seria ilegível em
# p-valores, indefinida na contagem de violações (que vale zero na amostra
# completa) e enganosa na assimetria, que muda de magnitude e quase de sinal.
# A comparação relevante é lida diretamente das duas colunas.
salvar_tabela("tab24_robustez", tab_robustez, digitos = 4)    # Tabela 14

familias_por_amostra <- data.frame(
  `marginal IBOV` = c(res$completa$mx$nome, res$sem_2020$mx$nome),
  `marginal USD`  = c(res$completa$my$nome, res$sem_2020$my$nome),
  `cópula`        = c(res$completa$nome,    res$sem_2020$nome),
  row.names = unname(rotulos), check.names = FALSE
)
salvar_tabela("tab25_familias_por_amostra", familias_por_amostra)

salvar_tabela("tab26_copulas_sem2020", res$sem_2020$tb_P, digitos = 2)


# =============================================================================
# 11. SÍNTESE
# =============================================================================
# Tabela final do relatório: reúne, numa linha cada, o resultado produzido por
# cada ferramenta empregada, do tamanho da amostra ao último teste de validação.

sintese <- data.frame(
  resultado = c(
    format(min(datas)), format(max(datas)), as.character(n),
    nome_marg_x, nome_marg_y,
    sprintf("%.3f", tau_emp),
    nome_copula,
    sprintf("%.3f", unname(coef(ajuste_cop)[1])),
    if (is.na(df_livre)) "—" else sprintf("%.2f", df_livre),
    sprintf("%.3f", tau(cop_final)),
    sprintf("%.3f", unname(lam_final["sup-esq"])),
    sprintf("%.3f", tab_gof[nome_copula, 1]),
    sprintf("%.2f%%", 100 * (1 - exp(q_x))),
    sprintf("%.2f%%", comparacao_var[["VaR 1% | dólar +2%"]]),
    sprintf("%.2f%%", 100 * VaR_carteira),
    sprintf("%.2f p.p.", 100 * (VaR_ind - VaR_carteira)),
    sprintf("%d de %.1f esperadas", bt$viol_marginal, bt$esperadas),
    sprintf("%.3f", bt$p_pit)
  ),
  row.names = c(
    "Início da amostra", "Fim da amostra", "Observações",
    "Marginal do Ibovespa", "Marginal do USD/BRL",
    "Tau de Kendall empírico",
    "Cópula selecionada", "Parâmetro da cópula", "Graus de liberdade",
    "Tau implicado pelo modelo", "Lambda no canto de crise",
    "P-valor de aderência",
    "VaR 1% marginal do Ibovespa", "VaR 1% dado dólar +2%",
    "VaR 1% da carteira 50/50", "Redução do VaR pela dependência",
    "Violações no período de teste", "P-valor do PIT condicional"
  )
)
salvar_tabela("tab27_sintese", sintese)


# =============================================================================
# 12. ENCERRAMENTO
# =============================================================================

cat("Figuras gravadas em", dir_fig, ":", length(list.files(dir_fig, "\\.pdf$")), "\n")
cat("Tabelas gravadas em", dir_tab, ":", length(list.files(dir_tab, "\\.tex$")), "\n")
cat(sessionInfo()$R.version$version.string, "\n")
