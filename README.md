# Cópulas: Ibovespa, dólar e Value at Risk

Iniciação científica *Modelagem da Função de Distribuição Conjunta de Variáveis Aleatórias por Meio da Teoria de Cópulas* (PIBIC/CNPq, IMECC, Unicamp, set/2025 – ago/2026), apresentada no XXXIV Congresso de Iniciação Científica da Unicamp.

- Autor: Gabriel de Andrade Belchior
- Orientação: Profa. Dra. Verónica Andrea González-López

Este repositório tem duas partes:

- **O site** (`index.html`, publicado no GitHub Pages): a teoria usada no trabalho em sete capítulos curtos com simulações e questões, que vão de log-retornos e VaR até a melhoria proposta (cópula sobre os resíduos de modelos GARCH).
- **O código** (`codigo/R_usado.R`): o script em R que reproduz todas as tabelas e figuras do relatório final.

## O problema

Quanto um investidor pode perder num dia ruim quando o Ibovespa e o dólar se movem juntos? Uma correlação resume a associação num número só e não diz o que acontece nos extremos. A teoria de cópulas separa o comportamento de cada ativo (as marginais) da estrutura de dependência entre eles, e o teorema de Sklar garante que as duas partes recompõem uma distribuição conjunta legítima.

## O que foi feito

1. **Dados:** 1.205 log-retornos diários pareados do Ibovespa e do USD/BRL (jan/2020 – jun/2025), do Yahoo Finance.
2. **Marginais:** 6 famílias candidatas (normal, logística, Laplace, t de Student, Johnson SU e t assimétrica), ajustadas por máxima verossimilhança e escolhidas por AIC e BIC.
3. **Cópula:** 10 famílias candidatas, ajustadas por máxima pseudo-verossimilhança, com seleção por AIC/BIC, teste de Vuong e teste de aderência por bootstrap.
4. **VaR:** VaR de 1% do Ibovespa isolado, condicionado ao dólar e de uma carteira 50/50, este por simulação de 100 mil pares.
5. **Validação fora da amostra:** reestimação nos primeiros 80% dos dias e backtest nos 241 restantes, com o teste de Kupiec, a transformada de Rosenblatt e comparação com uma regressão linear.
6. **Robustez:** toda a análise repetida sem o ano de 2020, para separar o efeito da crise da pandemia.

## Resultados

| | |
|---|---|
| Cópula escolhida | t, com 41,7 unidades de AIC à frente da segunda colocada; AIC e BIC concordam |
| Dependência de cauda | λ = 0,159 no canto de crise (Ibovespa em queda e dólar em alta); teste de aderência não rejeita (p = 0,326) |
| VaR condicional | ignorar o dólar subestima o VaR de 1% do Ibovespa em 1,48 p.p. (37%) num dia de alta de 2% do câmbio |
| VaR de carteira 50/50 | 2,06% com a cópula contra 2,32% supondo independência: a dependência negativa funciona como proteção |
| Fora da amostra | zero violações para a cópula e para a regressão, mas VaR médio de 3,69% contra 3,98%: a mesma proteção com 0,29 p.p. a menos de capital reservado |

**Limitação principal:** os retornos têm volatilidade agrupada, então os p-valores são otimistas, e o teste de Rosenblatt rejeita a especificação condicional na amostra completa (p = 0,020), mas não sem 2020 (p = 0,370). O próximo passo natural é ajustar a cópula aos resíduos de um modelo GARCH.

## Como rodar o código

```r
# R 4.4 ou mais novo
install.packages(c("copula", "ExtDist", "fGarch", "quantmod"))
```

```bash
cd codigo
Rscript R_usado.R
```

O script baixa as séries do Yahoo Finance na primeira execução e guarda uma cópia em `dados_ic/`. As tabelas saem em `tabelas_relatorio/` e as figuras em `figuras_relatorio/`. Leva de 10 a 15 minutos, por causa do bootstrap e do backtest. Com a variável de ambiente `IC_RAPIDO` definida, o bootstrap cai de 200 para 30 réplicas. O Yahoo revisa os preços ajustados de tempos em tempos, então os números podem variar um pouco em relação ao relatório.
