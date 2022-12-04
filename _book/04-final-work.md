# Trabalho final {#final-work}

Impacto da taxa Selic no índice Bovespa

## Introdução

Após a regulamentação do Anexo IV e da implementação do Plano Real, o mercado acionário brasileiro teve um grande salto no seu desenvolvimento, tanto em termos de volume dos negócios quanto na eficiência alocativa... A grande conquista do Plano Real foi o controle da inflação. Taxas de inflação em níveis aceitáveis foram e estão sendo mantidas sob controle, devido às constantes intervenções governamentais, aumentando ou reduzindo as taxas de juros, tanto as de curto quanto as de longo prazo, controlando a oferta monetária através dos depósitos compulsórios sobre depósitos à vista, bem como controlando a taxa de câmbio, instrumento utilizado pelo BACEN por um longo período. Há, portanto, por parte de todos os agentes, a necessidade de se conhecer como o mercado acionário responde às mudanças dessas variáveis macroeconômicas. (DE SOUZA GRÔPPO, 2005)

Para os participantes do mercado financeiro, há um interesse dada a influência exercida pela política monetária sobre os mercados, conhecer tais estimativas representaria uma relevante informação para a construção de posições e estratégias, tanto de investimento, bem como para a gestão dos riscos implícitos às operações normalmente desenvolvidas em seu dia-a-dia. (GONÇALVES JUNIOR, 2007)

De acordo com o conhecimento convencional, mudanças nas taxas de juros afetariam o custo de capital e as expectativas futuras quanto à lucratividade das empresas, impactando portanto sua geração de dividendos ao acionista, especialmente se considerada a valores presentes; em última análise, isto repercute no valor de mercado atual da empresa, e portanto de suas ações em Bolsa. (GONÇALVES JUNIOR, 2007)

Porém, a atividade econômica também exerce efeito importante sobre a determinação dos preços na Bolsa de Valores, particularmente no Ibovespa. Os preços das ações são positivamente relacionados com a atividade econômica, medida pelo nível de produção industrial. A política monetária possui efeito significativo sobre a produção industrial o que indicaria a existência do segundo canal pelo qual a política monetária afeta os preços das ações.(DOS SANTOS et al., 2006)

## Objetivo

O objetivo é mensurar e analisar as respostas do mercado acionário às decisões do Comitê de Política Monetária do Banco Central do Brasil (COPOM) em relação à taxa básica de juro, verificando o comportamento do índice agregado da Bolsa de Valores de São Paulo (Bovespa).

## Problema

Quantificar ou prever a influência da taxa selic no índice Bovespa demanda modelos de predições dinâmicas.

A modelagem prevê predizer o Indice Bovespa (serie temporal em base diária) em função das covariáveis (taxa Selic anunciada mês a mês). 

Pressupõe-se que cortes não previstos na taxa de juros Selic proporcionem uma valorização das ações, já que abriria espaço para expansão da economia, com aumentos dos lucros e dos dividendos distribuídos pelas empresas.
 
Mercados eficientes antecipam as decisões do COPOM, o que não deveria sensibilizar o índice bovespa, uma vez que que as mudanças já foram incorporadas ao valor das ações, no entando, as surpresas, a diferença da expectativa e da decisão, deveriam causar uma resposta do índice. O estudo define uma medida que procura captar a surpresa gerada nos mercados. As medidas foram baseadas em dados do mercado de futuros DI 1 dia, pela sua similaridade à taxa SELIC. (GONÇALVES JUNIOR, 2007)

## Metodologia

A meta para a taxa básica de juro, inicialmente referida como TBC, teve sua sistemática alterada em agosto de 1998, quando o Banco Central passou a fazer uso também da TBAN (taxa de assistência para redesconto bancário), ofertando e tomando recursos na banda assim definida, conforme necessidades, importância, garantia e riscos dos agentes envolvidos, aplicando-se também um sistema de quotas instituído na ocasião. Em 05/03/1999, a TBC foi extinta, tendo sido substituída pela SELIC para fins de política monetária, passando-se a empregar esta última como a taxa básica de juro e principal instrumento das operações de mercado aberto. (GONÇALVES JUNIOR, 2007)

O índice Bovespa (ou simplesmente Ibovespa) foi criado há mais de cinquenta anos, em 2 de janeiro de 1968. Antes do Ibovespa, os preços das ações negociadas na Bolsa de Valores de São Paulo (SPSE) eram divulgados por meio de boletim diário (Boletim Diário de Informação – BDI). (CASTRO et al., 2019)

De modo geral, os mercados financeiros são forward looking, o que equivale a dizer que eles estão constantemente incorporando novas informações a seus preços, seja devido a fatos que impactam imediatamente suas operações, seja por razões que modificam expectativas sobre o futuro; é a constatação de que estes agentes são racionais, utilizando toda a informação disponível para realizar suas previsões, tomando valores passados e correntes de variáveis exógenas ou não (LUCAS et al., 1996).

Cada decisão tomada pelo COPOM é estudada por esses agentes, que antecipam em alguma medida seus resultados; assim sendo, quaisquer mudanças observadas nas variáveis correntes e futuras através do tempo decorrerão exclusivamente de mudanças não antecipadas. (CASTRO et al., 2019)

Conseqüentemente, analisar-se a relação direta entre as variações na meta da taxa de juro e as do Índice Bovespa pode mostrar baixa significância entre as variáveis; de fato, se os mercados inevitavelmente não gerarão respostas a mudanças já esperadas, então é preciso distinguir na variação da meta suas componentes antecipada e não antecipada. É o que proporemos a seguir. (CASTRO et al., 2019)

Após apresentação da evolução do índice ibovesoa, taxa selic e DI, foi calculado o coeficiente de correlação linear de Pearson existente entre elas, a fim de verificar sua relação, em quatro períodos distintos:

- de 2004 a 2007: período de início da redução de juros até a crise dos Subprime,
- de 2008 a 2010: do auge da crise financeira mundial no Brasil até a fase de recuperação do mercado;
- de 2011 a 2012: período que inclui tendência de redução da taxa básica de juros e a crise financeira, iniciada na Europa em 2011.
- de 2013 a 2016: período da crise econômica de 2014, conhecida como a "grande recessão brasileira", foi uma profunda e duradoura crise econômica, sendo caracterizada por recessão por dois anos consecutivos (2015 e 2016) e por sua longa e lenta recuperação, a mais lenta da história do país. (WIKIPEDIA, 2022)
- de 2017 a 2022: período que incluem os efeitos da COVID-19, iniciada em 2020.

Os dados utilizados neste trabalho foram obtidos de banco de dados, índices e relatórios contidos em sites como da BM&F BOVESPA, BACEN e CVM, além de informações coletadas da literatura indicada.

A análise de correlação será feita utilizando o coeficiente de Pearson que varia de -1 a 1, sendo que o sinal indica a direção, positiva ou negativa, da relação e o valor sugere a força do relacionamento entre as
variáveis. 

A condução do trabalhe segue os seguintes passos:

Figura 1: Roteiro
![roteiro](pics/guide.png)
Fonte: (YOSHIDA, 2022)


### Estimando a Surpresa dos Mercados 

TODO

Um modo interessante de se conseguir tal intento é utilizar os contratos futuros de taxa média de depósitos interbancários de um dia (referidos comumente por DI 1 dia), negociados na Bolsa de Mercadorias e Futuros de São Paulo (BM&F).

Esta relação implica dizer que o preço futuro (F0) é o valor esperado do preço à vista que vigorará no futuro (ST).

Para captar a componente da variação na meta dos juros que responderia pela “surpresa” decorrente da decisão do COPOM, podemos adaptar a metodologia desenvolvida por Kuttner (2001), posteriormente aplicada ao mercado acionário americano por Bernanke e Kuttner (2005); assim, suponhamos que no dia d – 1 ocorre a reunião do COPOM e no dia útil seguinte os mercados abrem já cientes da nova meta para a taxa básica de juro – ou seja, no dia d ocorre a implantação da nova meta.

Deste modo, temos um evento ocorrendo entre os dois dias citados: trata-se de expectativas de véspera (em d – 1) dos mercados sobre a decisão do COPOM e a constatação dela no dia seguinte (dia d); isto está esquematicamente representado na figura 1 abaixo:

![](bkp/expectativa-reuniao-copom.jpg)

![](bkp/expectativa-reuniao-copom-implanta.jpg)

Assim, os mercados esperam que em d–1 a taxa média diária passe de ⎯ia (antes da reunião) para⎯im , após a reunião, e que assim permaneça até a liquidação do instrumento; no entanto, em alguma medida eles são surpreendidos pelo COPOM e esta surpresa (Δiu) passa a ser incorporada ao valor médio anteriormente previsto: os mercados se ajustam a eventuais discrepâncias em relação a suas projeções da véspera.


## Fonte de dados

TODO

- [Taxa selic](https://dados.gov.br/dataset/11-taxa-de-juros-selic/resource/3d751a0d-afb2-452b-83f2-310a201f8a82): selic.json. selic.csv
- [Índices Bovespa](https://finance.yahoo.com/quote/%5EBVSP/history?period1=735868800&period2=1668297600&interval=1d&filter=history&frequency=1d&includeAdjustedClose=true): idc_BVSP_daily.csv, idx_BVSP_monthly.csv
- [Juros DI](https://www.infomoney.com.br/ferramentas/juros-futuros-di/)



```r
library(quantmod)
library(xts)
library(sidrar)
library(timetk)
library(tidyverse)
library(jsonlite)
library(ggplot2)
library(scales)
library(BatchGetSymbols)
library(dplyr)
library(rb3)
library(pastecs)
library(ggpubr)
library(bizdays)

#conflicts(detail = TRUE)
```


```r
first_date = as.Date('2000-01-01')
last_date = as.Date('2022-11-25')
eval_period = last_date - first_date

df_bvsp = BatchGetSymbols('^BVSP', first.date = first_date,
 last.date = last_date)
```

## Índice IBOVESPA

O período de análise será de 22 anos, de 2000-01-01 até 2022-11-25.

O comportamento do índice bovespa neste período foi:


```r
ggplot(df_bvsp$df.tickers, aes(x = ref.date, y = price.close))+
 geom_line()+
 scale_y_discrete(limits=c(10000, 20000, 30000, 40000, 50000, 60000,
 70000, 80000, 90000, 100000, 110000, 120000, 130000))+
 scale_x_date(breaks = date_breaks("1 years"),
 labels = date_format("%Y"))+
 xlab('')+ylab('Pontos')+
 labs(title='Índice Bovespa',
 caption='Fonte: Yahoo Finance.')+
 geom_line() +
 geom_smooth(method = "loess", se = FALSE) +
 theme(axis.text.x = element_text(angle = 90)
)
```

```
## Warning: Continuous limits supplied to discrete scale.
## ℹ Did you mean `limits = factor(...)` or `scale_*_continuous()`?
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

<img src="04-final-work_files/figure-html/unnamed-chunk-4-1.png" width="672" />

## Indicador SELIC

A meta da taxa [SELIC](https://www.bcb.gov.br/estatisticas/grafico/graficoestatistica/metaselic) diária.



```r
df_selic_meta <- read.csv2(file = "../dados/Taxa de juros - Selic - fixada pelo Comitê de Política Monetária.csv",
                           sep=";", dec=",",
                           colClasses = c("character", "numeric", "NULL"),
                           col.names = c("ref.date", "selic.tax", ""))
df_selic_meta <- transform(df_selic_meta, ref.date = as.Date(ref.date,'%d/%m/%Y'))

df_selic_meta <- na.omit(df_selic_meta) %>% 
                    filter(ref.date >= first_date)

tail(df_selic_meta)
```

```
##        ref.date selic.tax
## 8292 2022-11-25     13.75
## 8293 2022-11-26     13.75
## 8294 2022-11-27     13.75
## 8295 2022-11-28     13.75
## 8296 2022-11-29     13.75
## 8297 2022-11-30     13.75
```

E o comportamento no período foi:


```r
ggplot(df_selic_meta, aes(x = ref.date, y = selic.tax))+
 geom_line()+
 scale_x_date(breaks = date_breaks("1 years"),
 labels = date_format("%Y"))+
 xlab('')+ylab('%')+
 labs(title='Meta taxa selic',
 caption='Fonte: Banco Central do Brasil.')+
 geom_line() +
 geom_smooth(method = "loess", se = FALSE)+
 theme(axis.text.x = element_text(angle = 90))
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

<img src="04-final-work_files/figure-html/unnamed-chunk-6-1.png" width="672" />

### Unindo os conjuntos de dados


```r
# Usando apenas valor de fechamento
df_bvsp_close <- df_bvsp$df.tickers %>%
                  dplyr::select(ref.date, price.close)

df_bvsp_selic <- merge(df_bvsp_close, df_selic_meta, by = 'ref.date', all.x = T)

df_bvsp_selic <- df_bvsp_selic %>% 
                    fill(selic.tax, .direction = "downup")
```

#### Salvando o conjunto de dados


```r
# antes de proceder com o restante do exercicio, vamos salvar o dataset para
# o usar nos proximos modulos
write_rds(df_bvsp_selic, "../dados/series_bvsp_selic_10.rds")
```

## Correlação ibovespa e selic


```r
bvsp_norm <- sd(df_bvsp_selic$price.close, na.rm=TRUE)
selic_norm <- sd(df_bvsp_selic$selic.tax, na.rm=TRUE)

ggplot(df_bvsp_selic, aes(x=ref.date)) +
  
  geom_line(aes(y=price.close / bvsp_norm), size=2, color="blue", na.rm = TRUE, alpha=.6) + 
  stat_smooth(size=2, aes(y=price.close / bvsp_norm), color="lightblue") +
  geom_line(aes(y=selic.tax / selic_norm), size=2, color="red", na.rm = TRUE, alpha=.6) +
  stat_smooth(size=2, aes(y=selic.tax / selic_norm), color = "tomato") +

  scale_y_continuous(
    
    # Features of the first axis
    name = "bvsp",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./selic_norm, name="selic")
  ) +

  ggtitle("Ibovespa vs Selic")
```

```
## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
## `geom_smooth()` using method = 'gam' and formula = 'y ~ s(x, bs = "cs")'
```

<img src="04-final-work_files/figure-html/unnamed-chunk-9-1.png" width="672" />


## Decomposição anual

TODO

## Análise de _outliers_

TODO

## Correlação cruzada ibovespa e selic

CCF - Cross Correlation Function

Dado duas séries temporais $y_t, x_t$

Conjunto de correlações $x_{t+h}$ e $y_t$ onde $h = 0, \pm 1, \pm 2, \pm p$

Um valor negativo de $h$ é a correlação entre a série $x_t$ em um tempo antes de $t$ com a série $y_t$ no tempo $t$.



```r
# https://www.lobdata.com.br/2020/09/15/how-to-perform-correlation-analysis-in-time-series-data-using-r/

df_bvsp_selic_long <- df_bvsp_selic %>% 
  pivot_longer(cols = c(price.close, selic.tax), names_to = "names", values_to = "value")
```


```r
# the series
df_bvsp_selic_long %>% 
  plot_time_series(.date_var = ref.date,
                   .value = value,
                   .facet_vars = names)
```

```{=html}
<div id="htmlwidget-5f69c8b6f31cf82fde04" style="width:672px;height:480px;" class="plotly html-widget"></div>
```

```r
# acf/pacf plots
df_bvsp_selic_long %>%
  group_by(names) %>% 
  plot_acf_diagnostics(.date_var = ref.date,
                       .value = value,
                       .show_white_noise_bars = T)
```

```{=html}
<div id="htmlwidget-1473fa83757f4a42eca4" style="width:672px;height:480px;" class="plotly html-widget"></div>
```


```r
# CCF plot
df_bvsp_selic %>% 
  plot_acf_diagnostics(.date_var = ref.date,
                       .value = price.close,
                       .ccf_vars = selic.tax,
                       .show_ccf_vars_only = T)
```

```{=html}
<div id="htmlwidget-d68c0f340fb1ba41cfa6" style="width:672px;height:480px;" class="plotly html-widget"></div>
```



```r
stat.desc(df_bvsp_selic)
```

```
##                  ref.date  price.close    selic.tax
## nbr.val      5.667000e+03 5.667000e+03 5.667000e+03
## nbr.null     0.000000e+00 0.000000e+00 0.000000e+00
## nbr.na       0.000000e+00 0.000000e+00 0.000000e+00
## min          1.095900e+04 8.371000e+03 2.000000e+00
## max          1.932000e+04 1.307760e+05 2.650000e+01
## range        8.361000e+03 1.224050e+05 2.450000e+01
## sum          8.578703e+07 3.135702e+08 6.981100e+04
## median       1.513800e+04 5.448600e+04 1.200000e+01
## mean         1.513800e+04 5.533266e+04 1.231886e+01
## SE.mean      3.210008e+01 4.057237e+02 6.881799e-02
## CI.mean.0.95 6.292844e+01 7.953737e+02 1.349096e-01
## var          5.839363e+06 9.328546e+08 2.683843e+01
## std.dev      2.416477e+03 3.054267e+04 5.180582e+00
## coef.var     1.596299e-01 5.519827e-01 4.205406e-01
```



```r
cor(df_bvsp_selic$price.close, df_bvsp_selic$selic.tax, method = c("pearson", "kendall", "spearman"))
```

```
## [1] -0.8312603
```


```r
cor.test(df_bvsp_selic$price.close, df_bvsp_selic$selic.tax, method=c("pearson", "kendall", "spearman"))
```

```
## 
## 	Pearson's product-moment correlation
## 
## data:  df_bvsp_selic$price.close and df_bvsp_selic$selic.tax
## t = -112.55, df = 5665, p-value < 2.2e-16
## alternative hypothesis: true correlation is not equal to 0
## 95 percent confidence interval:
##  -0.8391354 -0.8230367
## sample estimates:
##        cor 
## -0.8312603
```


```r
ccf(df_bvsp_selic$price.close, df_bvsp_selic$selic.tax)
```

<img src="04-final-work_files/figure-html/unnamed-chunk-17-1.png" width="672" />


```r
# Quick display of two cabapilities of GGally, to assess the distribution and correlation of variables 
library(GGally)
```

```
## Registered S3 method overwritten by 'GGally':
##   method from   
##   +.gg   ggplot2
```

```r
# Nice visualization of correlations
ggcorr(df_bvsp_selic, method = c("pairwise", "pearson"), label = TRUE, label_alpha = TRUE) 
```

```
## Warning in ggcorr(df_bvsp_selic, method = c("pairwise", "pearson"), label =
## TRUE, : data in column(s) 'ref.date' are not numeric and were ignored
```

<img src="04-final-work_files/figure-html/unnamed-chunk-18-1.png" width="672" />


```r
ggpairs(df_bvsp_selic, columns = 2:3) 
```

<img src="04-final-work_files/figure-html/unnamed-chunk-19-1.png" width="672" />

## Expectativas do COPOM nos contratos de DI1

Ref: http://wilsonfreitas.github.io/posts/expectativas-do-copom-nos-contratos-de-di1.html

TODO


## Conclusão

TODO

A partir dessas informações, mais as variações do Índice Bovespa, formou-se o banco de dados que respaldou o estudo. Entre as conclusões mais importantes destaca-se a de que o mercado acionário reage fracamente às variações diretas na meta da taxa básica de juro. Os agentes têm a capacidade de se antecipar às decisões do Banco Central. Porém, a pesquisa também observou que pode haver uma resposta relevante às variações inesperadas (tal como previsto pela hipótese da eficiência informacional dos mercados). Cada ponto percentual de incremento não esperado na meta da taxa básica pode estar associado a uma queda média de 1,3% do Índice Bovespa. Outro resultado interessante é que eventos de ruptura econômica parecem não interferir de modo robusto no padrão de respostas às surpresas.
 
Os autores ressaltam que os resultados originaram-se de um estudo essencialmente empírico, de modo que é preciso desenvolver modelos que abordem não apenas as variáveis e a formação dos preços, mas também investiguem outras implicações e desdobramentos teóricos.
 
Os autores também destacaram que a pesquisa parte da premissa de que a política monetária é exógena em relação aos mercados. Os mercados reagem às decisões do COPOM, e não o oposto. É preciso considerar a possibilidade de eventuais ocorrências de simultaneidade nas respostas de ambos, governo e mercados, a determinados eventos ou tendências verificadas na economia.

## Referências

CASTRO, F. Henrique et al. Fifty-year History of the Ibovespa. **Brazilian Review of Finance**, v. 17, n. 3, p. 47-65, 2019.

DE SOUZA GRÔPPO, Gustavo. Co-integração e causalidade entre variáveis de política monetária e Ibovespa. **Revista de Economia e Administração**, v. 4, n. 2, 2005.

DOS SANTOS, Fernando Siqueira; PRADO, Roberto RA. Causalidade Selic-Ibovespa revisada. **Revista de Economia e Administração**, v. 5, n. 1, 2006.

GONÇALVES JUNIOR, Walter. **Surpresas com relação à política monetária e o mercado de capitais: Evidências do caso brasileiro**. 2007. Tese de Doutorado.

LUCAS, Robert E.; SARGENT, Thomas. After Keynesian Macroeconomics. In: Miller, P. J. (org.) **The Rational Expectations Revolutions**: Readings from the Front Line. [S.1]: Massachussetts Institute of Technology, p.05-30, 1996.

YOSHIDA, Olga Satomi. **BIGDATA ANALITYCS**: data 22-11-2022. São Paulo: Instituto de Pesquisas Tecnológicas do Estado de São Paulo, 2022. 4 slides, color.

WIKIPEDIA. Lista de crises econômicas no Brasil. Disponível em: https://pt.wikipedia.org/wiki/Lista_de_crises_econ%C3%B4micas_no_Brasil. Acesso em: 03 dez. 2022.