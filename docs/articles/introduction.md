# Seed germination process

## Seed germination process

The physiology and seed technology have provided valuable tools for the
production of high quality seed and treatments and storage conditions
([Marcos-Filho, 1998](#ref-Marcos-Filho1998)). In basic research, the
seeds are studied exhaustively, and the approach of its biology is
performed to fully exploit the dormancy and germination ([Penfield &
King, 2009](#ref-Penfield2009)). An important tool for indicate the
performance of a seed lot is the precise quantification of germination
through accurate analysis of the cumulative germination data ([Joosen et
al., 2009](#ref-Joosen2010)). Time, velocity, homogeneity, uncertainty
and synchrony are measurements that inform the dynamics of the
germination process. It is interesting not only for physiologists and
seed technologists, but also for environmentalists, since it is possible
to predict the degree of success of the species, based on the seed crop
ability to redistribute germination over time, allowing the recruitment
of part of the seedlings formed ([Ranal & Santana,
2006](#ref-Ranal2006)).

## Seed germination indices

| Variables | abbreviation | units | limits |
|:---|:---|:---|:---|
| Germinated seeds | grs | count | \\0 \leq grs \leq n\_{i}\\ |
| Germinability | grp | % | \\0 \leq grp \leq 100\\ |
| Mean germination time | mgt | time | \\0 \leq mgt \leq k\\ |
| Mean germination rate | mgr | \\time^{-1}\\ | \\0 \< mgr \leq 1\\ |
| Germination speed | gsp | % | \\0 \< gsp \leq 100\\ |
| Uncertainty Index | unc | bit | \\0 \leq unc \leq \log \_{2} n\_{i}\\ |
| Synchronization Index | syn | \- | \\0 \leq syn \leq 1\\ |
| Germination variance | vgt | \\time^2\\ | \\0 \< vgt \leq \infty\\ |
| Germination standard deviation | sdg | time | \\0 \< sdg \leq \infty\\ |
| Coefficient of variation | cvg | % | \\0 \< cvg \leq \infty\\ |

Table 1: Germination variables evaluated in GerminaR package and limits
according to Ranal and Santana (2006), where \\n_i\\ is the number of
seeds germinated in \\i^{th}\\ time; and \\k\\ is the last day of the
evaluation process for germination

> More info: <https://doi.org/10.1111/1440-1703.1275>

## Fieldbook preparation

For a correct analysis and fast data processing is important to take
into account the data organization and the proper record of the
germination process. In this section we will explain how you should
collect and organize your data.

### Data organization

For using GerminaR and GerminaQuant is necessary that you have a dataset
with germination values. You can use a following data as an example
[*“GerminaR”*](https://docs.google.com/spreadsheets/d/1QziIXGOwb8cl3GaARJq6Ez6aU7vND_UHKJnFcAKx0VI/edit?usp=sharing).
If you have a Google account you can made a copy of the document and
edit it online or download in Excel format for your own analysis.

The fieldbook should have three essential parts.(1) The factor columns
(red), according to the experimental design;(2) the seed number column,
indicate the number of seeds sown in each experimental unit (green) and
(3) the evaluation columns with the germination values (blue). You can
design your own field book with different names in the column according
your experimental design.

![Layout for germination evaluation process. The factor column (red) are
according the experimental design. The seed number column (green) for
the number of seed sown and the evaluation columns (blue) for accounting
the germination.](files/dtorg.png)

Figure 1: Layout for germination evaluation process. The factor column
(red) are according the experimental design. The seed number column
(green) for the number of seed sown and the evaluation columns (blue)
for accounting the germination.

### Data collection

The evaluation of the germination process is obtained of the count of
the germination in each experimental unit and It can be evaluated in any
time lapse (hours, days, months, etc) in continuous interval of the same
length always beginning with the time zero (ei. Ti00), until the end of
the germination process or according to the researcher criteria.

## Germination analysis

After the data collection, the information can be processed using
GerminaR by the [R
console](https://germinar.inkaverse.com/articles/GerminaR) or using the
[GerminaQuant](https://germinar.inkaverse.com/articles/GerminaR) app.

## References

Joosen, R. V. L., Kodde, J., Willems, L. A. J., Ligterink, W., Plas, L.
H. W. van der, & Hilhorst, H. W. M. (2009). Germinator: A software
package for high-throughput scoring and curve fitting of *arabidopsis*
seed germination. *The Plant Journal*, *62*(1), 148–159.
<https://doi.org/10.1111/j.1365-313x.2009.04116.x>

Marcos-Filho, J. (1998). New approaches to seed vigor testing. *Scientia
Agricola*, *55*, 27–33.
<https://doi.org/10.1590/S0103-90161998000500005>

Penfield, S., & King, J. (2009). Towards a systems biology approach to
understanding seed dormancy and germination. *Proceedings of the Royal
Society B: Biological Sciences*, *276*(1673), 3561–3569.
<https://doi.org/10.1098/rspb.2009.0592>

Ranal, M. A., & Santana, D. G. de. (2006). How and why to measure the
germination process? *Revista Brasileira de Botânica*, *29*(1), 1–11.
<https://doi.org/10.1590/s0100-84042006000100002>
