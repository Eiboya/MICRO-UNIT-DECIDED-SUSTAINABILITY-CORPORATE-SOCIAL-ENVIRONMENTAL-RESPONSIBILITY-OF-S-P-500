clear all

* change the path
cd "/Users/kongqingluo/Desktop/Final Project/dataset"


* =========== The following code uses dataset_2 (with fillings) ============== *


***  Outline  ***

/* ==Data==
1 Intro of vars
2 Manipulation and Panel setup */

/* ==Model==
1 Fe(Inconsistent)
2 2SLS (Anderson-Hsiao)
	2.1 With year dummies
	2.2 With year dummies and sector dummies
	2.3 Testing for overidentification
3 One-Step GMM
4 Two-Step GMM 

5 With Square
	5.1 Unitroot testing
	5.2 Quasi-Maximum Likelihood 
	5.3 Model Comparison*/

/* ==Weighted 2SLS on Sector==
1 generate baron weight
2 W2SLS */

***   Data   ***

* ------------- Intro of vars -------------- *

rename (CF_FREE_CASH_FLOW SUSTAINALYTICS_SOCIAL_PERCENTILE ///
SUSTAINALYTICS_ENVIRONMENT_PCT) (fcf social environment)
rename SALES_REV_TURN SRT

* ------------- Manipulat and Panel setup -------------- *

* unbalanced data to balanced one

egen ID = group(Ticker)
tab Year, gen(yr)
tab GICSSector, gen(sector)
tab State, gen(area)
sort ID Year

by ID: gen nyear=[_N]
keep if nyear==4 

xtset ID Year
xtdes


gen RISK = TD/TA
gen sqenvironment = environment^2
gen sqsocial = social^2

foreach v in ROC ROA ROE environment social RISK TA fcf TD{
	gen l`v' = log(`v')
}


save QML.dta, replace

* ------------- Unit Root Testing -------------- *
xtunitroot fisher ROA, dfuller lags(1)
xtunitroot fisher ROC, dfuller lags(1)
xtunitroot fisher ROE, dfuller lags(1)
xtunitroot fisher environment, dfuller lags(1)
xtunitroot fisher social, dfuller lags(1)
xtunitroot fisher TA, dfuller lags(1)


* QML, baseline dynamic fixed effect model:
* ssc install xtdpdqml

xtdpdqml ROA environment sqenvironment social sqsocial ///
RISK TA fcf TD SRT yr2-yr4, nolog
xtdpdqml ROE environment sqenvironment social sqsocial ///
RISK TA fcf TD SRT yr2-yr4, nolog
xtdpdqml ROC environment sqenvironment social sqsocial ///
RISK TA fcf TD SRT yr2-yr4, nolog

* --------------- QML and LR tests --------------- *

*  ROA, fe, twice difference model, with square terms:
xtdpdqml d.ROA d.environment d.sqenvironment d.social d.sqsocial ///
d.RISK d.TA d.fcf d.TD yr2-yr4, mlparams nolog
eststo a_fe
qui xtdpdqml d.ROA d.environment d.social d.RISK d.TA d.fcf d.TD ///
yr2-yr4, mlparams nolog
lrtest a_fe
/*  cannot reject the null, means that the latter specification, i.e. linear
dynamic model without square terms does not significantly improve 
the goodness of fit. */

*  ROE, fe, twice difference model, with square terms:
xtdpdqml d.ROE d.environment d.social d.sqsocial d.sqenvironment ///
d.RISK d.TA d.fcf d.TD yr2-yr4, mlparams nolog
eststo e_fe
qui xtdpdqml d.ROE d.environment d.social d.RISK d.TA d.fcf d.TD ///
yr2-yr4, mlparams nolog
lrtest e_fe
/*  cannot reject the null, means that the latter specification, i.e. linear
dynamic model without square terms does not significantly 
improve the goodness of fit. */

*  ROC, fe, difference model, with square terms:
xtdpdqml d.ROC d.environment d.social d.sqsocial d.sqenvironment ///
d.RISK d.TA d.fcf d.TD yr2-yr4, mlparams nolog
eststo c_fe
qui xtdpdqml d.ROC d.environment d.social d.RISK d.TA d.fcf d.TD ///
yr2-yr4, mlparams nolog
lrtest c_fe
/*  cannot reject the null, means that the latter specification, i.e. linear
dynamic model without square terms does not significantly 
improve the goodness of fit. */

xtdpdqml d.NIStockholders d.environment d.social d.sqsocial d.sqenvironment ///
d.RISK d.TA d.fcf d.TD yr2-yr4, mlparams nolog
eststo nih_fe
qui xtdpdqml d.NIStockholders d.environment d.social d.RISK d.TA d.fcf d.TD ///
yr2-yr4, mlparams nolog
lrtest nih_fe

esttab a_fe a_fe c_fe nih_fe


* ------------ baron weights ---------------- *

*  Energy, sector7
gen baron = 76.45 if sector7 == 1
*  Utilities, sector16
replace baron = 3.87 if sector16 == 1
*  IT, sector11
replace baron = 2.89 if sector11 == 1
*  Industrials, sector10
replace baron = 3.25 if sector10 == 1
*  Materials, sector12
replace baron = 6.76 if sector12 == 1
display (100-(76.45+3.87+2.89+3.25+6.76))/(16-5)
*  Other sectors:
replace baron = 0.61636364 if sector1 == 1
replace baron = 0.61636364 if sector2 == 1
replace baron = 0.61636364 if sector3 == 1
replace baron = 0.61636364 if sector4 == 1
replace baron = 0.61636364 if sector5 == 1
replace baron = 0.61636364 if sector6 == 1
replace baron = 0.61636364 if sector8 == 1
replace baron = 0.61636364 if sector9 == 1
replace baron = 0.61636364 if sector13 == 1
replace baron = 0.61636364 if sector14 == 1
replace baron = 0.61636364 if sector15 == 1

* --------------- weighted ---------------- *

xtreg ROE d.ROE environment sqenvironment social sqsocial fcf AT TA RISK SRT [aweight = baron], fe r
xtreg ROA d.ROA environment sqenvironment social sqsocial fcf AT TA RISK SRT [aweight = baron], fe r
xtreg ROC d.ROC environment sqenvironment social sqsocial fcf AT TA RISK SRT [aweight = baron], fe r
xtreg NIStockholders d.NIStockholders environment sqenvironment social sqsocial fcf AT TA RISK SRT [aweight = baron], fe r


* --------------- Nonparametric Fitting --------- *
npregress kernel ROE environment social RISK TA fcf TD, vce(bootstrap, reps(5) seed(0106))
eststo np_e
margins, at(environment=generate(environment)) at(environment=generate(environment*1.1)) contrast(atcontrast(r) nowald) vce(bootstrap, reps(5) seed(0106))
margins, at(environment=(10(5)90))
marginsplot, title("ROE fitting")
graph save ROE.png, replace

npregress kernel ROA environment social RISK TA fcf TD, vce(bootstrap, reps(5) seed(0106))
eststo np_a
margins, at(environment=generate(environment)) at(environment=generate(environment*1.1)) contrast(atcontrast(r) nowald) vce(bootstrap, reps(5) seed(0106))
margins, at(environment=(10(5)90))
marginsplot, title("ROA fitting")
graph save ROA.png, replace

npregress kernel ROC environment social RISK TA fcf TD, vce(bootstrap, reps(5) seed(0106))
eststo np_c
margins, at(environment=generate(environment)) at(environment=generate(environment*1.1)) contrast(atcontrast(r) nowald) vce(bootstrap, reps(5) seed(0106))
margins, at(environment=(10(5)90))
marginsplot, title("ROC fitting")
graph save ROC.png, replace

npregress kernel NIStockholders environment social RISK TA fcf TD, vce(bootstrap, reps(5) seed(0106))
eststo np_nis
margins, at(environment=generate(environment)) at(environment=generate(environment*1.1)) contrast(atcontrast(r) nowald) vce(bootstrap, reps(5) seed(0106))
margins, at(environment=(10(5)90))
marginsplot, title("NIStockholders fitting")
graph save NIStockholders.png, replace



