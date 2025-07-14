
***************************************************************************
**    STAGE 1: Linear models with Fixed efect, FD-IV, one/two-step GMM   **
***************************************************************************

/* the following codes use dataset_1; the difference between dataset_1 and 
dataset_2 is that dataset_2 is with filling-ins of mean for omitted variables.*/

* change the path
cd D:\zhdata\tatat
import excel "SP500data.xlsx", firstrow clear


** Variables Generation **
    
gen TOTSTK=log(TOT_STK_AWARDS_GIVEN_TO_EXECS)

gen TOTTOPSTK=log(TOT_TOP_3_HIGHEST_STK_AW_AMT)

gen SCDSTK=log(X2ND_HIGHEST_STK_AWARDS_AMT)

gen SCDOTH=log(X2ND_HIGHEST_ALL_OTH_COMP_AMT_AW)

gen TRDOTH=log(X3RD_HIGHEST_ALL_OTH_COMP_AMT_AW)

gen SCDTOT=log(X2ND_HIGHEST_TOT_COMP_AMT_AW)

gen risk= TD/TA

rename SUSTAINALYTICS_SOCIAL_PERCENTILE SOCIAL

rename SUSTAINALYTICS_ENVIRONMENT_PCT ENVIRONMENT

rename NET_INCOME NI

rename SALES_REV_TURN SALESRM

rename CF_FREE_CASH_FLOW FCF

tab GICSSector, gen(sector)

tab State, gen(area)


** Panel Data **

egen ID = group(Ticker)
tab Year, gen(yr)
sort ID Year

xtset ID Year
xtdes

gen Green=1 if ENVIRONMENT>=50
replace Green=0 if ENVIRONMENT<50

** Specification 1: All tickers **

/////2SLS (Anderson-Hsiao) =====================================================

//Regressions & Tests Overidentification. Reject the model if P<0.05.

// Regressions With year dummies
xtivreg ROE (l1.ROE=l2.ROE) ENVIRONMENT SOCIAL SALESRM risk TA FCF TD yr4, nocons fd vce(cluster State) // R-sq==0.1753
est store FDIV_ROE

xtoverid //Instruments are exogeneous.

xtivreg ROA (l1.ROA=l2.ROA) ENVIRONMENT SOCIAL SALESRM risk TA FCF TD yr4, nocons fd vce(cluster State) // R-sq==0.0679
est store FDIV_ROA

xtoverid //Instruments are exogeneous.

xtivreg ROC (l1.ROC=l2.ROC) ENVIRONMENT SOCIAL SALESRM risk TA FCF TD yr4, nocons fd vce(cluster State) // R-sq==0.0140
est store FDIV_ROC

xtoverid //Instruments are exogeneous.

*(NI as a regressor regression on NIStockholders)*
xtivreg NIStockholders (l1.NIStockholders=l2.NIStockholders) ENVIRONMENT SOCIAL SALESRM risk TA NI FCF TD yr4, nocons fd vce(cluster State) // R-sq==0.6852
est store FDIV_NIStockholders

xtoverid //Instruments are exogeneous.

esttab FDIV_ROE FDIV_ROA FDIV_ROC FDIV_NIStockholders, star(* 0.10 ** 0.05 *** 0.01) scalars(r2_w) se ar2
esttab FDIV_ROE FDIV_ROA FDIV_ROC FDIV_NIStockholders using "D:\zhdata\tatat\FDIV.tex", star(* 0.10 ** 0.05 *** 0.01) scalars(r2_w) se ar2 replace

/*
*-------------- The regressions above equivalent to the following -------------*
//1 With year dummies
ivregress 2sls d.ROE (l1.d.ROE=l2.d.ROE) d.ENVIRONMENT d.SOCIAL d.SALESRM d.risk d.TA d.FCF d.TD yr4 , nocons cluster(State)

ivregress 2sls d.ROA (l1.d.ROA=l2.d.ROA) d.ENVIRONMENT d.SOCIAL d.SALESRM d.risk d.TA d.FCF d.TD yr4 , nocons cluster(State)

ivregress 2sls d.ROC (l1.d.ROC=l2.d.ROC) d.ENVIRONMENT d.SOCIAL d.SALESRM d.risk d.TA d.FCF d.TD yr4 , nocons cluster(State)

ivregress 2sls d.NIStockholders (l1.d.NIStockholders=l2.d.NIStockholders) d. ENVIRONMENT d.SOCIAL d.SALESRM d.risk d.TA d.NI d.FCF d.TD yr4, nocons cluster(State)

//2 With year dummies and sector dummies
ivregress 2sls d.ROE (l1.d.ROE=l2.d.ROE) d.ENVIRONMENT d.SOCIAL d.SALESRM d.risk d.TA d.FCF d.TD yr4 sector1-sector13 sector15, nocons cluster(State)

ivregress 2sls d.ROA (l1.d.ROA=l2.d.ROA) d.ENVIRONMENT d.SOCIAL d.SALESRM d.risk d.TA d.FCF d.TD yr4 sector1-sector13 sector15, nocons cluster(State)

ivregress 2sls d.ROC (l1.d.ROC=l2.d.ROC) d.ENVIRONMENT d.SOCIAL d.SALESRM d.risk d.TA d.FCF d.TD yr4 sector1-sector13 sector15, nocons cluster(State)

ivregress 2sls d.NIStockholders (l1.d.NIStockholders=l2.d.NIStockholders) d.ENVIRONMENT d.SOCIAL d.SALESRM d.risk d.TA d.NI d.FCF d.TD yr4 sector1-sector13 sector15, nocons cluster(State)

*------------------------------------------------------------------------------*
*/

/////One step GMM ==============================================================

// 1. ROE 
xtabond ROE, nocons
estat sargan //We reject the validity of the instruments. [X]
estat abond // No autocorrelation of order 1 [X]

qui xtabond ROE ENVIRONMENT SOCIAL SALESRM risk TA FCF TD yr4, nocons
estat sargan //We reject the validity of the instruments. Need to remove instruments. [X] 0.0004
estat abond // No autocorrelation of order 1 [X] 0.2879

* No solution of removing instruments
xtabond ROE ENVIRONMENT SOCIAL SALESRM risk TA FCF TD yr4, nocons vce(robust)
est store osGMMROE
//Run the regression with INVALID instruments.

// 2. ROA
xtabond ROA, nocons
estat sargan // Instruments valid [√]
estat abond // No autocorrelation of order 1 [X]

qui xtabond ROA ENVIRONMENT SOCIAL SALESRM risk TA FCF TD yr4, nocons
estat sargan //We reject the validity of the instruments. Need to remove instruments.
estat abond // Autocorrelation of order 1 as expected [√]

qui xtabond ROA ENVIRONMENT SOCIAL SALESRM TA FCF TD yr4, nocons //Remove "risk"
estat sargan //Instruments valid [√] 0.3297
estat abond // No autocorrelation of order 1 [X] 0.4806 

xtabond ROA ENVIRONMENT SOCIAL SALESRM TA FCF TD yr4, nocons vce(robust)
est store osGMMROA
//Run the regression with VALID instruments.

// 3. ROC
xtabond ROC, nocons
estat sargan //We reject the validity of the instruments. [X]
estat abond // Autocorrelation of order 1 as expected [√]

qui xtabond ROC ENVIRONMENT SOCIAL SALESRM risk TA FCF TD yr4, nocons
estat sargan //We reject the validity of the instruments. Need to remove instruments.[X]  0.0012
estat abond //Autocorrelation of order 1 as expected [√] 0.0007

// No solution of removing instruments
xtabond ROC ENVIRONMENT SOCIAL SALESRM risk TA FCF TD yr4, nocons vce(robust)
est store osGMMROC
//Run the regression with INVALID instruments.

// 4. NIStockholders
xtabond NIStockholders, nocons
estat sargan //We reject the validity of the instruments. [X]
estat abond // Autocorrelation of order 1 as expected [√]

qui xtabond NIStockholders ENVIRONMENT SOCIAL SALESRM risk TA NI FCF TD yr4, nocons
estat sargan //We reject the validity of the instruments. Need to remove instruments.[X] 0.0001
estat abond //Autocorrelation of order 1 as expected [√] 0.0000 

// No solution of removing instruments
xtabond NIStockholders ENVIRONMENT SOCIAL SALESRM risk TA NI FCF TD yr4, nocons vce(robust)
est store osGMMNIStockholders
//Run the regression with INVALID instruments.

//As can be seen in the footnote of the output, the instruments included are from the second lag to the first observation. Compared to the estimate found using the Anderson and Hsiao estimator, the effect of the first lag becomes significant, and is positive, with a slightly larger magnitude

/////Two step GMM ==============================================================

// 1. ROE
xtabond ROE, nocons twostep
estat sargan // Instruments valid [√]
estat abond // No autocorrelation of order 1 [X]

qui xtabond ROE ENVIRONMENT SOCIAL SALESRM risk TA FCF TD yr4, nocons twostep
estat sargan // Instruments valid [√] 0.3715
estat abond // No autocorrelation of order 1 [X] 0.2501

xtabond ROE ENVIRONMENT SOCIAL SALESRM risk TA FCF TD yr4, nocons twostep vce(robust)
est store tsGMMROE
//Run the regression with VALID instruments.

// 2. ROA
xtabond ROA, nocons twostep
estat sargan // Instruments valid [√]
estat abond // No autocorrelation of order 1 [X]

qui xtabond ROA ENVIRONMENT SOCIAL SALESRM risk TA FCF TD yr4, nocons twostep
estat sargan //We reject the validity of the instruments. Need to to remove instruments.
estat abond // No autocorrelation of order 1 [X]

qui xtabond ROA ENVIRONMENT SOCIAL SALESRM TA FCF TD yr4, nocons twostep //Remove "risk"
estat sargan // Instruments valid [√] 0.8405
estat abond // No autocorrelation of order 1 [X] 0.5524

xtabond ROA ENVIRONMENT SOCIAL SALESRM TA FCF TD yr4, nocons twostep vce(robust)
est store tsGMMROA
//Run the regression with VALID instruments.

// 3. ROC
xtabond ROC, nocons twostep
estat sargan // Instruments valid [√]
estat abond // No autocorrelation of order 1 [X]

qui xtabond ROC ENVIRONMENT SOCIAL SALESRM risk TA FCF TD yr4, nocons twostep
estat sargan // Instruments valid [√] 0.1440
estat abond // No autocorrelation of order 1 [X] 0.9488

xtabond ROC ENVIRONMENT SOCIAL SALESRM risk TA FCF TD yr4, nocons twostep vce(robust)
est store tsGMMROC
//Run the regression with VALID instruments.

// 4. NIStockholders
xtabond NIStockholders, nocons twostep
estat sargan // Instruments valid [√]
estat abond // No autocorrelation of order 1 [X]

qui xtabond NIStockholders ENVIRONMENT SOCIAL SALESRM risk TA NI FCF TD yr4, nocons twostep
estat sargan // Instruments valid [√] 0.3688
estat abond //Autocorrelation of order 1 as expected [√] 0.0043

xtabond NIStockholders ENVIRONMENT SOCIAL SALESRM risk TA NI FCF TD yr4, nocons twostep vce(robust)
est store tsGMMNIStockholders
//Run the regression with VALID instruments.


/////Consider the some variables as being weakly exogenous.  ===================

// All other covariates are treated as strictly exogenous.

// 1. ROE

* One-Step GMM
xtabond ROE ENVIRONMENT SOCIAL SALESRM risk FCF TD yr4, nocons pre(TA) vce(robust)
est store osGMMpROE
qui xtabond ROE ENVIRONMENT SOCIAL SALESRM risk FCF TD yr4, nocons pre(TA)
estat sargan //We reject the validity of the instruments. [X] 0.0130
estat abond  // No autocorrelation of order 1 [X] 0.2859 

* Two-Step GMM
xtabond ROE ENVIRONMENT SOCIAL SALESRM risk FCF TD yr4, nocons pre(TA) twostep vce(robust)
est store tsGMMpROE
qui xtabond ROE ENVIRONMENT SOCIAL SALESRM risk FCF TD yr4, nocons pre(TA) twostep 
estat sargan // Instruments valid [√] 0.7459
estat abond  // No autocorrelation of order 1 [X] 0.2741

// 2. ROA

* One-Step GMM
xtabond ROA ENVIRONMENT SOCIAL SALESRM FCF TD yr4, nocons pre(TA) vce(robust)
est store osGMMpROA
qui xtabond ROA ENVIRONMENT SOCIAL SALESRM FCF TD yr4, nocons pre(TA)
estat sargan // Instruments valid [√] 0.6677
estat abond  // No autocorrelation of order 1 [X] 0.4632 

* Two-Step GMM
xtabond ROA ENVIRONMENT SOCIAL SALESRM TD yr4, nocons pre(TA) twostep vce(robust)
est store tsGMMpROA
qui xtabond ROA ENVIRONMENT SOCIAL SALESRM TD yr4, nocons pre(TA) twostep
estat sargan // Instruments valid [√] 0.1215
estat abond  // No autocorrelation of order 1 [X] 0.4019

// 3. ROC

* One-Step GMM
xtabond ROC ENVIRONMENT SOCIAL SALESRM risk FCF TD yr4, nocons pre(TA) vce(robust)
est store osGMMpROC
qui xtabond ROC ENVIRONMENT SOCIAL SALESRM risk FCF TD yr4, nocons pre(TA)
estat sargan //We reject the validity of the instruments. [X] 0.0313
estat abond  // No autocorrelation of order 1 [√] 0.0004

* Two-Step GMM
xtabond ROC ENVIRONMENT SOCIAL SALESRM risk FCF TD yr4, nocons pre(TA) twostep vce(robust)
est store tsGMMpROC
qui xtabond ROC ENVIRONMENT SOCIAL SALESRM risk FCF TD yr4, nocons pre(TA) twostep
estat sargan // Instruments valid [√] 0.1048
estat abond  // No autocorrelation of order 1 [X] 0.4124

// 4. NIStockholders

* One-Step GMM
xtabond NIStockholders ENVIRONMENT SOCIAL SALESRM risk NI FCF TD yr4, nocons pre(TA) vce(robust)
est store osGMMpNIStockholders
qui xtabond NIStockholders ENVIRONMENT SOCIAL SALESRM risk NI FCF TD yr4, nocons pre(TA) 
estat sargan //We reject the validity of the instruments. [X] 0.0000
estat abond  //Autocorrelation of order 1 as expected [√] 0.0000 

* Two-Step GMM
xtabond NIStockholders ENVIRONMENT SOCIAL SALESRM risk NI FCF TD yr4, nocons pre(TA) twostep vce(robust)
est store tsGMMpNIStockholders
qui xtabond NIStockholders ENVIRONMENT SOCIAL SALESRM risk NI FCF TD yr4, nocons pre(TA) twostep
estat sargan // Instruments valid [√] 0.3733
estat abond  //Autocorrelation of order 1 as expected [√] 0.0046 


esttab osGMMROE osGMMROA osGMMROC osGMMNIStockholders tsGMMROE tsGMMROA tsGMMROC tsGMMNIStockholders, star(* 0.10 ** 0.05 *** 0.01) scalars(r2_w) se ar2
esttab osGMMROE osGMMROA osGMMROC osGMMNIStockholders tsGMMROE tsGMMROA tsGMMROC tsGMMNIStockholders using "D:\zhdata\tatat\GMMs.tex", star(* 0.10 ** 0.05 *** 0.01) scalars(r2_w) se ar2 replace

esttab osGMMpROE osGMMpROA osGMMpROC osGMMpNIStockholders tsGMMpROE tsGMMpROA tsGMMpROC tsGMMpNIStockholders, star(* 0.10 ** 0.05 *** 0.01) scalars(r2_w) se ar2
esttab osGMMpROE osGMMpROA osGMMpROC osGMMpNIStockholders tsGMMpROE tsGMMpROA tsGMMpROC tsGMMpNIStockholders using "D:\zhdata\tatat\GMMw.tex", star(* 0.10 ** 0.05 *** 0.01) scalars(r2_w) se ar2 replace



***************************************************************************
**    STAGE 2: Quadratic models with FD-IV for Green / Non-Green Tickers **
***************************************************************************

gen ENVIRONMENT2=(ENVIRONMENT)^2
gen SOCIAL2=(SOCIAL)^2


/////2SLS (Anderson-Hsiao) =====================================================

// 1. All tickers with squared forms

//Regressions & Tests Overidentification with squared form. Reject the model if P<0.05.

xtivreg ROE (l1.ROE=l2.ROE) ENVIRONMENT ENVIRONMENT2 SOCIAL SOCIAL2 SALESRM risk TA FCF TD yr4, nocons fd vce(cluster State) // R-sq==0.1748
est store quadrROE

xtoverid //Instruments are exogeneous.

xtivreg ROA (l1.ROA=l2.ROA) ENVIRONMENT ENVIRONMENT2 SOCIAL SOCIAL2 SALESRM risk TA FCF TD yr4, nocons fd vce(cluster State) // R-sq==0.0466
est store quadrROA

xtoverid //Instruments are exogeneous.

xtivreg ROC (l1.ROC=l2.ROC) ENVIRONMENT ENVIRONMENT2 SOCIAL SOCIAL2 SALESRM risk TA FCF TD yr4, nocons fd vce(cluster State) // R-sq==0.0181
est store quadrROC

xtoverid //Instruments are exogeneous.

*(NI as a regressor regression on NIStockholders)*
xtivreg NIStockholders (l1.NIStockholders=l2.NIStockholders) ENVIRONMENT ENVIRONMENT2 SOCIAL SOCIAL2 SALESRM risk TA NI FCF TD yr4, nocons fd vce(cluster State) // R-sq==0.6929
est store quadrNIStockholders

xtoverid //Instruments are exogeneous.

esttab quadrROE quadrROA quadrROC quadrNIStockholders, star(* 0.10 ** 0.05 *** 0.01) scalars(r2_w) se ar2
esttab quadrROE quadrROA quadrROC quadrNIStockholders using "D:\zhdata\tatat\ALLsq.tex", star(* 0.10 ** 0.05 *** 0.01) scalars(r2_w) se ar2 replace


/*
*-------------- The regressions above equivalent to the following -------------*
//1 With year dummies
ivregress 2sls d.ROE (l1.d.ROE=l2.d.ROE) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.FCF d.TD yr4, nocons cluster(State)

ivregress 2sls d.ROA (l1.d.ROA=l2.d.ROA) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.FCF d.TD yr4, nocons cluster(State)

ivregress 2sls d.ROC (l1.d.ROC=l2.d.ROC) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.FCF d.TD yr4, nocons cluster(State)

ivregress 2sls d.NIStockholders (l1.d.NIStockholders=l2.d.NIStockholders) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.NI d.FCF d.TD yr4, nocons cluster(State)

//2 With year dummies and sector dummies
ivregress 2sls d.ROE (l1.d.ROE=l2.d.ROE) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.FCF d.TD yr4 sector1-sector13 sector15, nocons cluster(State)

ivregress 2sls d.ROA (l1.d.ROA=l2.d.ROA) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.FCF d.TD yr4 sector1-sector13 sector15, nocons cluster(State)

ivregress 2sls d.ROC (l1.d.ROC=l2.d.ROC) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.FCF d.TD yr4 sector1-sector13 sector15, nocons cluster(State)

ivregress 2sls d.NIStockholders (l1.d.NIStockholders=l2.d.NIStockholders) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.NI d.FCF d.TD yr4 sector1-sector13 sector15, nocons cluster(State)

*------------------------------------------------------------------------------*
*/

// 2. Green

//1 With year dummies
ivregress 2sls d.ROE (l1.d.ROE=l2.d.ROE) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.FCF d.TD yr4 if Green==1, nocons cluster(State)
est store quadrROEgreen
//MSE==.46335

ivregress 2sls d.ROA (l1.d.ROA=l2.d.ROA) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.FCF d.TD yr4 if Green==1, nocons cluster(State)
est store quadrROAgreen
//MSE==.09416

ivregress 2sls d.ROC (l1.d.ROC=l2.d.ROC) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.FCF d.TD yr4 if Green==1, nocons cluster(State)
est store quadrROCgreen
//MSE==.29101

ivregress 2sls d.NIStockholders (l1.d.NIStockholders=l2.d.NIStockholders) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.NI d.FCF d.TD yr4 if Green==1, nocons cluster(State)
est store quadrNISgreen
//MSE==1720.2

//2 With year dummies and sector dummies
ivregress 2sls d.ROE (l1.d.ROE=l2.d.ROE) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.FCF d.TD yr4 sector1-sector13 sector15 if Green==1, nocons cluster(State)

ivregress 2sls d.ROA (l1.d.ROA=l2.d.ROA) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.FCF d.TD yr4 sector1-sector13 sector15 if Green==1, nocons cluster(State)

ivregress 2sls d.ROC (l1.d.ROC=l2.d.ROC) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.FCF d.TD yr4 sector1-sector13 sector15 if Green==1, nocons cluster(State)

ivregress 2sls d.NIStockholders (l1.d.NIStockholders=l2.d.NIStockholders) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.NI d.FCF d.TD yr4 sector1-sector13 sector15 if Green==1, nocons cluster(State)

// 3. Non-Green

//1 With year dummies
ivregress 2sls d.ROE (l1.d.ROE=l2.d.ROE) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.FCF d.TD yr4 if Green==0, nocons cluster(State)
est store quadrROEno
//MSE==2.6831

ivregress 2sls d.ROA (l1.d.ROA=l2.d.ROA) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.FCF d.TD yr4 if Green==0, nocons cluster(State)
est store quadrROAno
//MSE==.05178

ivregress 2sls d.ROC (l1.d.ROC=l2.d.ROC) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.FCF d.TD yr4 if Green==0, nocons cluster(State)
est store quadrROCno
//MSE==.05499

ivregress 2sls d.NIStockholders (l1.d.NIStockholders=l2.d.NIStockholders) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.NI d.FCF d.TD yr4 if Green==0, nocons cluster(State)
est store quadrNISno
//MSE==1012

//2 With year dummies and sector dummies
ivregress 2sls d.ROE (l1.d.ROE=l2.d.ROE) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.FCF d.TD yr4 sector1-sector13 sector15 if Green==0, nocons cluster(State)

ivregress 2sls d.ROA (l1.d.ROA=l2.d.ROA) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.FCF d.TD yr4 sector1-sector13 sector15 if Green==0, nocons cluster(State)

ivregress 2sls d.ROC (l1.d.ROC=l2.d.ROC) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.FCF d.TD yr4 sector1-sector13 sector15 if Green==0, nocons cluster(State)

ivregress 2sls d.NIStockholders (l1.d.NIStockholders=l2.d.NIStockholders) d.ENVIRONMENT d.ENVIRONMENT2 d.SOCIAL d.SOCIAL2 d.SALESRM d.risk d.TA d.NI d.FCF d.TD yr4 sector1-sector13 sector15 if Green==0, nocons cluster(State)

esttab quadrROEgreen quadrROAgreen quadrROCgreen quadrNISgreen quadrROEno quadrROAno quadrROCno quadrNISno, star(* 0.10 ** 0.05 *** 0.01) scalars(r2_w) se ar2
esttab quadrROEgreen quadrROAgreen quadrROCgreen quadrNISgreen quadrROEno quadrROAno quadrROCno quadrNISno using "D:\zhdata\tatat\green.tex", star(* 0.10 ** 0.05 *** 0.01) scalars(r2_w) se ar2 replace


*================================== END ========================================
