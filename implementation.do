********************************************************************************
***ECN 327 Event Study Project**************************************************
***EV Tax Changes on EV Vehicle Stocks******************************************
********************************************************************************

********************************************************************************
***Download data from Yahoo! Finance********************************************
***May 02 2022 to May 01 2023***************************************************
********************************************************************************

********************************************************************************
***PUT TOGETHER DATA FOR ALL DIFFERENT FIRMS************************************
********************************************************************************

cd "D:\ECN327"

// use "TSLA.dta", clear
use "RIVN.dta", clear

// append using "RIVN.dta"

append using "F.dta"

append using "NSANY.dta"


********************************************************************************
***SET DATE AND MERGE ON FF3FM Factors******************************************
********************************************************************************

*Note date is a Stata Date

rename date yahoo_date

* we pulled from CSV, and date is changed, so we did YMD, and change
*  the dashes to slashes

replace yahoo_date = subinstr(yahoo_date, "-", "/", .) 

gen date = date(yahoo_date, "YMD")

merge m:1 date using "GSPC.dta"
* DO THIS

drop if _merge == 2

********************************************************************************
***CREATE EVENT TIME (THIS IS KINDA ANNOYING)************************************
********************************************************************************

*Find event date
*April 17, 2023 day = 241 (implement)

sort date
egen day = group(date)

*April 17, 2023 day = 241 (implement)
gen event_time = day - 241

*Note, if you perform an event study with different events for each firm, you will
*need to do something like this for each event

*gen event_helper = .
*replace event_helper = date - XXXXX if tic =="AAA"
*replace event_helper = date - XXXXX if tic =="BBB"

********************************************************************************
***CREATE EVENT WINDOW AND ESTIMATION WINDOW************************************
********************************************************************************

*Estimation Window to -110 to -10

gen estimation_window = 0
replace estimation_window = 1 if event_time >-110 & event_time<-10

*Event Window +/- 10 days

gen event_window = 0
replace event_window = 1 if event_time >=-10 & event_time<=10

********************************************************************************
***CREATE A UNIQUE FIRM IDENTIFIER and XTSET DATA*******************************
********************************************************************************

egen firm = group(tic)

xtset firm event_time

********************************************************************************
***GENERATE STOCK RETURN********************************************************
********************************************************************************

gen stock_return = ((adjclose - l.adjclose)/l.adjclose)*100

********************************************************************************
***ESTIMATE CAPM MODEL IN ESTIMATION WINDOW FOR EACH FIRM**********************
********************************************************************************

*reg stock_return c.mktrf#i.firm c.smb#i.firm c.hml#i.firm if estimation_window ==1
* We use CAPM pricing because FF3FM data isn't recent enough
reg stock_return c.market_return#i.firm if estimation_window ==1

********************************************************************************
***PREDICT STOCK RETURN, GENERATE ABNORMAL RETURNS******************************
********************************************************************************

predict return_hat if event_window==1

gen ar = stock_return - return_hat

drop if event_time<-10
drop if event_time>10

// twoway (connect ar event_time if tic =="F") ///
// (connect ar event_time if tic =="NSANY") ///
// (connect ar event_time if tic =="TSLA") ///
// (connect ar event_time if tic =="RIVN"), ///
// legend(label(1 "Ford") label(2 "Nissan") label(3 "Tesla") label(4 "Rivian") ) ///
// title("Abnormal Returns by Brand (Implementation)") xtitle("Days Relative to Implementation") ytitle("") yline(0)

tab event_time, sum(ar)

********************************************************************************
***USE EGEN SUM COMMAND TO ACCUMULATE ABNORMAL RETURNS**************************
********************************************************************************
gen Average = 1

egen car_3 = sum(ar) if event_time>=-1 & event_time<=1, by(firm)
label variable car_3 "3-day CAR"

reg car_3 Average if event_time==0, nocons
estimates store model1

egen car_5 = sum(ar) if event_time>=-2 & event_time<=2, by(firm)
label variable car_5 "5-day CAR"

reg car_5 Average if event_time==0, nocons
estimates store model2

egen car_11 = sum(ar) if event_time>=-5 & event_time<=5, by(firm)
label variable car_11 "11-day CAR"

reg car_11 Average if event_time==0, nocons
estimates store model3

esttab model1 model2 model3 using implementation.rtf, label title() replace

********************************************************************************
***CUMULATIVE ABNORMAL RETURNS THAT EVOLVE OVER TIME****************************
********************************************************************************

drop if event_time<-10
drop if event_time>10

egen ccar_1 = sum(ar) if event_time <=-10 , by(firm)
egen ccar_2 = sum(ar) if event_time <=-9 , by(firm)
egen ccar_3 = sum(ar) if event_time <=-8 , by(firm)
egen ccar_4 = sum(ar) if event_time <=-7 , by(firm)
egen ccar_5 = sum(ar) if event_time <=-6 , by(firm)
egen ccar_6 = sum(ar) if event_time <=-5 , by(firm)
egen ccar_7 = sum(ar) if event_time <=-4 , by(firm)
egen ccar_8 = sum(ar) if event_time <=-3 , by(firm)
egen ccar_9 = sum(ar) if event_time <=-2 , by(firm)
egen ccar_10 = sum(ar) if event_time <=-1 , by(firm)
egen ccar_11 = sum(ar) if event_time <=0 , by(firm)
egen ccar_12 = sum(ar) if event_time <=1 , by(firm)
egen ccar_13 = sum(ar) if event_time <=2 , by(firm)
egen ccar_14 = sum(ar) if event_time <=3 , by(firm)
egen ccar_15 = sum(ar) if event_time <=4 , by(firm)
egen ccar_16 = sum(ar) if event_time <=5 , by(firm)
egen ccar_17 = sum(ar) if event_time <=6, by(firm)
egen ccar_18 = sum(ar) if event_time <=7 , by(firm)
egen ccar_19 = sum(ar) if event_time <=8 , by(firm)
egen ccar_20 = sum(ar) if event_time <=9 , by(firm)
egen ccar_21 = sum(ar) if event_time <=10 , by(firm)

*Now put these all together in a single cumulative CAR variable

gen ccar = .
replace ccar = ccar_1 if event_time ==-10
replace ccar = ccar_2 if event_time ==-9
replace ccar = ccar_3 if event_time ==-8
replace ccar = ccar_4 if event_time ==-7
replace ccar = ccar_5 if event_time ==-6
replace ccar = ccar_6 if event_time ==-5
replace ccar = ccar_7 if event_time ==-4
replace ccar = ccar_8 if event_time ==-3
replace ccar = ccar_9 if event_time ==-2
replace ccar = ccar_10 if event_time ==-1
replace ccar = ccar_11 if event_time ==0
replace ccar = ccar_12 if event_time ==1
replace ccar = ccar_13 if event_time ==2
replace ccar = ccar_14 if event_time ==3
replace ccar = ccar_15 if event_time ==4
replace ccar = ccar_16 if event_time ==5
replace ccar = ccar_17 if event_time ==6
replace ccar = ccar_18 if event_time ==7
replace ccar = ccar_19 if event_time ==8
replace ccar = ccar_20 if event_time ==9
replace ccar = ccar_21 if event_time ==10

twoway connect ccar event_time, by(firm)

// twoway (connect ccar event_time if tic =="F") ///
// (connect ccar event_time if tic =="NSANY") ///
// (connect ccar event_time if tic =="TSLA") ///
// (connect ccar event_time if tic =="RIVN")
//
// twoway (connect ccar event_time if tic =="F") ///
// (connect ccar event_time if tic =="NSANY") ///
// (connect ccar event_time if tic =="TSLA") ///
// (connect ccar event_time if tic =="RIVN"), ///
// legend(label(1 "Ford") label(2 "Nissan") label(3 "Tesla") label(4 "Rivian") ) ///
// title("CARS by Brand Starting 10 Days Prior to Implementation") xtitle("Days Relative to Implementation") ytitle("") yline(0)



********************************************************************************
***CUMULATIVE AVERAGE ABNORMAL RETURNS OVER TIME********************************
********************************************************************************

*This generate an indicator variable for each event time

tab event_time, gen(t)

reg ccar t1-t21, nocons

*This command stores estimates from 

gen beta = .
gen ci_high = .
gen ci_low = .
forvalues i = 1(1)21{
replace beta = _b[t`i'] if t`i'==1
replace ci_high = _b[t`i'] + 1.96*_se[t`i'] if t`i'==1
replace ci_low = _b[t`i'] - 1.96*_se[t`i'] if t`i'==1 
}


twoway (rcap ci_high ci_low event_time) (connect beta event_time)  if firm==1, ///
legend(order(2 1) label(1 "95% CI Bands") label(2 "CAARs from t = -10 to t") ) ///
title(Average CARs and 95% Confidence Bands (Implementation)) xtitle("Days Relative to Implementation") 

********************************************************************************
***COMPARING CARs of SOME FIRMS TO OTHERS***************************************
********************************************************************************

gen indicator = 0
replace indicator = 1 if tic == "F"
// replace indicator = 1 if tic == "TSLA"
replace indicator = 1 if tic == "NSANY"

forvalues i = 1(1)21{
    gen t_indicator_`i'= t`i'*indicator
}

reg ccar t1-t21 t_indicator_1-t_indicator_21


gen beta2 = .
gen ci_high2 = .
gen ci_low2 = .
forvalues i = 1(1)21{
replace beta2 = _b[t_indicator_`i'] if t`i'==1
replace ci_high2 = _b[t_indicator_`i'] + 1.96*_se[t_indicator_`i'] if t`i'==1
replace ci_low2 = _b[t_indicator_`i'] - 1.96*_se[t_indicator_`i'] if t`i'==1 
}


twoway (rcap ci_high2 ci_low2 event_time) (connect beta2 event_time)  if firm==1, ///
legend(order(2 1) label(1 "95% CI Bands") label(2 "CAARs from t = -10 to t") ) ///
xtitle("Days Relative to Implementation") title("CARS of Ford and Nissan Relative to Rivian (Implementation)")

