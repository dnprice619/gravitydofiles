*Gravity project Actual Analysis
*5/19/17

*real analysis

clear

set more off

cd "/Users/davidprice/Documents/q3 2017/Topics in International Trade/Gravity Data"

use datagravityproject.dta

rename govtype_o govtype_od
rename govtype_d govtype_o
rename govtype_od govtype_d

*CREATE GRAVITY REGRESSION VARIABLES
gen gatt_both=gatt_o*gatt_d
gen lnv=log(trade/pce)
gen lny=log(gdp_d*gdp_o/pce)
gen lnd=log(dist)
gen lng=lny-lnd

*CREATE YEAR DUMMIES
tab year, gen(yr)

*CREATE COUNTRY PAIR DUMMIES
gen impexp=iso_d+iso_o

*CREATE IMPORTER AND EXPORTER DUMMIES
tab iso_o, gen(ee)
tab iso_d, gen(ii)

*CREATE IMPORTER AND EXPORTER TIME TRENDS AND QUADRATIC TIME TRENDS
foreach x of numlist 1/95 {
	gen yii`x'=ii`x'*year
	gen y2ii`x'=ii`x'*(year^2)
	}

	*CREATE IMPORTER AND EXPORTER TIME TRENDS AND QUADRATIC TIME TRENDS
foreach x of numlist 1/94 {
	gen yee`x'=ee`x'*year
	gen y2ee`x'=ee`x'*(year^2)
	}

*government type explanations
*govt type 1 --> full autocracy
*govt type 2 --> closed anoncracy
*govt type 3 --> open anoncracy
*govt type 4 --> democracy
*govt type 5 --> full democracy

*CREATE ID VARIABLES FOR IMPORTER-YEAR AND EXPORTER-YEAR FIXED EFFECTS
gen impyr=iso_d+string(year)
gen expyr=iso_o+string(year)

*straight summary stats for trade etc.
*trade
forvalues x=1/5{
	sum trade if govtype_o==`x'
	}
	forvalues x=1/5{
	sum trade if govtype_d==`x'
	}

*simplified govt type treatment
*binary for the main treatment variable
gen simplegov_d = 0
replace simplegov_d = 1 if govtype_d==1
replace simplegov_d = 2 if govtype_d==2 | govtype_d==3
replace simplegov_d = 3 if govtype_d==4 | govtype_d==5
replace simplegov_d=. if govtype_d==.

gen simplegov_o = 0
replace simplegov_o = 1 if govtype_o==1
replace simplegov_o = 2 if govtype_o==2 | govtype_o==3
replace simplegov_o = 3 if govtype_o==4 | govtype_o==5
replace simplegov_o=. if govtype_o==.

*IMPORTANT EDIT 
*CREATE SIMPLE treatment VARIABLE 
gen demotreat_o = 0
replace demotreat_o = 1 if simplegov_o==3

gen demotreat_d = 0
replace demotreat_d = 1 if simplegov_d==3

gen democracy_o = 0
replace democracy_o=1 if simplegov_o==3

gen partial_d_o = 0
replace partial_d_o = 1 if simplegov_o==2

gen autocracy_o = 0
replace autocracy_o = 1 if simplegov_o==1

*re do graphs with binary variable
preserve
collapse democracy_o partial_d_o autocracy_o, by(name_o year)
egen totaldemoc = sum(democracy_o), by(year)
egen totalautoc = sum(autocracy_o), by(year)
egen totalpart = sum(partial_d_o), by(year)

gen total = totaldemoc+totalautoc+totalpart

gen prodemoc = totaldemoc/total

gen pronondemoc = 1-prodemoc

collapse prodemoc pronondemoc, by(year)

twoway (line prodemoc year)(line pronondemoc year), title(Proportion of Countries Democratic) ///
	subtitle("1964-2014") legend(order(1 "Democratic" 2 "Non Demcoractic")) ytitle(Proportion) ///
	xtitle(Year) xlabel(1964(5)2015)

restore

*trends in the data
*simple proportion of each gov type
preserve
collapse democracy_o partial_d_o autocracy_o, by(name_o year)
egen totaldemoc = sum(democracy_o), by(year)
egen totalautoc = sum(autocracy_o), by(year)
egen totalpart = sum(partial_d_o), by(year)

gen total = totaldemoc+totalautoc+totalpart

*proportion variables
gen prodemoc = totaldemoc/total
gen propart = totalpart/total
gen proauto = totalautoc/total

collapse prodemoc propart proauto,by(year)
twoway (line prodemoc year)(line propart year)(line proauto year), title(Government Ideology Trends) ///
	legend(order(1 "Democracy" 2 "Partial Democracy" 3 "Autocracy")) subtitle(Proportion of Total Governments) ///
	ytitle(Proportion) xtitle(Year) xlabel(1964(5)2015)

restore

*other good ways to show trends?

*by region just for democracy?
*generate region codes
kountry stdname_o, from(other) geo(un)

drop NAMES_STD
rename GEO region_o

kountry stdname_d, from(other) geo(un)
drop NAMES_STD
rename GEO region_d

encode region_o, gen(regioncode_o)

encode region_d, gen(regioncode_d)

br regioncode_d regioncode_o year name_d if regioncode_d==.

rename region_d region_de
rename region_o region_d
rename region_de region_o

rename regioncode_d regioncode_de
rename regioncode_o regioncode_d
rename regioncode_de regioncode_o

*regional analysis

preserve
collapse simplegov_d, by(regioncode_d year)
drop if regioncode==.
twoway (line simplegov_d year if regioncode_d==1) (line simplegov_d year if regioncode_d==2) ///
	 (line simplegov_d year if regioncode_d==3)  (line simplegov_d year if regioncode_d==4) ///
	 (line simplegov_d year if regioncode_d==5, lpattern(dash)), title(Average Governance Type by Region) ///
	 subtitle("From 1964-2014") ytitle(Governance Type) xtitle(Year) xlabel(1964(5)2015) ///
	 legend(order(1 "Africa" 2 "Americas" 3 "Asia" 4 "Europe" 5 "Oceania"))
restore

*Just democratic
preserve
collapse demotreat_d, by(regioncode_d year)
drop if regioncode==.
twoway (line demotreat_d year if regioncode_d==1) (line demotreat_d year if regioncode_d==2) ///
	 (line demotreat_d year if regioncode_d==3)  (line demotreat_d year if regioncode_d==4) ///
	 (line demotreat_d year if regioncode_d==5, lpattern(dash)), title(Proportion of Governments Democractic) ///
	 subtitle("From 1964-2014 by Region" ) ytitle(Proportion) xtitle(Year) xlabel(1964(5)2015) ///
	 legend(order(1 "Africa" 2 "Americas" 3 "Asia" 4 "Europe" 5 "Oceania"))
restore

*trade by govt type
preserve
collapse (mean) lnv, by(simplegov_d year)
twoway (line lnv year if simplegov_d==1)(line lnv year if simplegov_d==2) ///
	(line lnv year if simplegov_d==3), title(Average Yearly Importer Trends in Trade) subtitle(By Governance Type) ///
	legend(order(1 "Autocracy" 2 "Partial Democracy" 3 "Full Democracy")) ///
	ytitle(Log USD Trade) xtitle(Year) xlabel(1964(5)2015)
restore

*just democratic
preserve
collapse (mean) lnv, by(demotreat_d year)
twoway (line lnv year if demotreat_d==1)(line lnv year if demotreat_d==0) , title(Average Yearly Importer Trends in Trade) subtitle(By Governance Type) ///
	legend(order(1 "Democratic" 2 "Non Democratic")) ///
	ytitle(Log USD Trade) xtitle(Year) xlabel(1964(5)2015)
restore

preserve
collapse (mean) lnv, by(demotreat_o year)
twoway (line lnv year if demotreat_o==1)(line lnv year if demotreat_o==0) , title(Average Yearly Exporter Trends in Trade) subtitle(By Governance Type) ///
	legend(order(1 "Democratic" 2 "Non Democratic")) ///
	ytitle(Log USD Trade) xtitle(Year) xlabel(1964(5)2015)
restore

preserve
collapse lnv, by(simplegov_o year)
twoway (line lnv year if simplegov_o==1)(line lnv year if simplegov_o==2) ///
	(line lnv year if simplegov_o==3), title(Average Yearly Exporter Trends in Trade) subtitle(By Governance Type) ///
	legend(order(1 "Autocracy" 2 "Partial Democracy" 3 "Full Democracy")) ///
	ytitle(Log USD Trade) xtitle(Year) xlabel(1964(5)2015)
restore

*preserve
preserve
collapse gdp_d pce, by(name_d govtype_d)
gen realgdp_d = (gdp_d/pce)/1000000
graph bar realgdp_d, over(govtype_d, sort(1)) asyvars ///
	legend(order(1 "Autocracy" 2 "Closed Anocracy" 3 "Open Acroacy" 4 "Democracy" 5 "Full Democracy")) ///
	ytitle(Real GDP) note(GDP in USD in $000000's) title(Average GDP by Regime Type) ///
	subtitle("Averaged Over 1964-2014")
restore

preserve
collapse trade pce, by(name_d govtype_d)
gen realtrade = (trade/pce)/1000000
graph bar realtrade, over(govtype_d,sort(1)) asyvars ///
	legend(order(1 "Autocracy" 2 "Closed Anocracy" 3 "Open Acroacy" 4 "Democracy" 5 "Full Democracy")) ///
	ytitle(Real Trade) note(Trade in USD in $000000's) title(Average Trade Volume by Regime Type) ///
	subtitle("Averaged Over 1964-2014")
restore

preserve
collapse trade pce, by(name_d demotreat_d)
gen realtrade = (trade/pce)/1000000
graph bar realtrade, over(demotreat_d,sort(1)) asyvars ///
	legend(order(1 "Non Democracy" 2 "Democracy")) ///
	ytitle(Real Trade) note(Trade in USD in $000000's) title(Average Trade Volume) ///
	subtitle("1964-2014")
restore

preserve
collapse gdp_d gdp_o pce, by(name_d name_o demotreat_d demotreat_o)
gen realgdp_o = (gdp_o/pce)/1000000
gen realgdp_d = (gdp_d/pce)/1000000

graph bar realgdp_o realgdp_d, over(demotreat_o, sort(1)) ///
	asyvars title(Average Real GDP) legend(order(1 "Non Democracy" 2 "Democracy")) ///
	ytitle(Real GDP) note(GDP in USD in $000000's) ///
	subtitle("1964-2014")
restore

preserve
collapse gdp_d pce, by(year demotreat_d)
gen realgdp_d = (gdp_d/pce)/1000000
twoway (line realgdp_d year if demotreat_d==0)(line realgdp_d year if demotreat_d==1), ///
	legend(order(1 "Non Democracies" 2 "Democracies")) title(Average Real GDP) ///
	ytitle(Real USD GDP) xtitle(Year) xlabel(1964(5)2015) note(GDP in USD in $000000's) ///
	subtitle("1964-2014")

restore

preserve
collapse gdp_d pce [aweight=gdp_d], by(year demotreat_d regioncode_d)
gen realgdp_d = (gdp_d/pce)/1000000

twoway (line realgdp_d year if demotreat==0 & regioncode_d==1) (line realgdp_d year if demotreat==0 & regioncode_d==2) ///
	 (line realgdp_d year if demotreat==0 & regioncode_d==3) (line realgdp_d year if demotreat==0 & regioncode_d==4) ///
	  (line realgdp_d year if demotreat==0 & regioncode_d==5), ytitle(Real USD GDP) xtitle(Year) ///
	  xlabel(1964(5)2015) legend(order(1 "Africa" 2 "Americas" 3 "Asia" 4 "Europe" 5 "Oceania")) note(GDP in USD in $000000's)
restore

*now look at trade between partners if both are autocracies democracies etc.

gen bothauto = 0
replace bothauto =1 if simplegov_o==1 & simplegov_d==1

gen bothpartial = 0
replace bothpartial = 1 if simplegov_o==2 & simplegov_o==2

gen bothdemo = 0
replace bothdemo = 1 if simplegov_d==3 & simplegov_o==3


gen demoauto = 0
replace demoauto=1 if simplegov_d==1 & simplegov_o==3
replace demoauto=1 if simplegov_d==3 & simplegov_d==1


gen demopartial = 0
replace demopartial=1 if simplegov_d==2 & simplegov_o==3
replace demopartial = 1 if simplegov_d==3 & simplegov_d==2


gen partialauto = 0
replace partialauto=1 if simplegov_d==1 & simplegov_o==2
replace partialauto=1 if simplegov_d==2 & simplegov_d==1

preserve

collapse lnv, by(bothdemo bothpartial bothauto year)
twoway (line lnv year if bothdemo==1)(line lnv year if bothpartial==1) ///
	(line lnv year if bothauto==1), xlabel(1964(5)2015) xtitle(Year) ytitle(Log USD Trade) ///
	legend(order(1 "Both Democracies" 2 "Both Partial Democracies" 3 "Both Autocracies")) ///
	title(Trade Patterns Between Like Regime Types) subtitle(Averaged Over Time)
restore

*no china

preserve
drop if name_d=="China"
drop if name_o=="China"
collapse lnv, by(bothdemo bothpartial bothauto year)
twoway (line lnv year if bothdemo==1)(line lnv year if bothpartial==1) ///
	(line lnv year if bothauto==1), xlabel(1964(5)2015) xtitle(Year) ytitle(Log USD Trade) ///
	legend(order(1 "Both Democracies" 2 "Both Partial Democracies" 3 "Both Autocracies")) ///
	title(Trade Patterns Between Like Regime Types) subtitle(Averaged Over Time) note(China excluded from all observations)
restore

preserve
collapse lnv, by(demoauto demopartial partialauto year)
twoway (line lnv year if demoauto==1)(line lnv year if demopartial==1) ///
	(line lnv year if partialauto==1), xlabel(1964(5)2015) xtitle(Year) ytitle(Log USD Trade) ///
	legend(order(1 "Democracy & Autocracy" 2 "Democracy & Partial Democracy" 3 "Partial Democracy & Autocracy")size(small)) ///
	title(Trade Between Differing Regime Types) subtitle(Averaged Over Time)
restore

*count by region
preserve

collapse simplegov_d, by(regioncode_d name_d year)

gen autocracy = 0
replace autocracy = 1 if simplegov_d==1

gen partd = 0
replace partd = 1 if simplegov_d==2

gen demo = 0
replace demo = 1 if simplegov_d==3

collapse (sum) demo partd autocracy, by(regioncode_d year)

gen check = autocracy+partd+demo
tab year check

sort regioncode_d year
list check autocracy demo partd regioncode_d if year==1964

list check autocracy demo partd regioncode_d if year==2014
restore

************************************
*sum gdp by region ************************************
************************************
preserve

drop if year<2010

collapse (mean) gdp_d pce, by(regioncode_d demotreat_d)

gen realgdp = (gdp_d/pce)/10000

tab regioncode_d

forvalues x = 1/5 {
	sum realgdp if regioncode_d==`x' & demotreat_d==0
	}

forvalues x = 1/5 {
	sum realgdp if regioncode_d==`x' & demotreat_d==1
	}
restore

*first graph movement of countries towards democracy
*proportion of total countries that are democracies
*generate variable of change in government

*Sequence of specifications ---> remember to check SE estimation

*no country FE but with year dummies
reg lnv i.demotreat_o lny lnd contig comlang_off colony gatt_both gatt_d fta_wto ///
	i.year, robust
est sto r1
esttab r1
*outreg2 using res1_nofe, append excel

reg lnv i.demotreat_d lny lnd contig comlang_off colony gatt_both gatt_d fta_wto ///
	i.year, robust
est sto r2
esttab r2
*outreg2 using res1_nofe, append excel

reg lnv i.demotreat_d##i.demotreat_o lny lnd contig comlang_off colony gatt_both gatt_d fta_wto ///
	i.year, robust
est sto r3
esttab r3
*outreg2 using res1_nofe, append excel

********************************************************************************
*what is the counterfactual here??????
********************************************************************************


*next sequence: import & exporter fixed effects
*first exporter
reg lnv i.demotreat_o lny lnd contig comlang_off colony gatt_both gatt_d fta_wto ///
	i.year ee* ii*, r
est sto r4
esttab r4
*outreg2 using res2_ee&ii, append excel

reg lnv i.demotreat_d lny lnd contig comlang_off colony gatt_both gatt_d fta_wto ///
	i.year ee* ii*, r
est sto r5
esttab r5
*outreg2 using res2_ee&ii, append excel

reg lnv i.demotreat_d##i.demotreat_o lny lnd contig comlang_off colony gatt_both gatt_d fta_wto ///
	i.year ee* ii*, r
est sto r6
esttab r6
*outreg2 using res2_ee&ii, append excel

********************************************************************************
*what is the counterfactual here??????
********************************************************************************

*same but importer fixed effects
areg lnv i.demotreat_o lny lnd contig comlang_off colony gatt_both gatt_d fta_wto ///
	i.year, absorb(iso_d) r
est sto r7
esttab r7
*outreg2 using res3_importer, append excel

areg lnv i.demotreat_d lny lnd contig comlang_off colony gatt_both gatt_d fta_wto ///
	i.year, absorb(iso_o) r
est sto r8
esttab r8
*outreg2 using res3_importer, append excel

areg lnv i.demotreat_d##i.demotreat_o lny lnd contig comlang_off colony gatt_both gatt_d fta_wto ///
	i.year, absorb(iso_d) r
est sto r9
esttab r9
*outreg2 using res3_importer, append excel
********************************************************************************
*what is the counterfactual here??????
********************************************************************************

*same but country pair fixed effects
areg lnv i.demotreat_o lny lnd contig comlang_off colony gatt_both gatt_d fta_wto ///
	i.year, absorb(impexp) cluster(impexp)
est sto r10
esttab r10
*outreg2 using res4_countrypair, append excel

areg lnv i.demotreat_d lny lnd contig comlang_off colony gatt_both gatt_d fta_wto ///
	i.year, absorb(impexp) cluster(impexp)
est sto r11
esttab r11
*outreg2 using res4_countrypair, append excel

areg lnv i.demotreat_d##i.demotreat_o lny lnd contig comlang_off colony gatt_both gatt_d fta_wto ///
	i.year, absorb(impexp) cluster(impexp)
est sto r12
esttab r12
*outreg2 using res4_countrypair, append excel

*same but country pair fixed effects
areg lnv i.demotreat_o lny i.year, absorb(impexp) cluster(impexp)
est sto r16
esttab r16
*outreg2 using res4_countrypair, append excel

areg lnv i.demotreat_d lny i.year, absorb(impexp) cluster(impexp)
est sto r17
esttab r17
*outreg2 using res4_countrypair, append excel

areg lnv i.demotreat_d##i.demotreat_o lny i.year, absorb(impexp) cluster(impexp)
est sto r18
esttab r18
*outreg2 using res4_countrypair, append excel
********************************************************************************
*what is the counterfactual here??????
********************************************************************************


*MIGHT NOT WANT TO DO TIME TRENDS THEY R F'd
*final model with linear time trends
*same but country pair fixed effects
areg lnv i.demotreat_o lny lnd contig comlang_off colony gatt_both gatt_d fta_wto ///
	i.year yr* yii* yee*, absorb(impexp) cluster(impexp)
est sto r13
esttab r13
*outreg2 using res5_timetrends, append excel

areg lnv i.demotreat_d lny lnd contig comlang_off colony gatt_both gatt_d fta_wto ///
	i.year yr* yii* yee*, absorb(impexp) cluster(impexp)
est sto r14
esttab r14
*outreg2 using res5_timetrends, append excel

areg lnv i.demotreat_d##i.demotreat_o lny lnd contig comlang_off colony gatt_both gatt_d fta_wto ///
	i.year yr* yii* yee*, absorb(impexp) cluster(impexp)
est sto r15
esttab r15
*outreg2 using res5_timetrends, append excel

areg lnv i.demotreat_o lny i.year yr* yii* yee*, absorb(impexp) cluster(impexp)
est sto r19
esttab r19
*outreg2 using res5_timetrends, append excel

areg lnv i.demotreat_d lny i.year yr* yii* yee*, absorb(impexp) cluster(impexp)
est sto r20
esttab r20
*outreg2 using res5_timetrends, append excel

areg lnv i.demotreat_d##i.demotreat_o lny i.year yr* yii* yee*, absorb(impexp) cluster(impexp)
est sto r21
esttab r21
*outreg2 using res5_timetrends, append excel
********************************************************************************
*what is the counterfactual here??????
********************************************************************************

**************************************************************************************************************
**************************************************************************************************************



*prediction over sample with final specification
areg lnv i.demotreat_o##i.demotreat_d lny lnd contig comlang_off colony gatt_both gatt_d fta_wto ///
	i.year yr* yii* yee* if year<2009, absorb(impexp) cluster(impexp)
	predict yhat
	br yhat lnv

areg lnv i.demotreat_d##i.demotreat_o lny lnd contig comlang_off colony gatt_both gatt_d fta_wto ///
	i.year if year<2009, absorb(impexp) cluster(impexp)
	predict yhat2

*Graphs for prediction 
twoway (scatter yhat lnv)(lfit yhat lnv), title(Predicted vs. Observed Trade) ///
	subtitle(Country FE With Linear Trends) ytitle(Predicted Values) xtitle(Observed Values) ///
	legend(order(1 "Log(Real Trade)" 2 "Linear Fit"))


twoway (scatter yhat2 lnv)(lfit yhat2 lnv), title(Predicted vs. Observed Trade) ///
	subtitle(Country FE Without Linear Trends) ytitle(Predicted Values) xtitle(Observed Values) ///
	legend(order(1 "Log(Real Trade)" 2 "Linear Fit"))
	
************************************************************************************
*********************************************************************************************************
*********************************************************************************************************
*********************************************************************************************************
*********************************************************************************************************
************************************************************************************

*Try prediction but over first 5 years of sample instead of last 5 years 
areg lnv i.demotreat_o##i.demotreat_d lny lnd contig comlang_off colony gatt_both gatt_d fta_wto ///
	i.year yr* yii* yee* if year>1969, absorb(impexp) cluster(impexp)
	predict yhat3
	br yhat lnv

areg lnv i.demotreat_d##i.demotreat_o lny lnd contig comlang_off colony gatt_both gatt_d fta_wto ///
	i.year if year>1969, absorb(impexp) cluster(impexp)
	predict yhat4

*linear trends first 5 years 
twoway (scatter yhat3 lnv)(lfit yhat3 lnv), title(Predicted vs. Observed Trade First 5 Years of Sample) ///
	subtitle(Country FE With Linear Trends) ytitle(Predicted Values) xtitle(Observed Values) ///
	legend(order(1 "Log(Real Trade)" 2 "Linear Fit")) note("predicted over first 5 years of sample 1964-1969") 

*no linear trends first 5 years
twoway (scatter yhat4 lnv)(lfit yhat4 lnv), title(Predicted vs. Observed Trade First 5 Years of Sample) ///
	subtitle(Country FE Without Linear Trends) ytitle(Predicted Values) xtitle(Observed Values) ///
	legend(order(1 "Log(Real Trade)" 2 "Linear Fit")) note(predicted over first 5 years of sample) 
	
*show
preserve
collapse yhat lnv, by(name_d)
twoway (lfitci yhat lnv)(scatter yhat lnv)
restore

preserve
collapse yhat2 lnv, by(name_d)
twoway (lfitci yhat2 lnv)(scatter yhat2 lnv)
restore

preserve
collapse yhat2 lnv, by(name_o)
twoway (lfitci yhat2 lnv)(scatter yhat2 lnv), xtitle(Log Observed Trade) ///
	legend(order(1 "95% CI" 2 "Linear Fit" 3 "Average Predicted Value")) ytitle(Log Predicted Trade) ///
	title(Fit Analysis Predicted vs. Observed Trade) subtitle(Exporter Countries) ///
	note(Predicted Values from full model without linear trends)
restore

preserve
collapse yhat2 lnv, by(name_d)
twoway (lfitci yhat2 lnv)(scatter yhat2 lnv), xtitle(Log Observed Trade) ///
	legend(order(1 "95% CI" 2 "Linear Fit" 3 "Average Predicted Value")) ytitle(Log Predicted Trade) ///
	title(Fit Analysis Predicted vs. Observed Trade) subtitle(Importer Countries) ///
	note(Predicted Values from full model without linear trends)
restore

*now over last years

preserve
collapse yhat2 lnv, by(year)
twoway (line yhat2 year if year>=2009)(line lnv year if year>=2009), title(Predicted vs. Observed Trade) ///
	subtitle("Last 5 years of Observations") legend(order(1 "Predicted Values" 2 "Observed Values")) ///
	xtitle(Year) ytitle(Log USD Trade) note(Predicted values from model without linear trends & averaged over all obs.)
restore

*now regime types
preserve
collapse yhat2 lnv, by(year simplegov_d)
drop if year<2009
twoway  (line yhat2 year if simplegov_d==1, lpattern(dash))(line yhat2 year if simplegov_d==2, lpattern(dash)) ///
	(line yhat2 year if simplegov_d==3, lpattern(dash))(line lnv year if simplegov_d==1) ///
	(line lnv year if simplegov_d==2)(line lnv year if simplegov_d==3), ///
	legend(order(1 "Predicted Autocracy" 2 "Predicted Partial Democracy" 3 "Predicted Democracy" 4 "Obs. Autocracy" 5 "Obs. Partial Democracy" 6 "Obs. Democracy")size(vsmall)) ///
	title(Predicted vs. Observed Trade by Regime Type) xtitle(Year) ytitle(Log USD Trade) ///
	note(Predicted Values from model without linear trends)
restore

*mean and kdensity comparisons
twoway(kdensity lnv)(kdensity yhat2) if year>=2009, legend(order(1 "Observed Trade" 2 "Predicted Trade")) ///
	title(K-Density Comparisons) xtitle(Log Trade) ytitle(Density) title(Predicted vs. Observed Trade) ///
	subtitle("Kernal Density Comparison Country FE & No Linear Trends")

twoway(kdensity lnv)(kdensity yhat) if year>=2009, legend(order(1 "Observed Trade" 2 "Predicted Trade")) ///
	title(K-Density Comparisons) xtitle(Log Trade) ytitle(Density) title(Predicted vs. Observed Trade) ///
	subtitle("Kernal Density Comparison Country FE With Linear Trends")

**************************************************************************************************************
**************************************************************************************************************



*prediction fix etc.

*change prediction to first five years
areg lnv i.demotreat_o##i.demotreat_d lny lnd contig comlang_off colony gatt_both gatt_d fta_wto ///
	i.year yr* yii* yee* if year>1969, absorb(impexp) cluster(impexp)
predict yhatnew


*robustness checks

*endogeneity with placebo variable??
*lead lags

*anticipation effect?
*3 lags?

*importer
gen L1demotreat_d = demotreat_d[_n-1] if name_d==name_d[_n-1]

gen L2demotreat_d = demotreat_d[_n-2] if name_d==name_d[_n-2]

gen L3demotreat_d = demotreat_d[_n-3] if name_d==name_d[_n-3]

*exporter
gen L1demotreat_o = demotreat_o[_n-1] if name_o==name_o[_n-1]

gen L2demotreat_o = demotreat_o[_n-2] if name_o==name_o[_n-2]

gen L3demotreat_o = demotreat_o[_n-3] if name_o==name_o[_n-3]

*LeadS
gen Le1demotreat_d = demotreat_d[_n+1] if name_d==name_d[_n+1]

gen Le2demotreat_d = demotreat_d[_n+2] if name_d==name_d[_n+2]

gen Le3demotreat_d = demotreat_d[_n+3] if name_d==name_d[_n+3]

gen Le6demotreat_d = demotreat_d[_n+6] if name_d==name_d[_n+6]

*exporter
gen Le1demotreat_o = demotreat_o[_n+1] if name_o==name_o[_n+1]

gen Le2demotreat_o = demotreat_o[_n+2] if name_o==name_o[_n+2]

gen Le3demotreat_o = demotreat_o[_n+3] if name_o==name_o[_n+3]

gen Le6demotreat_o = demotreat_o[_n+6] if name_o==name_o[_n+6]


areg lnv i.demotreat_o##i.demotreat_d Le6demotreat_o#Le6demotreat_d lny i.year yii* yee*, absorb(impexp) cluster(impexp)


*country pair leads ???

**************************************************************************************************************
**************************************************************************************************************
**************************************************************************************************************
**************************************************************************************************************


*time heterogeneity
*split sample in half???
**and look at time periods

*country pair fe only
areg lnv i.demotreat_d##i.demotreat_o lny i.year if year<1989, absorb(impexp) cluster(impexp)
est sto r31
esttab r31
*outreg2 using timehettable, append excel

areg lnv i.demotreat_d##i.demotreat_o lny i.year if year>=1989, absorb(impexp) cluster(impexp)
est sto r32
esttab r32
*outreg2 using timehettable, append excel

*with linear trends and by sample in half
areg lnv i.demotreat_d##i.demotreat_o lny i.year yr* yii* yee* if year<1989, ///
	absorb(impexp) cluster(impexp)
est sto r33
esttab r33
*outreg2 using timehettable, append excel

areg lnv i.demotreat_d##i.demotreat_o lny i.year yr* yii* yee* if year>=1989, ///
	absorb(impexp) cluster(impexp)
est sto r34
esttab r34
*outreg2 using timehettable, append excel


*Heterogeneity by region 
tab regioncode_d region_d

*using country FE model 
*regions??
forvalues x=1/5{
areg lnv i.demotreat_d##i.demotreat_o lny i.year if regioncode_d==`x', ///
	absorb(impexp) cluster(impexp)
est sto r4`x'
esttab r4`x'
*outreg2 using Regionstable, append excel
}

forvalues x=1/5{
areg lnv i.demotreat_d##i.demotreat_o lny i.year if regioncode_o==`x', ///
	absorb(impexp) cluster(impexp)
est sto r5`x'
esttab r5`x'
*outreg2 using Regionstable, append excel
}


*out of sample prediction
*review just in case
****estimate the baseline specification excluding last 5 years of sample
*predict command to predict for full sample period


*Outliers?????
****concern?
*------->________> stability of specification for diff time periods and sub groups

*another test change in do file

