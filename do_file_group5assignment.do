/* Applied Econometrics
Dataset: School_spending.dta
*/


***clear workspace
clear all

*** Setting working directory
cd "/Users/benson/Desktop/applied econometrics"

*** creating and saving a logfile
log using "results/educ_logfile", text replace

*** loading the school_spending.dta dataset
use "data/school_spending", clear

***Inspecting and cleaning the dataset
browse
describe

*The dataset contains 420 observations and 11 variables. Some variables are labeled in German. This is changed in the following code.

*** dropping computer variable because it is not included in the question paper
drop computer

*** checking for missing data
mdesc
*no missing observations in the datase

*** changing the variable labels
label variable dist_cod "District-ID"
label variable read_scr "Average test scores reading"
label variable math_scr "Average test scores math"
label variable testscr "Average test scores general (read_scr+math_scr)/2)"
label variable meal_pct "Share of pupils with food vouchers (in %)"
label variable comp_stu "Number of computers per pupil"
label variable expn_stu "Expenditures per pupil"
label variable avginc "Average income (in 1000 US$)"
label variable el_pct "Share of non native English speakers (in %)"
label variable str "Student Teacher Ration (in %)"

***save the cleaned data
save "clean_data/school_spending_cleaned", replace

*** Descriptive statistics: Using summary statistics and graphical methods

* Summary statistics
ssc install estout
* installs estpost and esttab to produce better output
estpost summarize testscr expn_stu meal_pct comp_stu avginc el_pct
esttab using results/Table1.rtf, ///
cells("mean sd min max count") ///
title("Table 1: Summary Statistics") ///
label replace

* A histogram of the distribution of average test scores
histogram testscr, frequency normal ///
 title ("Histogram of the Distribution of Pupil's Average Test Scores") ///
 xtitle ("Average Test Scores") ///
 ytitle ("Frequency") ///
 start(605) ///
 width (5) ///
 graphregion(color(white)) ///
 name (FigureI, replace)
 graph export "results/FigureI.png", replace
 * the distribution is approximately symmetric

* A histogram of the distribution of expenditure per pupil
histogram expn_stu, frequency normal ///
 start(3926) ///
 title ("Histogram of the Distribution of School Expenditure Per Pupil") ///
 xtitle ("Expenditure Per Pupil") ///
 ytitle ("Frequency") ///
 graphregion(color(white)) ///
 name (FigureII, replace)
 graph export "results/FigureII.png", replace
 * the distribution is right-skewed
 
 * A scatter plot of the relationship between average test scores and expenditure per pupils, including in it a their correlation coefficient.

 * obtaining correlation coefficient
correlate testscr expn_stu
 local corr = r(rho)
 * creating a twoway scatter with a correlation coefficient
 twoway ///
 (scatter testscr expn_stu, ///
 mcolor(navy) ///
 msize(small) ///
 msymbol(circle)) ///
 (lfit testscr expn_stu, ///
 lcolor(red) ///
 lwidth(medium)), ///
 title("Scatter Plot of Pupil's Test Scores and Expenditure Per Pupil") ///
 subtitle("Correlation = `: display %4.3f `corr''") ///
 xtitle("Expenditure Per Pupil") ///
 ytitle("Average Test Score", margin(medium)) ///
 legend(order(1 "Observed Values" 2 "Fitted Line")) ///
 graphregion(color(white)) ///
 name(FigureIII, replace)
* exporting the scatter plot as MS Word
 graph export "results/FigureIII.png", replace
 
 
/* Specifying a baseline modal
dependent variable: Average Test Scores, testscr
Independent variable: Expenditure per pupils, expn_stu
Covariates: 
 - share of pupils with food vouchers, meal_pct
 - number of computers per pupil, comp_stu
 - share of non-native English speakers, el_pct
 Since expenditure per pupil is skewed to the right, it is transformed to log. This is also to ensure better interpretation of its effect on test scores
*/

* log transforming expenditure per pupil to make its distribution symmetric
generate log_expn = log(expn_stu)
* this generates log of average test scores

* label log_expn
label variable log_expn "log (expenditure per pupil)"

* Model I: Bivariate regression of expn_stu on testscr
eststo modelI: regress testscr log_expn
* Run regression and store the estimates as modelI

* Model II: Multivariate regression, including covariates
eststo modelII: regress testscr log_expn el_pct comp_stu avginc meal_pct

* extract and save the regression estimates of model I and II
esttab modelI modelII using "results/baseline_reg.rtf", replace ///
b(3) se(3) ///
compress ///
nogaps ///
varwidth(20) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(F p r2 r2_a N, fmt(3 3 3 3 0) ///
labels("F_Statistic" "Prop > F" "R_Squared" "Adjusted R_squared" "Observations")) ///
mtitles("Bivariate Regression" "Multivariate Regression") ///
title("Table II: OLS Regression of Average Test Scores on Expenditure Per Pupil") ///
label

***Testing and comparing different model specifications

/*
Model I is the baseline multivariate model
Model II is a semi-log model including log transformed average income
*/
eststo clear
* clears stored model estimates

* Generate variables to test non-linear relationships 
generate log_avginc = log(avginc)
* generates a log transformation of average income

label variable log_avginc "log (average income)"

*** estimating and comparing two models I (baseline) and II (quadratic)

eststo modelI: regress testscr log_expn el_pct comp_stu avginc meal_pct
* baseline model I

eststo modelII: regress testscr log_expn log_avginc el_pct comp_stu meal_pct
* semi-log model II, with log transformed average income

* extract and and save the regression estimates of modelI and II
esttab modelI modelII using "results/model_testing_reg.rtf", replace ///
b(3) se(3) ///
compress ///
nogaps ///
varwidth(20) ///
star(* 0.10 ** 0.05 *** 0.01) ///
stats(r2_a aic bic N, fmt(3 3 3 0) ///
labels("Adjusted R_squared" "AIC" "BIC" "Observations")) ///
mtitles("Baseline Model" "log-linear Model") ///
title("Table III: Testing and Comparing Different Model Specifications") ///
label

*** Testing and Correcting for Heteroskedasticity

* Statistical test for heteroskedasticity

* White test
eststo clear
*clears stored model estimates

eststo m1: regress testscr log_expn el_pct comp_stu avginc meal_pct
* estimating baseline model without the option "robust"
estat imtest, white
* white test. Fail to reject homoskedasticity (Prob > chi2 = 0.7335 > 0.005)
return list
* to check which scalar contains the p-value of the white test
scalar white_p = r(p)
* store the white test P_value in regression model m1
est restore m1
estadd scalar white_p = white_p
* restores model m1 and adds the white test p-values to it

* estimating baseline model with robust standard errors
eststo m2: regress testscr log_expn el_pct comp_stu avginc meal_pct, robust
est restore m2
estadd scalar white_p = white_p


esttab m1 m2 using "results/robust.rtf", replace ///
nogaps ///
se star(* 0.10 ** 0.05 *** 0.01) ///
stats(N r2 white_p, fmt(0 3 3) ///
labels("Observations" "R_Squared" "White Test p_value")) ///
mtitles("OLS" "OLS with Robust SE") ///
title("Table IV: OLS Regression with Robust Standard Errors") ///
label


* Graphical analysis for heteroskedasticity

regress testscr log_expn el_pct comp_stu avginc meal_pct
* run the regression

predict resid_hat, resid
* predict residuals

predict y_hat, xb
* predict fitted values

* create a scatter plot of residuals and fitted values
twoway ///
(scatter resid_hat y_hat, ///
mcolor(navy) msize(small)) ///
(lowess resid_hat y_hat, ///
lcolor(red) lwidth(medium)), ///
yline(0, lpattern(dash) lcolor(black)) ///
title("Scatter Plot of Residuals and Fitted Values") ///
xtitle("Fitted Values") ///
ytitle("Residuals") ///
legend(order(1 "Residuals" 2 "Lowess smooth")) ///
graphregion(color(white)) ///
name(FigureIV, replace)
* exporting the scatter plot as MS Word
 graph export "results/FigureIV.png", replace

 
*** Checking functional form specification using RESET test
* Ho: correct specification. H1: misspecified

regress testscr log_expn el_pct comp_stu avginc meal_pct
estat ovtest
* Prob > F = 0.7762 > 0.05: Fail to reject Ho. Model has no ommitted variables

log close
