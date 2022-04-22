//  program:    NAAS Clean
//  author:     BS Hartmann
//  version:    2019-12-09

clear 		all
set 		linesize 80
set matsize 800
matrix drop _all
set more off, permanently

//setting path name and opening data
		cd "/Users/hartmabs/Documents/Masters/New"
		use "/Users/hartmabs/OneDrive - Indiana University/NAAS/NAAS/naas_2202019.dta", clear

		gen age_2 =  2016 - q10_18 
		replace age_2 = . if age_2 < 0

		replace age = age_2
		
		//Final Data clean - hopefully
			rename q2_2d blm_final
			recode blm (0 1 = 0) (2 3 = 1), gen(blm2)

		**To Analysis
		keep if race == 1 | race == 6

		foreach var in  q4_5a q4_5b q4_5c q4_5d {
		recode `var' (2 = 0) (88 99 = .)
		}

		rename q4_5a common_race
		rename q4_5b common_culture
		rename q4_5c common_econ
		rename q4_5d common_pol

		gen group_con = common_race + common_culture + common_econ + common_pol
		alpha common_*
		label var group_con "Group Conciousness"
		
		tabstat common_*, by(race)

		clonevar black_common = q4_6a 
			replace black_common = . if q4_6a != 1
			replace black_common = q4_6b if order4_602 == 1
			replace black_common = q4_6c if order4_603 == 1
			
		clonevar asian_common = q4_6a 
			replace asian_common = . if q4_6a != 4
			replace asian_common = q4_6b if order4_602 == 4
			replace asian_common = q4_6c if order4_603 == 4

		clonevar hisp_common = q4_6a 
			replace hisp_common = . if q4_6a != 2
			replace hisp_common = q4_6b if order4_602 == 2
			replace hisp_common = q4_6c if order4_603 == 2
			
		foreach var in black_common asian_common hisp_common {
			recode `var' (88 99 = .)
			replace `var' =  (`var'-5)*-1
		}

		gen common_minority = .
			replace common_minority = (asian_common+black_common)/2 if race == 6
			replace common_minority = (hisp_common+black_common)/2 if race == 1

		*recode black_common (3 4 = 1) (1 2 = 0)

			clonevar bc_aa = black_common
				replace bc_aa = . if race != 1
			clonevar bc_l = black_common 
				replace bc_l = . if race != 6

		clonevar income = q10_15 
		replace income = . if income == 88 | income == 99
		
		label var effectall_aapi "Panethnic Linked Fate"
		label var effectall_ethnic "Ethnic Linked Fate"
	save pre_listwise_deletion.dta, replace
			recode partyid (0 = 0 "Democrat") (1=1 "Republican") (2 3 = 2 "Other"), gen(pid)
		recode blm_final (1 2 = 0) (3 4 = 1) (5 6 = 2), gen(blm3)

recode blm3 (2=0) (0 1 = 1), gen(answered)
recode blm3 (1=1) (2 0 =0), gen(blm4)
recode blm3 (2=1) (1 0 =0), gen(blm5)

foreach var in q3_1_a-q3_1_f {
recode `var' (99 88 = .) 
}

foreach var in q3_6_a-q3_6_g {
recode `var' (88 99 = .)
}

foreach var in q5_1_* {
recode `var' (88 99 = .) (1=1) (2=0)
}

alpha q5_1_*, std min(8) gen(disc_scale)

gen discrim= 1 if q5_1_b==1
replace discrim = 0 if q5_1_b==0
replace discrim= discrim+1 if q5_1_c==1
replace discrim= discrim+1 if q5_1_d==1
replace discrim= discrim+1 if q5_1_e==1
replace discrim= discrim+1 if q5_1_g==1
replace discrim= discrim+1 if q5_1_h==1
replace discrim= discrim+1 if q5_1_i==1
replace discrim= discrim+1 if q5_1_j==1
replace discrim= discrim+1 if q5_1_k==1

alpha q3_1_a-q3_1_f, c i 
alpha q3_6_a-q3_6_g , c i 
alpha q3_6_a-q3_6_g q3_1_a-q3_1_f, c i gen(inequal_scale) min(8)
*egen ineq_s = std(inequal_scale)

	// Listwise deletion 
	drop if missing(income, region, usborn, sec_gen, pid4, educ, female, ///
	age, race_important, ethnic_important, effectall_aapi, effectall_ethnic, ///
	common_race, common_cul, common_pol, common_econ, black_con)
	
replace q3_1_b=((q3_1_b-6)*-1)
replace q3_1_d=((q3_1_d-6)*-1)
replace q3_1_e=((q3_1_e-6)*-1)

alpha q3_1_a-q3_1_f, i d
alpha q3_6_a-q3_6_g , c i 
	
gen inequal_scale2 = ((q3_1_a+(((q3_1_b-6)*-1)+((q3_1_d-6)*-1)+((q3_1_e-6)*-1))+q3_1_c+q3_1_f))*-1
egen ineq_s2 = std(inequal_scale2)

gen gov_act = (q3_6_a+q3_6_b+q3_6_c+q3_6_d+q3_6_f+q3_6_g)
egen gov_act2 = std(gov_act)
replace gov_act2 = gov_act2 * (-1)

gen new_ineq = inequal_scale2 + gov_act
egen new_ineq2 = std(inequal_scale2)

recode educ (1 2 3 4 = 0) (5 6 =1)

recode blm_final (1 2 = 0) (3 4 = 2) (6 5=1), gen(tree)
clonevar tree_asian = tree
	replace tree_asian = . if race == 6
clonevar tree_latino = tree
	replace tree_latino = . if race == 1
	
	recode tree_asian (0 1 = 0) (2=1), gen(asian_support)
		clonevar asian_support2 = asian_support
	recode tree_asian (0 1 = 0) (2 =1), gen(asian_opinion)

	recode tree_latino (0 1 = 0) (1=1), gen(latino_support)
		clonevar latino_support2 = latino_support
	recode tree_latino (0  3= 0) (1 2 =1), gen(latino_opinion)
	
	foreach var in black_common black_con discrim {
		qui sum `var'
		qui replace `var' = (`var' - abs(r(min)))
		qui sum `var' 
		qui replace `var' = `var'/r(max)
		sum `var'
		}
		
		foreach var in  cat_effectall_a cat_effectall_ethnic group_con race_important ethnic_important black_common int_ ineq_s2 gov_act2 {
		qui sum `var'
		qui replace `var' = (`var' + abs(r(min)))
		qui sum `var' 
		qui replace `var' = `var'/r(max)
		sum `var'
		}
		
save "masters_mediation.dta", replace
/*
capture drop inequal_scale
alpha q3_6_a-q3_6_g, std item min(4) gen(inequal_scale)
replace inequal_scale = inequal_scale*-1
capture drop inequal_scale2
gen inequal_scale2 = ((q3_1_a+(((q3_1_b-5)*-1)+((q3_1_d-5)*-1)+((q3_1_e-5)*-1))+q3_1_c+q3_1_f))*-1
egen ineq_s = std(inequal_scale)
egen ineq_s2 = std(inequal_scale2)
*/
		desctable i.tree c.race_important c.ethnic_important c.cat_effectall_a c.cat_effectall_ethnic ///
		c.group_con i.common_race i.common_cul i.common_pol i.common_econ ///
, filename(model_var_id) ///
			stats(mean sd) group(race) 
			
		desctable c.black_common c.int_ c.ineq_s c.gov_act2 ///
, filename(model_var_mediator) ///
			stats(mean sd) group(race) 

					desctable c.age i.female i.educ i.pid4 c.income i.region i.usborn i.sec_gen c.discrim ///
, filename(model_var_control) ///
			stats(mean sd) group(race) 
			
local identity race_important ethnic_important
local group cat_effectall_a cat_effectall_ethnic group_con
local control c.age i.female c.educ i.pid4 c.income ib4.region i.usborn i.sec_gen c.black_con c.discrim 

ologit tree_asian `identity' `group' `control'
brant
ologit tree_latino `identity' `group' `control' 
brant 

eststo : mlogit tree_asian race_important ethnic_important cat_effectall_a cat_effectall_ethnic group_con c.age i.female c.educ i.pid4 c.income ib4.region i.usborn i.sec_gen  c.discrim [pweight = nweightnativity], b(0)
eststo : mlogit tree_latino race_important ethnic_important cat_effectall_a cat_effectall_ethnic group_con c.age i.female c.educ i.pid4 c.income ib4.region i.usborn i.sec_gen c.discrim [pweight = nweightnativity], b(0)
eststo : mlogit tree_asian race_important ethnic_important cat_effectall_a cat_effectall_ethnic group_con c.age i.female c.educ i.pid4 c.income ib4.region i.usborn i.sec_gen  c.discrim black_con [pweight = nweightnativity], b(0)
eststo : mlogit tree_latino race_important ethnic_important cat_effectall_a cat_effectall_ethnic group_con c.age i.female c.educ i.pid4 c.income ib4.region i.usborn i.sec_gen c.discrim black_con [pweight = nweightnativity], b(0)

esttab est1 est3 est2 est4 using "Table1_new_temp.csv", unstack replace nobase bic aic n se eform compress label

//Support
domin asian_support race_important ethnic_important effectall_a effectall_e [pweight = nweightnativity], reg(logit) fitstat(e(r2_p)) /// 
			all(`control')
			
domin latino_support race_important ethnic_important effectall_a effectall_e [pweight = nweightnativity], reg(logit) fitstat(e(r2_p)) /// 
			all(`control')	


gsem (ib0.tree_asian <-`identity' `group' `control', mlogit) ///
	 (ib0.tree_latino <- `identity' `group' `control' , mlogit) [pweight = nweightnativity], ///
	 vce(robust) iterate(45)
				est store prediction_model_mlogit
esttab prediction_model_mlogit using "Table2_new_temp.csv", unstack replace nobase bic aic n se eform compress label

gsem (ib2.tree_asian <-c.cat_effectall_ethnic  `control', mlogit) ///
	 (ib2.tree_latino <-c.cat_effectall_ethnic `control' , mlogit) [pweight = nweightnativity], ///
	 vce(robust) iterate(45)
	 
gsem (ib2.tree_asian <-c.cat_effectall_a `control', mlogit) ///
	 (ib2.tree_latino <-c.cat_effectall_a  `control' , mlogit) [pweight = nweightnativity], ///
	 vce(robust) iterate(45)

log using MEcompare_mlogit_temp, replace text
qui mlincom, clear
		foreach var2 in race_important ethnic_important cat_effectall_a cat_effectall_ethnic group_con {
			foreach num in 0 1 2 {
				est restore prediction_model_mlogit
					qui margins, over(race) post at(`var2' = (0 1)) predict(outcome(`num'.tree_asian)) predict(outcome(`num'.tree_latino)) 
						qui mlincom 1,            rowname("AAPI: No `var2'`num'") stat(est se p ul ll) add 
						qui mlincom 3,            rowname("AAPI: Yes `var2'`num'") stat(est se p ul ll) add 
						qui mlincom 3 - 1,        rowname("AAPI: Diff `var2'`num'") stat(est se p ul ll) add 
						qui mlincom 6,            rowname("Latino:No `var2'`num'") stat(est se p ul ll) add 
						qui mlincom 8,            rowname("Latino: Yes `var2'`num'") stat(est se p ul ll) add 
						qui mlincom 8-6,           rowname("Latino: Diff `var2'`num'") stat(est se p ul ll) add 
						qui mlincom (3-1)-(8-6),    rowname("Df ADC `var2'`num'") stat(est se p ul ll) add 					
					mlincom, stat(est se p ul ll) dec(3) twidth(15) ///
							title("DCR for `var2' in model`num'")
				}	
			}
log close 	
matrix list _mlincom
putexcel set "huge_matrix2.xlsx", sheet("A") replace
putexcel A1=matrix(_mlincom), names

log using MEcompare_mlogit_temp2, replace text
qui mlincom, clear
		foreach var2 in race_important ethnic_important cat_effectall_a cat_effectall_ethnic group_con {
				est restore prediction_model_mlogit
					qui margins, over(pid4 race) post at(`var2' = (0 1)) predict(outcome(2.tree_asian)) predict(outcome(2.tree_latino)) 
						qui mlincom 1,            rowname("Dem AAPI: No `var2'") stat(est se p ul ll) add 
						qui mlincom 7,            rowname("Dem AAPI: Yes `var2'") stat(est se p ul ll) add 
						qui mlincom 7-1,            rowname("Dem AAPI: Diff `var2'") stat(est se p ul ll) add 
						
						qui mlincom 3,            rowname("Rep AAPI: No `var2'") stat(est se p ul ll) add 
						qui mlincom 9,            rowname("Rep AAPI: Yes `var2'") stat(est se p ul ll) add 						
						qui mlincom 9-3,            rowname("Rep AAPI: Diff `var2'") stat(est se p ul ll) add 						
						
						qui mlincom 5,            rowname("Ind AAPI: No `var2'") stat(est se p ul ll) add 
						qui mlincom 11,            rowname("Ind AAPI: Yes `var2'") stat(est se p ul ll) add 
						qui mlincom 11-5,            rowname("Ind AAPI: Diff `var2'") stat(est se p ul ll) add 
						
						qui mlincom 14,            rowname("Dem Latino: No `var2'") stat(est se p ul ll) add 
						qui mlincom 20,            rowname("Dem Latino: Yes `var2'") stat(est se p ul ll) add
						qui mlincom 20-14,            rowname("Dem Latino: Diff `var2'") stat(est se p ul ll) add
						
						qui mlincom 16,            rowname("Rep Latino: No `var2'") stat(est se p ul ll) add 
						qui mlincom 22,            rowname("Rep Latino: Yes `var2'") stat(est se p ul ll) add 
						qui mlincom 22-16,            rowname("Rep Latino: Diff `var2'") stat(est se p ul ll) add 
						
						qui mlincom 18,            rowname("Ind Latino: No `var2'") stat(est se p ul ll) add 
						qui mlincom 24,            rowname("Ind Latino: Yes `var2'") stat(est se p ul ll) add 
						qui mlincom 24-18,            rowname("Ind Latino: Diff `var2'") stat(est se p ul ll) add 

					mlincom, stat(est se p ul ll) dec(3) twidth(15) ///
							title("DCR for `var2' in model")
			}
log close 	

