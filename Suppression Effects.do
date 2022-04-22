	
use "/Users/hartmabs/Box Sync/NAAS/NAAS/naas_2202019.dta", clear

//Final Data clean - hopefully
	rename q2_2d blm_final
	recode blm (0 1 = 0) (2 3 = 1), gen(blm2)
	recode race_important (0 1 = 0) (2 3 = 1)
	recode ethnic_important (0 1 = 0) (2 3 = 1)
	gen dream = q2_2f1
		replace dream = q2_2f2
		recode dream (1 2 = 0) (3 4 = 1) (5 6 = .)
	recode partyid (0 = 0 "Democrat") (1=1 "Republican") (2 3 = 2 "Other"), gen(pid)

	clonevar blm_east = blm_final 
		replace blm_east = . if ethn_cat != 0
	clonevar blm_southeast = blm_final 
		replace blm_southeast = . if ethn_cat != 2
	clonevar blm_south = blm_final
		replace blm_south = . if ethn_cat != 1
	clonevar blm_other = blm_final 
		replace blm_other = . if ethn_cat != 3
recode blm_final (1 2 = 0) (3 4 = 1) (5 6 = 2), gen(blm3)

	clonevar blm2_aa = blm2 
		replace blm2_aa = . if race != 1
	clonevar blm2_l = blm2 
		replace blm2_l = . if race != 6

	clonevar blm3_aa = blm3
		replace blm3_aa = . if race != 1
	clonevar blm3_l = blm3 
		replace blm3_l = . if race != 6
		
		recode black_contact (0 1 = 0) (2 3 = 1), gen(bc2)

**To Analysis
keep if race == 1 | race == 6

// Listwise deletion 
 drop if missing(money_worries, region, usborn, sec_gen, pid, educ, female, ///
 age, race_important, ethnic_important, effectall_aapi, effectall_ethnic, bc2)

save final_dataset.dta, replace
 
keep if race == 1
save aapi.dta, replace

use final_dataset.dta, clear
keep if race == 6
save latino.dta, replace

use final_dataset.dta, clear
logit blm2 race_important ethnic_important effectall_a effectall_e usborn sec_gen i.age i.female c.educ i.pid c.money_worries ib4.region
collin race_important ethnic_important effectall_a effectall_e usborn sec_gen age female educ pid money_worries region

domin blm2 race_important ethnic_important effectall_a effectall_e, reg(logit) fitstat(e(r2_p)) /// 
	all(usborn sec_gen i.age i.female c.educ i.pid c.money_worries ib4.region)
	
domin blm3 race_important ethnic_important effectall_a effectall_e, reg(mlogit) fitstat(e(r2_p)) /// 
	all(usborn sec_gen i.age i.female c.educ i.pid c.money_worries ib4.region)

use aapi.dta, clear
logit blm2 race_important ethnic_important effectall_a effectall_e usborn sec_gen i.age i.female c.educ i.pid c.money_worries ib4.region
collin race_important ethnic_important effectall_a effectall_e usborn sec_gen age female educ pid money_worries region


*domin blm2 race_important ethnic_important effectall_a effectall_e, reg(logit) fitstat(e(r2_p)) ///
*	all(usborn sec_gen i.age i.female c.educ i.pid c.money_worries ib4.region)
	
domin blm3 race_important ethnic_important effectall_a effectall_e, reg(mlogit) fitstat(e(r2_p))  nocon nocom ///
	all(usborn sec_gen i.age i.female c.educ i.pid c.money_worries ib4.region)

use latino.dta, clear
logit blm2 race_important ethnic_important effectall_a effectall_e usborn sec_gen i.age i.female c.educ i.pid c.money_worries ib4.region
collin race_important ethnic_important effectall_a effectall_e usborn sec_gen age female educ pid money_worries region

*domin blm2 race_important ethnic_important effectall_a effectall_e, reg(logit) fitstat(e(r2_p)) /// 
*	all(usborn sec_gen i.age i.female c.educ i.pid c.money_worries ib4.region)
	
domin blm3 race_important ethnic_important effectall_a effectall_e, reg(mlogit) fitstat(e(r2_p)) nocon nocom ///
	all(usborn sec_gen i.age i.female c.educ i.pid c.money_worries ib4.region)

	
//Support
foreach dataset in final_dataset aapi latino {
	use `dataset'.dta, clear
	recode blm3 (0 2 = 0) (1=1), gen(blm_support)

		domin blm_support race_important ethnic_important effectall_a effectall_e, reg(logit) fitstat(e(r2_p)) /// 
			all(usborn sec_gen i.age i.female c.educ i.pid c.money_worries ib4.region)
}	

//No Support
foreach dataset in final_dataset aapi latino {
	use `dataset'.dta, clear
	recode blm3 (0 1 = 0) (2=1), gen(blm_nosupport)
		domin blm_nosupport race_important ethnic_important effectall_a effectall_e, reg(logit) fitstat(e(r2_p)) /// 
			all(usborn sec_gen i.age i.female c.educ i.pid c.money_worries ib4.region)
}	

//No opinion
foreach dataset in final_dataset aapi latino {
	use `dataset'.dta, clear
recode blm3 (1 2 = 0) (0=1), gen(blm_nop)
		domin blm_nop race_important ethnic_important effectall_a effectall_e, reg(logit) fitstat(e(r2_p)) /// 
			all(usborn sec_gen i.age i.female c.educ i.pid c.money_worries ib4.region)
}	
