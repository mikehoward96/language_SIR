cd "C:\Users\mhoward\OneDrive - Cato Institute\Mike\language SIR"
global datapath "\\FS01.cato.int\USERS\mhoward\Desktop\datasets\d_english_acq"

forvalues y = 2010/2019 {
	import excel "$datapath\eng_`y'.xlsx", sheet("Data") firstrow case(lower) clear
		
		drop c e g i k m o q s u w y aa ac ae ag ai ak am ao aq as au aw ay ba bc be bg bi bk bm bo ///
			 bq bs bu bw by ca cc ce cg ci ck cm co cq cs cu cw cy da puertorico
			 
		foreach var of varlist _all {
			if "`var'" != "a" qui capture replace `var' =subinstr(`var',",","",.)
			if "`var'" != "a" qui capture destring `var', replace force
			rename `var' r_`var'
				}
				
		
		keep if inlist(_n, 25, 26, 28, 33, 38, 43)
		
		drop r_a
		
		insobs 1, after(6)
		
			foreach var of varlist _all {
				qui replace `var' = (`var'[2]+`var'[3]+`var'[4]+`var'[5]+`var'[6])/`var'[1] if `var' == .
				}
				
		keep if _n == 7
		gen placeholder = 1
		
		qui reshape long r_, i(placeholder) j(state_name) string
		rename r_ fbes //foreign-born english speaking
		drop placeholder
		gen year = `y'
	save "$datapath\eng_`y'.dta", replace
	}
	
use "$datapath\eng_2010.dta", clear

	forvalues y = 2011/2019 {
		append using "$datapath\eng_`y'.dta"
		}
		
		
		replace state = "district of columbia" if state == "districtofcolumbia"
		replace state = "new hampshire" if state == "newhampshire"
		replace state = "new york" if state == "newyork"
		replace state = "new jersey" if state == "newjersey"
		replace state = "new mexico" if state == "newmexico"
		replace state = "north carolina" if state == "northcarolina"
		replace state = "north dakota" if state == "northdakota"
		replace state = "rhode island" if state == "rhodeisland"
		replace state = "south carolina" if state == "southcarolina"
		replace state = "south dakota" if state == "southdakota"
		replace state = "west virginia" if state == "westvirginia"
		
save "state_fbes_year.dta", replace	

use "state_population.dta", clear

	merge 1:1 state_name year using "C:\Users\mhoward\OneDrive - Cato Institute\Mike\language SIR\state_fbes_year.dta"
	
	*impute missing values using linear trend			
		levelsof state_name, local(levels)
			foreach state of local levels {
				qui reg fbes year if state_name == "`state'"
					predict imp if state_name == "`state'"
					replace imp = fbes if fbes != . & state_name == "`state'"
				
				*twoway scatter imp year if state_name == "`state'" || ///
					*lfit imp year if state_name == "`state'", xline(2009) title("`state'")
					*graph export "graphs\imp_`state'.png", replace
					
				replace fbes = imp if fbes ==. & state_name == "`state'"
				drop imp
					
			}
			
	gen imputed_share = year <= 2009
	drop _m
	
	merge 1:1 year state_name using "immigrants_by_state.dta", nogen keep (1 3)
	sort state_name year
	
save "merged_sir_dataset.dta", replace
				
		