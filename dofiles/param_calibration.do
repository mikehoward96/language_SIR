*parameter calibration
cd "C:\Users\mhoward\OneDrive - Cato Institute\Mike\language SIR"
use "merged_sir_dataset.dta", clear
rename state_name state

global start = 2006 // earliest year + 1
global end = 2019
	
		*real data
			gen r_N = Population
			gen r_f = Foreign
			gen r_n = Native
			gen r_x = r_f*fbes
			gen r_i = r_x+r_n
			
		*model params
			gen f = .
			gen n = .
			gen x = .
			gen i = .
			gen s = .
			gen W = .
			gen s_tp1 = .
			gen i_tp1 = .
			gen x_tp1 = .
			
		*initial state
			foreach var in f n x i {
			replace `var' = r_`var' if year == $start -1 
			}
					
	*known parameters
		global b = 0.012 //annual birth rate
		global d = 0.009 //annual death rate
		
*estimating unknown parameters
	
local M = 1
		
forvalues beta = 0(0.005)0.5 {
	
	forvalues gamma = 0.5(0.005)1 {
	
	preserve
	
			local iteration = `M'
		
			global c = `gamma' //contact btwn. imms and natives
			global B = `beta' // tranmissibility
			
		*initial knowns
			qui	bys state: replace W = ((x/i)*$B) + ((n/i)*($c * $B)) if year == $start -1
			qui	by state: replace s = f - x if year == $start -1
			qui	by state: replace i_tp1 = W*s + ($b * n) - ($d * n) if year == $start -1
			qui	by state: replace s_tp1 = ($b * f) - ($d * f) + m if year == $start -1
			qui	by state: replace x_tp1 = W*s if year == $start -1
				
				
			*discrete time evolution equations
			qui by state: replace f = f[_n-1] + ($b * f[_n-1]) - ($d * f[_n-1]) + m if _n != 1
			qui by state: replace n = n[_n-1] + ($b * n[_n-1]) - ($d * n[_n-1]) if _n != 1

			*iterating model
				forvalues t = $start / $end {
					
					qui	by state: replace s = s[_n-1] + s_tp1[_n-1] + m - x_tp1[_n-1] if year == `t'			
					qui	by state: replace i = i[_n-1] + i_tp1[_n-1] if year == `t'				
					qui	by state: replace x = x[_n-1] + x_tp1[_n-1] if year == `t'
						
					qui replace W = ($B * (x/i)) + ($c * $B * (n/i)) if year == `t'
					qui	replace i_tp1 = W*s + ($b * n) - ($d * n) if year == `t'
					qui	replace s_tp1 = ($b * f) - ($d * f) if year == `t'
					qui	replace x_tp1 = W*s if year == `t'
					
					}
			
			*comparing proportion of English speakers: real vs. predicted
				gen r_Y = (r_n+r_x)/r_N
				gen Y = (n+x)/(i+s)
			
			gen sq_error = (r_Y - Y)^2
			
		collapse (sum) sse = sq_error //[fw = Population]
		gen beta = $B
		gen gamma = $c
		gen iter = `iteration'
		
		append using "calibration_results.dta"
		qui save "calibration_results.dta", replace
		
		di `iteration'
		
		local M = `M'+1

	restore

	}
}

	*check appended file for lowest SSE value and use that beta and gamma
		use "calibration_results.dta", clear
			sort sse
			
			keep if _n == 1
			gen winner = 1
			
		save "calibration_results.dta", replace
		
		
/*
	levelsof state, local(levels)
	foreach s of local levels {
		line r_Y Y year if state == "`s'", title("B = $B, c = $c") subtitle("`s'")
		graph export "graphs\calibration_`s'.png", replace
		}
