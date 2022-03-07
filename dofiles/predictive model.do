*prediction using estimations from quasi-MLA
cd "C:\Users\mhoward\OneDrive - Cato Institute\Mike\language SIR"
use "merged_sir_dataset.dta", clear
rename state_name state

global start = 2006 // earliest year + 1
global end = 2100
		
	
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
		
	*variable parameters
		global c = 0.995 // 0.995 // contact btwn. imms and natives. estimated by quasi-MLA
		global B = 0.055 // 0.055 // tranmissibility. estimated by quasi-MLA
		
	*initial knowns
			bys state: replace W = ((x/i)*$B) + ((n/i)*($c * $B)) if year == $start -1
			by state: replace s = f - x if year == $start -1
			by state: replace i_tp1 = W*s + ($b * n) - ($d * n) if year == $start -1
			by state: replace s_tp1 = ($b * f) - ($d * f) + m if year == $start -1
			by state: replace x_tp1 = W*s if year == $start -1
			
	
		keep state year r_N r_x r_n m f-x_tp1
		
			*adding in additional years
				local exp = $end - 2018
					expand `exp' if year == $start + 1, gen(e)
										
					foreach var in year r_N r_x r_n m f n x i s W s_ i_ x_ {
					replace `var' = . if e == 1
					}
					
						sort state year
						
						
					*local end2 = $end + 1
					by state: egen Y = seq(), f(2005) t($end)
					replace year = Y
					
			*imputing migration for future years (linear prediction)		
			levelsof state, local(levels)
			foreach state of local levels {
				qui reg m year if state == "`state'"
					qui predict imp if state == "`state'"
					qui replace imp = m if m != . & state == "`state'"
				
				*twoway scatter imp year if state == "`state'" || ///
					*lfit imp year if state == "`state'", xline(2019) title("`state'")
					*graph export "graphs\imp_m_`state'.png", replace
					
				qui replace m = imp if m ==. & state == "`state'"
				drop imp
					
			}
			
			
			
				by state: egen imp_m = mean(m)
				replace m = imp_m if m == .
				
			drop e Y imp_m 
				
			
			
		*discrete time evolution equations
			qui by state: replace f = f[_n-1] + ($b * f[_n-1]) - ($d * f[_n-1]) + m if _n != 1
			qui by state: replace n = n[_n-1] + ($b * n[_n-1]) - ($d * n[_n-1]) if _n != 1
			
		save temp.dta, replace
		use temp.dta, clear

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
				
				
		
		*comparing proportion of English speakers
			gen r_Y = (r_n+r_x)/r_N if inrange(year, 2005, 2019)
			gen Y = (n+x)/(i+s)
			
	save "projected_data.dta", replace
	*use projected_data.dta", clear
		
	*line r_Y Y year if state == "texas", title("texas") ylabel(0.6(0.4)1) xlabel(none) ytitle("Prop. English-Speaking Population") xtitle("")
		
	gen title_s = subinstr(state," ","_",.)
	replace title_s = upper(title_s)
		
	local graphs ""	
	levelsof title_s, local(levels)
	foreach s of local levels {
		line Y year if title_s == "`s'", title("`s'") ylabel(0(0.5)1) xlabel(minmax) ytitle("") xtitle("") ///
		name(proj_`s', replace) nodraw
		local graphs "`graphs' proj_`s'"
		
		*graph export "graphs\w_projection_`s'.png", replace
		}
		
		preserve 
			collapse (sum) r_n r_x r_N s i, by(year)
			gen Y = (i/(s+i))
			gen r_Y = (r_n+r_x)/r_N if inrange(year, 2005, 2019)
			line Y year, title("NATION") ylabel(0(0.5)1) xlabel(minmax) ytitle("") xtitle("") ///
			name(proj_NATION, replace) nodraw
			local graphs "`graphs' proj_NATION"
		restore
		
		graph combine `graphs', xsize(1.5) ysize(5) col(4) ycommon xcommon
		graph export "graphs\projection_all_fixed.png", replace
		


*other graphs
		
		
	local graphs ""	
	levelsof title_s, local(levels)
	foreach s of local levels {
		line n f year if title_s == "`s'", title("`s'") xlabel(minmax) ytitle("") xtitle("") ///
		name(proj_`s', replace) nodraw
		local graphs "`graphs' proj_`s'"
		
		*graph export "graphs\w_projection_`s'.png", replace
		}
		
		preserve 
			collapse (sum) n f, by(year)
			line n f year, title("NATION") xlabel(minmax) ytitle("") xtitle("") ///
			name(proj_NATION, replace) nodraw
			local graphs "`graphs' proj_NATION"
		restore
		
		graph combine `graphs', xsize(1.5) ysize(5) col(4)
		graph export "graphs\pop_all.png", replace
		
	/*national trend
		collapse (sum) s i, by(year)
			gen Y = (i/(s+i))
			line Y year, title("Nation") ylabel(0.6(0.1)1) ytitle("Prop. English-Speaking Population") xtitle("")
			graph export "graphs\w_projection_AAnation.png", replace
