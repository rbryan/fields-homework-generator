(prob "3-5" '(
		
		 (set "m" "1e-4")
		 (set "l" "0.2")
		 (maxima-eval "epsilon: \"epsilon_0\"$")
		 (vspace)
		 (set "f_g" "[0,-9.8*m]" "Calculate the force on one sphere due to gravity.")
		 (set "f_sy" "[0,9.8*m]" "String's $y$ component will oppose $f_g$.")
		 (set "f_q" "[-1*q^2/4/%pi/epsilon/(sin(10)*l)^2,0]" "Calculate the force vector 
		      on one charge due to Coulomb's law.")
	         (set "f_sx" "-1*f_q"	"$f_sx$ should oppose $f_q$.")
		 (solve "f_sx+f_sy+f_q+f_g=[0,0]" "q" "Forces should sum to zero.")

		 ))
