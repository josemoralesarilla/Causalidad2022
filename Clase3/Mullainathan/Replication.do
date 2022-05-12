use "/Users/josemoralesarilla/Desktop/Mullainathan/lakisha_aer.dta", clear

gen black = (race == "b")
gen black_fracwhite = black * fracwhite
gen boston = (city == "b")
gen black_boston = black * boston
gen black_req = black * req
gen black_expreq = black * expreq

// Table 1
* Main result
prtest call, by(black)
reg call black // Regression == Proportion test

* Main result by city
reg call black if city == "c"
reg call black if city == "b"

* Main result by gender
reg call black if sex == "f"
reg call black if sex == "m"

// Table 3: Markers of quality are orthogonal to race, not to quality
prtest email, by(black)
prtest email, by(h)

ttest yearsexp, by(black)
ttest yearsexp, by(h)

prtest honors, by(black)
prtest honors, by(h)

// Table 4: Callbacks by quality
prtest call if black == 0, by(h)
prtest call if black, by(h)

// Table 6: Markers of background
probit call fracwhite black_fracwhite black boston black_boston, cluster(adid)
margins, dydx(*) atmeans



/*
reg call i.black##c.fracwhite

reg call black if expreq
