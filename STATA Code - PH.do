clear
use "C:\Users\B51528\Documents\inflated_adjusted.dta"

gen id = _n

reshape long CashShortTermInvestments_ LongTermDebtTotal_ DepreciationAmortization_ DistributionPrefShares_ DivPaidTotal_ TotalDebt_ TotalCurrentLiabilities_ CommonEquityTotal_ TotalAssets_ InbDiscOpExtraItems_ TotalLiabilitiesEquity_ PriceClose_ PPE_ TotalShareholdersEquity_ IncomeTaxesDeferred_ StockholdersEquity_ InventoriesTotal_ OutstandingShares_ NetIncomeReal_ MarketCapitalization_, i(i) j(year)

tsset id year

* Generate a flag for non-missing PriceClose values
gen flag = !missing(PriceClose)

* Generate the Age variable
by Identifier (year), sort: gen Age = sum(flag)

* Replace Age with missing where PriceClose is missing
replace Age = . if missing(PriceClose)

* Reset Age to count consecutively non-missing PriceClose
by Identifier (year): replace Age = sum(Age != .)

* You might want to drop the flag variable if it's no longer needed
drop flag

* Keep observations where the year is 1996 or later
keep if year >= 1996

* Generate Size as the natural log of TotalAssets
gen Size = log(TotalAssets)

* Generate Size2 as the square of Size
gen Size2 = Size^2

* Replace NA with 0 in ColumnWithNA
*replace OutstandingShares  = 0 if missing(OutstandingShares)

replace DistributionPrefShares  = 0 if missing(DistributionPrefShares)  

replace DivPaidTotal  = 0 if missing(DivPaidTotal)

* Replace 0 with NA in ColumnWithZero
replace Age = . if Age == 0
replace PPE = . if PPE == 0
replace TotalLiabilitiesEquity = . if TotalLiabilitiesEquity == 0
replace CashShortTermInvestments = . if CashShortTermInvestments == 0
replace LongTermDebt = . if LongTermDebt == 0
replace TotalDebt = . if Age == TotalDebt
replace DepreciationAmortization = . if DepreciationAmortization == 0
replace TotalCurrentLiabilities = . if TotalCurrentLiabilities == 0
replace CommonEquityTotal = . if CommonEquityTotal == 0
replace TotalAssets = . if TotalAssets == 0
replace InbDiscOpExtraItems = . if InbDiscOpExtraItems == 0
replace NetIncomeReal = . if NetIncomeReal == 0
replace InventoriesTotal = . if InventoriesTotal == 0
replace StockholdersEquity = . if StockholdersEquity == 0

* Create SA as a linear combination of Size, Size2, and Age
gen SA = (-0.737 * Size) + (0.043 * Size2) - (0.040 * Age)

* Sort the data by Identifier and then by year to ensure proper order
sort id year

* Now create LagPPE and LagTA as PPE and TotalAssets lagged by one period
by id: gen LagPPE = L.PPE_
by id: gen LagTA = L.TotalAssets_

*Creating KZ variables
sort id year
by id: gen Q = (TotalLiabilitiesEquity_ - CommonEquityTotal_ - IncomeTaxesDeferred_ +( MarketCapitalization_ )) / TotalLiabilitiesEquity_
by id: gen CashFlowK = ( InbDiscOpExtraItems_ + DepreciationAmortization_ ) / LagPPE
by id: gen DebtTotalCapital = ( LongTermDebtTotal_ + ( TotalDebt_ - LongTermDebtTotal_ )) / ( LongTermDebtTotal_ + ( TotalDebt_ - LongTermDebtTotal_ ) + StockholdersEquity_ )
by id: gen DividendsK = DivPaidTotal_ / LagPPE
by id: gen CashK = CashShortTermInvestments_ / LagPPE

*Calculating KZ
by id: gen KZ = (-1.001909 * CashFlowK + 0.2826389 * Q +3.139193 * DebtTotalCapital - 39.3678 * DividendsK - 1.314759 * CashK )

sum CashFlowK Q DebtTotalCapital DividendsK CashK SA KZ
*Correlation
pwcorr SA KZ, sig
correlate SA KZ

*Regression dataset
keep id year TotalAssets_ MarketCapitalization_ StockholdersEquity_ DivPaidTotal_ NetIncomeReal_ SA KZ LagTA

* Preparing Variables for regression
sort id year
by id: gen AssetGrowth = ( TotalAssets_ -  LagTA) / LagTA
by id: gen MarkettoBookRatio = ( MarketCapitalization_ ) / StockholdersEquity_
by id: gen PayoutRatio = DivPaidTotal_ / NetIncomeReal_
by id: gen ROA = NetIncomeReal_ / TotalAssets_

keep id year SA KZ MarkettoBookRatio AssetGrowth PayoutRatio ROA

ssc install outreg2
ssc install ftools
ssc install reghdfe
cd "C:\Users\B51528\Documents"

* Winsoring
ssc install winsor2
winsor2 SA KZ MarkettoBookRatio AssetGrowth PayoutRatio ROA , cuts (5 95)

* Trim

* Store the 5th and 95th percentile values in locals
sum SA, d
local p5 = r(p5)
local p95 = r(p95)
gen SAtrim = SA if SA >= `p5' & SA <= `p95'

sum KZ, d
local p5 = r(p5)
local p95 = r(p95)
gen KZtrim = KZ if KZ >= `p5' & KZ <=`p95'

sum AssetGrowth, d
local p5 = r(p5)
local p95 = r(p95)
gen AssetGrowthtrim = AssetGrowth if AssetGrowth >= `p5' & AssetGrowth <=`p95'

sum MarkettoBookRatio, d
local p5 = r(p5)
local p95 = r(p95)
gen MarkettoBookRatiotrim = MarkettoBookRatio if MarkettoBookRatio >= `p5' & MarkettoBookRatio <=`p95'

sum PayoutRatio, d
local p5 = r(p5)
local p95 = r(p95)
gen PayoutRatiotrim = PayoutRatio if PayoutRatio >= `p5' & PayoutRatio <=`p95'

sum ROA, d
local p5 = r(p5)
local p95 = r(p95)
gen ROAtrim = ROA if ROA >= `p5' & ROA <=`p95'



* AssetGrowth SA
tsset id year
reg AssetGrowth SA
outreg2 using AssetGrowth_SA2, word dec(2) replace
xtset id year
reghdfe AssetGrowth SA, absorb(year id) vce(cluster id)
outreg2 using AssetGrowth_SA2, word dec(2) append
reghdfe AssetGrowth_w SA_w, absorb(year id) vce(cluster id)
outreg2 using AssetGrowth_SA2, word dec(2) append
reghdfe AssetGrowthtrim SAtrim, absorb(year id) vce(cluster id)
outreg2 using AssetGrowth_SA2, word dec(2) append

*MarkettoBookRatio SA
tsset id year
reg MarkettoBookRatio SA
outreg2 using MarkettoBookRatio_SA2, word dec(2) replace
xtset id year
reghdfe MarkettoBookRatio SA, absorb(year id) vce(cluster id)
outreg2 using MarkettoBookRatio_SA2, word dec(2) append
reghdfe MarkettoBookRatio_w SA_w, absorb(year id) vce(cluster id)
outreg2 using MarkettoBookRatio_SA2, word dec(2) append
reghdfe MarkettoBookRatiotrim SAtrim, absorb(year id) vce(cluster id)
outreg2 using MarkettoBookRatio_SA2, word dec(2) append

*PayoutRatio SA
tsset id year
reg PayoutRatio SA
outreg2 using PayoutRatio_SA2, word dec(2) replace
xtset id year
reghdfe PayoutRatio SA, absorb(year id) vce(cluster id)
outreg2 using PayoutRatio_SA2, word dec(2) append
reghdfe PayoutRatio_w SA_w, absorb(year id) vce(cluster id)
outreg2 using PayoutRatio_SA2, word dec(2) append
reghdfe PayoutRatiotrim SAtrim, absorb(year id) vce(cluster id)
outreg2 using PayoutRatio_SA2, word dec(2) append

*ROA SA
tsset id year
reg ROA SA
outreg2 using ROA_SA2, word dec(2) replace
xtset id year
reghdfe ROA SA, absorb(year id) vce(cluster id)
outreg2 using ROA_SA2, word dec(2) append
reghdfe ROA_w SA_w, absorb(year id) vce(cluster id)
outreg2 using ROA_SA2, word dec(2) append
reghdfe ROAtrim SAtrim, absorb(year id) vce(cluster id)
outreg2 using ROA_SA2, word dec(2) append

*AssetGrowth KZ
tsset id year
reg AssetGrowth KZ
outreg2 using AssetGrowth_KZ2, word dec(2) replace
xtset id year
reghdfe AssetGrowth KZ, absorb(year id) vce(cluster id)
outreg2 using AssetGrowth_KZ2, word dec(2) append
reghdfe AssetGrowth_w KZ_w, absorb(year id) vce(cluster id)
outreg2 using AssetGrowth_KZ2, word dec(2) append
reghdfe AssetGrowthtrim KZtrim, absorb(year id) vce(cluster id)
outreg2 using AssetGrowth_KZ2, word dec(2) append

*MarkettoBookRatio KZ
tsset id year
reg MarkettoBookRatio KZ
outreg2 using MarkettoBookRatio_KZ2, word dec(2) replace
xtset id year
reghdfe MarkettoBookRatio KZ, absorb(year id) vce(cluster id)
outreg2 using MarkettoBookRatio_KZ2, word dec(2) append
reghdfe MarkettoBookRatio_w KZ_w, absorb(year id) vce(cluster id)
outreg2 using MarkettoBookRatio_KZ2, word dec(2) append
reghdfe MarkettoBookRatiotrim KZtrim, absorb(year id) vce(cluster id)
outreg2 using MarkettoBookRatio_KZ2, word dec(2) append

*PayoutRatio KZ
tsset id year
reg PayoutRatio KZ
outreg2 using PayoutRatio_KZ2 , word dec(2) replace
xtset id year
reghdfe PayoutRatio KZ, absorb(year id) vce(cluster id)
outreg2 using PayoutRatio_KZ2, word dec(2) append
reghdfe PayoutRatio_w KZ_w, absorb(year id) vce(cluster id)
outreg2 using PayoutRatio_KZ2, word dec(2) append
reghdfe PayoutRatiotrim KZtrim, absorb(year id) vce(cluster id)
outreg2 using PayoutRatio_KZ2, word dec(2) append

*ROA KZ
tsset id year
reg ROA KZ
outreg2 using ROA_KZ2, word dec(2) replace
xtset id year
reghdfe ROA KZ, absorb(year id) vce(cluster id)
outreg2 using ROA_KZ2, word dec(2) append
reghdfe ROA_w KZ_w, absorb(year id) vce(cluster id)
outreg2 using ROA_KZ2, word dec(2) append
reghdfe ROAtrim KZtrim, absorb(year id) vce(cluster id)
outreg2 using ROA_KZ2, word dec(2) append


xtset id year

* Quintiles Winsored

* Quintiles
xtile KZ_quintiles_w = KZ_w, nq(5)
xtile SA_quintiles_w = SA_w, nq(5)

reghdfe AssetGrowth_w i.SA_quintiles_w, absorb(year id) vce(cluster id)
outreg2 using AG_quintile_W, word dec(2) replace
reghdfe AssetGrowth_w i.KZ_quintiles_w, absorb(year id) vce(cluster id)
outreg2 using AG_quintile_W, word dec(2) append

reghdfe MarkettoBookRatio_w i.SA_quintiles_w, absorb(year id) vce(cluster id)
outreg2 using MTB_quintile_W, word dec(2) replace
reghdfe MarkettoBookRatio_w i.KZ_quintiles_w, absorb(year id) vce(cluster id)
outreg2 using MTB_quintile_W, word dec(2) append

reghdfe PayoutRatio_w i.SA_quintiles_w, absorb(year id) vce(cluster id)
outreg2 using PR_quintile_W, word dec(2) replace
reghdfe PayoutRatio_w i.KZ_quintiles_w, absorb(year id) vce(cluster id)
outreg2 using PR_quintile_W, word dec(2) append

reghdfe ROA_w i.SA_quintiles_w, absorb(year id) vce(cluster id)
outreg2 using ROA_quintile_W, word dec(2) replace
reghdfe ROA_w i.KZ_quintiles_w, absorb(year id) vce(cluster id)
outreg2 using ROA_quintile_W, word dec(2) append














