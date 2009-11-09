###################################################
### chunk number 1: setValueSoUsersCanLookAtAllPlots
###################################################
options(device.ask.default=TRUE)


###################################################
### chunk number 2: LoadTriangleForStandardModel
###################################################
library(lossDev) #load the library

## load the triangle, "data" loads the triangle as a data.frame so it must be coerced into a matrix
data(IncrementalGeneralLiablityTriangle)
IncrementalGeneralLiablityTriangle <- as.matrix(IncrementalGeneralLiablityTriangle)
#print(IncrementalGeneralLiablityTriangle[(-5:0) + dim(IncrementalGeneralLiablityTriangle)[1], 1:6])
print(IncrementalGeneralLiablityTriangle)



###################################################
### chunk number 3: LoadCPIForStandardModel
###################################################
## load the stochastic inflation series, "data" loads the series as a data.frame so it must be coerced into a vector
data(CPI)
CPI <- as.matrix(CPI)[,1]
CPI.rate <- CPI[-1] / CPI[-length(CPI)] - 1
CPI.rate.length <- length(CPI.rate)
print(CPI.rate[(-10):0 + CPI.rate.length])

##restrict the cpi to only those years available when the triangle created
CPI.years <- as.integer(names(CPI.rate))
years.available <- CPI.years <= max(as.integer(dimnames(IncrementalGeneralLiablityTriangle)[[1]]))

CPI.rate <- CPI.rate[years.available]
CPI.rate.length <- length(CPI.rate)
print(CPI.rate[(-10):0 + CPI.rate.length])



###################################################
### chunk number 4: CreateTheStandardModelInputObject
###################################################
standard.model.input <- makeStandardAnnualInput(incremental.payments = IncrementalGeneralLiablityTriangle,
                                                stoch.inflation.weight = 1,
                                                non.stoch.inflation.weight = 0,
                                                stoch.inflation.rate = CPI.rate,
                                                exp.year.type = 'ay',
                                                extra.dev.years=5,
                                                use.skew.t=TRUE)




###################################################
### chunk number 5: EstiamteTheStandardModel
###################################################
standard.model.output <- runLossDevModel(standard.model.input,
                                         burnIn=30.0E+3,
                                         sampleSize=40.0E+3,
                                         thin=10)





###################################################
### chunk number 6: plotStandardexposureGrowthTrace
###################################################
exposureGrowthTracePlot(standard.model.output)



###################################################
### chunk number 7: plotStandardConsumptionPathTrace
###################################################
consumptionPathTracePlot(standard.model.output)



###################################################
### chunk number 8: plotStandardConsumptionPathTrace
###################################################
calendarYearEffectErrorTracePlot(standard.model.output)



###################################################
### chunk number 9: standardResiByDevYear
###################################################
triResi(standard.model.output, timeAxis='dy')



###################################################
### chunk number 10: standardResiByExpYear
###################################################
triResi(standard.model.output, timeAxis='ey')



###################################################
### chunk number 11: standardResiByCalYear
###################################################
triResi(standard.model.output, timeAxis='cy')



###################################################
### chunk number 12: standardQQ
###################################################
QQPlot(standard.model.output)



###################################################
### chunk number 13: standardfinalCumulativeDiff
###################################################
finalCumulativeDiff(standard.model.output)



###################################################
### chunk number 14: standardPredictedInc
###################################################
predictedPayments(standard.model.output,
                  type='incremental',
                  logScale=TRUE)


###################################################
### chunk number 15: standardPredictedCumul
###################################################
standard.ult <- predictedPayments(standard.model.output,
                                  type='cumulative',
                                  plotObservedValues=FALSE,
                                  mergePredictedWithObserved=TRUE,
                                  logScale=TRUE,
                                  quantiles=c(0.025, 0.5, 0.0975),
                                  plot=FALSE)
standard.ult <- standard.ult[,,dim(standard.ult)[3]]
print(standard.ult)



###################################################
### chunk number 16: standardPredictedCumul
###################################################
predictedPayments(standard.model.output,
                  type='cumulative',
                  plotObservedValues=FALSE,
                  logScale=TRUE)



###################################################
### chunk number 17: standardConsumptionPath
###################################################
consumptionPath(standard.model.output)




###################################################
### chunk number 18: standardNumberOfKnots
###################################################
numberOfKnots(standard.model.output)




###################################################
### chunk number 19: standardRateOfDecay
###################################################
rateOfDecay(standard.model.output)




###################################################
### chunk number 20: standardExposureYearGrowth
###################################################
exposureGrowth(standard.model.output)




###################################################
### chunk number 21: standardCalendarYearEffect
###################################################
calendarYearEffect(standard.model.output)




###################################################
### chunk number 22: standardCalendarYearEffectErrors
###################################################
calendarYearEffectErrors(standard.model.output)




###################################################
### chunk number 23: standardSTDvsDev
###################################################
standardDeviationVsDevelopmentTime(standard.model.output)




###################################################
### chunk number 24: standardSkew
###################################################
skewnessParameter(standard.model.output)




###################################################
### chunk number 25: standardDegreesOfFreedom
###################################################
degreesOfFreedom(standard.model.output)




###################################################
### chunk number 26: standardStochInflation
###################################################
stochasticInflation(standard.model.output)




###################################################
### chunk number 27: standardStochInflationStationaryMean
###################################################
stochasticInflationStationaryMean(standard.model.output)




###################################################
### chunk number 28: standardStochInflationRhoParameter
###################################################
stochasticInflationRhoParameter(standard.model.output)




###################################################
### chunk number 29: cleanUpWorkSpace
###################################################
rm(list=ls())


###################################################
### chunk number 30: lossTriangleForBreakModel
###################################################
data(CumulativeAutoBodilyInjuryTriangle)
CumulativeAutoBodilyInjuryTriangle <- as.matrix(CumulativeAutoBodilyInjuryTriangle)
sample.col <- (dim(CumulativeAutoBodilyInjuryTriangle)[2] - 6:0)
print(decumulate(CumulativeAutoBodilyInjuryTriangle)[1:7, sample.col])



###################################################
### chunk number 31: mcpiForBreakModel
###################################################

data(MCPI)
MCPI <- as.matrix(MCPI)[,1]
MCPI.rate <- MCPI[-1] / MCPI[-length(MCPI)] - 1
print(MCPI.rate[(-10):0 + length(MCPI.rate)])

MCPI.years <- as.integer(names(MCPI.rate))
max.exp.year <- max(as.integer(dimnames(CumulativeAutoBodilyInjuryTriangle)[[1]]))
years.to.keep <- MCPI.years <=  max.exp.year + 3
MCPI.rate <- MCPI.rate[years.to.keep]



###################################################
### chunk number 32: CreatingTheBreakInputObjectl
###################################################
break.model.input <- makeBreakAnnualInput(cumulative.payments = CumulativeAutoBodilyInjuryTriangle,
                                          stoch.inflation.weight = 1,
                                          non.stoch.inflation.weight = 0,
                                          stoch.inflation.rate = MCPI.rate,
                                          first.year.in.new.regime = c(1986, 1987),
                                          priors.for.first.year.in.new.regime=c(2,1),
                                          exp.year.type = 'ay',
                                          extra.dev.years = 5,
                                          use.skew.t = TRUE,
                                          bound.for.skewness.parameter=5)


###################################################
### chunk number 33: EstimateTheBreakModel
###################################################
break.model.output <- runLossDevModel(break.model.input,
                                      burnIn=30.0E+3,
                                      sampleSize=40.0E+3,
                                      thin=10)



###################################################
### chunk number 34: plotBreakConsumptionPathTracePreBreak
###################################################
consumptionPathTracePlot(break.model.output, preBreak=TRUE)



###################################################
### chunk number 35: plotBreakConsumptionPathTracePostBreak
###################################################
consumptionPathTracePlot(break.model.output, preBreak=FALSE)



###################################################
### chunk number 36: breakResiDevYearStandardized
###################################################
triResi(break.model.output, timeAxis='dy')



###################################################
### chunk number 37: breakResiDevYearUnStandardized
###################################################
triResi(break.model.output, standardize=FALSE, timeAxis='dy')



###################################################
### chunk number 38: breakfinalCumulativeDiff
###################################################
finalCumulativeDiff(break.model.output)



###################################################
### chunk number 39: breakPredictedInc
###################################################
predictedPayments(break.model.output,
                  type='incremental',
                  logScale=TRUE)


###################################################
### chunk number 40: breakConsumptionPath
###################################################
consumptionPath(break.model.output)




###################################################
### chunk number 41: breakNumberOfKnots
###################################################
numberOfKnots(break.model.output)




###################################################
### chunk number 42: breakRateOfDecay
###################################################
rateOfDecay(break.model.output)




###################################################
### chunk number 43: breakCalendarYearEffect
###################################################
calendarYearEffect(break.model.output)




###################################################
### chunk number 44: breakCalendarYearEffectErrors
###################################################
calendarYearEffectErrors(break.model.output)




###################################################
### chunk number 45: breakCalendarYearEffectErrorsAR1WithAR1
###################################################

break.model.input.w.ar1 <- makeBreakAnnualInput(cumulative.payments = CumulativeAutoBodilyInjuryTriangle,
                                                stoch.inflation.weight = 1,
                                                non.stoch.inflation.weight = 0,
                                                stoch.inflation.rate = MCPI.rate,
                                                first.year.in.new.regime = c(1986, 1987),
                                                priors.for.first.year.in.new.regime=c(2,1),
                                                exp.year.type = 'ay',
                                                extra.dev.years = 5,
                                                use.skew.t = TRUE,
                                                bound.for.skewness.parameter=5,
                                                use.ar1.in.calendar.year = TRUE)
break.model.output.w.ar1 <- runLossDevModel(break.model.input.w.ar1,
                                            burnIn=30.0E+3,
                                            sampleSize=40.0E+3,
                                            thin=10)
calendarYearEffectErrors(break.model.output.w.ar1)



###################################################
### chunk number 46: breakAutoregressiveParameter
###################################################

autoregressiveParameter(break.model.output.w.ar1)


###################################################
### chunk number 47: cleanUpBreakAR1
###################################################
rm(break.model.output.w.ar1, break.model.input.w.ar1)


###################################################
### chunk number 48: breakSkew
###################################################
skewnessParameter(break.model.output)




###################################################
### chunk number 49: breakFirstYearInNewRegime
###################################################
firstYearInNewRegime(break.model.output)




###################################################
### chunk number 50: accountForZeros
###################################################
break.model.output.w.zeros <- accountForZeroPayments(break.model.output)


###################################################
### chunk number 51: zeroPaymentsScale
###################################################
gompertzParameters(break.model.output.w.zeros, parameter='scale')




###################################################
### chunk number 52: zeroPayments5050
###################################################
gompertzParameters(break.model.output.w.zeros, parameter='fifty.fifty')




###################################################
### chunk number 53: zeroPaymentsCurve
###################################################
probablityOfPayment(break.model.output.w.zeros)




###################################################
### chunk number 54: breakPredictedIncWithZeros
###################################################
predictedPayments(break.model.output.w.zeros,
                  type='incremental',
                  logScale=TRUE)


