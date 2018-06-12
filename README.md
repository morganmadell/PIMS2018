# PIMS 2018
PIMS Industrial Problem Solving Workshop: Stochastic modelling of oil and gas economics


Many applications of SDEs are of interest to oil and gas economic problems.  As most economic analysis is currently conducted using deterministic discounted cash flow models, there is much scope of innovation and experimentation.  In particular, there is much scope for improving how uncertainty is mathematically modelled.


### Econometrics

Model construction and estimation of parameters for Canadian energy commodity prices

Most econometric analysis has used used price series external to Canada.  It is common to consider the price of West Texas Intermediate (WTI) oil or of gas traded at Henry Hub.  However, Canadian producers most often receive revenues based on the price of oil at various refineries in Edmonton or on the price of gas at either the AECO trading hub or at Station 2 in British Columbia.  A further complication is the relatively large amount of heavy oil produced within Alberta which is sold at a discount to prices of light oil.  These local hydrocarbon prices are related to each other and to world benchmarks (such as WTI) but do not necessarily share the same variance or other model structures.

Current practice often models Canadian prices to be simple linear functions of world benchmark prices.  However, such models do not allow different mean reversion rates, volatilities or correlations, all of which may be necessary to reflect fundamental physical and economic differences specific to the Canadian market.  Exploring price models suitable for Canadian producers would provide improvements in the ability to properly assess price inter-dependencies and uncertainties.

Data for various Canadian price series can be provided by GLJ Publications.


### Real Options Analysis 

Economics of constructing gas processing plants that extract more liquid hydrocarbons 

In designing gas processing plants an important factor is the degree to which various higher weight hydrocarbons are separated from methane.  This is commonly done by chilling the gas until the higher weight hydrocarbons condense and can be removed as a liquid.  As different hydrocarbons have different boiling points, the relative amount of condensation of each hydrocarbon can be controlled by careful choice of temperature.  Operators often consider increasing the recovery of higher weight hydrocarbons by constructing gas plants that operate at lower temperatures.  This allows the hydrocarbons to be sold as liquids (taking advantage of higher liquids prices) instead of a gas.  However, running gas plants at a lower temperatures increases operating costs.  Moreover, the operating temperature is primarily determined by the design and construction of the plant while the relative value of hydrocarbon liquids to gases is determined by spot an futures markets and is under constant change. 

The choice of plant operating temperature can be viewed as a real option.  The value of this real option will be determined by whether various hydrocarbon species (ethane, propane, butane, pentane, etc) are sold as separate liquids or sold within the gas stream, by the incremental cost of constructing plants capable of operating at lower temperatures and by the incremental costs of operating at those lower temperatures.  As the prices of various hydrocarbon species are interdependent (and likely co-integrated), a suitable commodity price model will likely be needed.

Constructing a model the real option of gas plant operating temperature would improve the ability of investors to weigh the relative merits of various plant designs, thereby maximizing the economic return of investments.


### Geographic Correlation and Temporal Uncertainty

Pipeline supply with discrete correlated declining input sources

Nearly all energy resources are spatially correlated and aggregated using fixed infrastucture.  Whether this is the sunlight available for a solar panel, the wind to power a turbine or productivity of a gas well, high output point sources are likely to be located near other high output sources.  Within the mining industry such correlations are often characterized through the use of various geostatistical descriptions.  Such models often use static outputs: the relative presence of minerals for example.  However, energy supplies often have a temporal quality.  The wind speed changes throughout the day and gas wells typically produce at higher rates early in their lives. Thus, wise investment decisions on fixed infrasture will need to consider these spatial and temporal patterns.

It would be beneficial to have a model for pipeline supply that considers a catchment area within which discrete input sources are located (ie gas wells).  These input sources are not constant: they decline over time according to uncertain spatially correlated parameters.  Such a model should calculate the distribution of number of point sources required, and the pace of adding them, in order to maintain a constant supply to the pipeline.  Such a model should also consider how the distribution changes with increasing certainty, both in terms of the underlying (spatially varying) productivity of the area and in terms of the uncertainty of the decline parameters.

Such a model for pipeline supply would be beneficial when considering development within relatively untested areas, thereby better informing investment decisions and development stratgies.


### SDE Solutions to PDEs



while others represent what I think are novel applications of SDE methods within the industry (estimating the uncertainty in forecasts of hydrocarbon production by solving common reservoir engineering PDE models within an SDE framework).

$$\sqrt{t}$$
