# Vauclin (1979) experiment with SHUD mdoel

***SHUD - Solver of Hydrological Unstructured Domain.*** 

Website: [www.shud.xyz](www.shud.xyz)

Author: Lele Shu [www.shulele.net](www.shulele.net)

## Vauclin's experiment

Vauclin's experiment \citep{Vauclin1979} is designed to assess groundwater table change and soil moisture in the unsaturated layer under precipitation or irrigation.  The experiment was conducted in a sandbox with dimension $3$ m long $\times 2$ m deep $\times 0.05$ m wide (see Fig. \ref{fig:vauclin}). The box was filled with uniform sand particles with measured hydraulic parameters: the saturated hydraulic conductivity was $35$ cm/hr and porosity was $0.33$ m$^3$/m$^3$. The left and bottom of the sandbox were impervious layers, and the top and the right side were open. A hydraulic head was set constant at $0.65 m$. Constant irrigation ($1.48$ cm/hr) was applied over the first $50​$ cm of the top-left of the sandbox while the rest of the top was covered to avoid water loss via evaporation.

![Vauclin](Ref/Vauclin.png)

The experiment's initial condition is an equilibrium water table under constant hydraulic head from the right side.  That is, the saturated water table across the sandbox was kept stable at $0.65$ m. When the groundwater table reached equilibrium, irrigation was initiated at $t = 0$.  The groundwater table was then measured at 2, 4, 6, and 8 hours at several locations along the length of the box.

![res](Ref/v1.png)

Besides the parameters specified in \citep{Vauclin1979}, additional information is needed by the SHUD, including the $\alpha$ and $\beta$ in the van Genutchen equation and  water content ($\theta _s$). Therefore,  we use a calibration tool to estimate the representative values of these parameters.  The use of calibration in this simulation is reasonable because the model -- inevitably -- simplifies the real hydraulic processes. The calibration thus nudges the parameters to \emph{representative} values that approach or fit the \emph{true} natural processes.  The calibrated values are  $\theta _s = 0.32 m^3/m^3$, $\alpha = 6.0$ and $\beta = 6.0​$.  Like the simulated results in \citep{Vauclin1979} and \citep{Shen2010}, a mismatch exists between the simulations and measurements.

This mismatch may be due to (1) the aquifer description of unsaturated and saturated layers limiting the capability to simulate infiltration and recharge in the unsaturated zone, or (2) the horizontal unsaturated flow assumptions no longer hold at the relatively  microscopic scales of this experiment.

The SHUD simulated the groundwater table at all four measurement points (see Fig. \ref{fig:vauclin}(b).  The maximum bias between simulation and Vauclin's observations is $ 4.6cm$, with $R^2$ = $0.99$. When the calibration takes more soil parameters into account, the bias in simulation decreases to  $3 cm$. Certainly, the simplifications employed by SHUD for the unsaturated and saturated zone benefits the computation efficiency while limiting the applicability of the model for micro-scale problems.

The simulations, compared against Vauclin's experiment, validate the algorithm for infiltration, recharge, and lateral groundwater flow.  More reliable vertical flow within unsaturated layer requires multiple layers, which is planned in next version of SHUD.



## R scripts

The R scripts include:

1. Build the Vauclin experiment
2. Generate the physical and model parameters
3. Run simulations.
4. Visulize results
5. Compare the result with Vauclin measurements.



## Data

1. Input files for SHUD model.
2. Result files from SHUD model.
3. Digitalized data from Vauclin (1979).



## Policy

The script and data are open access for any purposes. I would be most grateful if you could send me an email (at lele.shu@gmail.com) when they helps you.

