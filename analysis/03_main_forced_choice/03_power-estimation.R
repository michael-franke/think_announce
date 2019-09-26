# this assumes that the data and model fits exist
# we will target 'm3' because it does not seem to contain redundant REs
source("analysis/03_main_forced_choice/01_preparation-analysis.R")
# Make sure NOT  to run 02_plotting.R before as this rearranges factor levels and the following breaks!

library(simr) # for simulating power

# we had 90 participants (nobody excluded?) and 12 items in 2 conditions
# every participant saw all 12 items, so gave 6 observations per cell
# => we had a total of 90 * 6 = 540 observation per design cell (maybe expected value, if randomized?)

# hypothetical difference between conditions (think: effect size 0.4)
fixef(m3)["condthink"] <- -0.5

# extending to 120 participants & 20 items 
m_power_sim = extend(m3, along = "output_id", n = 220) 
m_power_sim = extend(m_power_sim, along = "itemName", n = 20)

powerSim(m_power_sim, nsim = 2)
pc = powerCurve(m_power_sim, nsim = 20, along = "output_id")
plot(pc)
