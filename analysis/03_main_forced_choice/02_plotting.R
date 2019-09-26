source("analysis/03_main_forced_choice/01_preparation-analysis.R")

# overall graph
dat$cond <- factor(dat$cond, levels=c("think", "announce"))
dat.subj <- 100*tapply(dat$isHigh, list(dat$cond, dat$output_id), mean)
mean.subj <- apply(dat.subj, 1, mean, na.rm=T)
print(mean.subj)
# calculate the standard deviation
sd.subj <- apply(dat.subj, 1, sd, na.rm=T)
# calculate the lengths (number subjects)
ns.subj <- apply(dat.subj, 1, length)
# calculate the standard errors
ses <- sd.subj/sqrt(ns.subj)

par(mar = c(4, 6, 1, 6) + 2)
xs <- barplot(mean.subj, beside=T,  ylab=c("% higher value selected"),ylim=c(0,100), cex.lab=1.5, xlab=c("cond"),  font=2, font.sub=3, las=1, legend=F, col=c("darkorange", "red"))
par(xpd = TRUE)
# plot the error bars
errbar(xs, mean.subj, mean.subj+ses, mean.subj-ses, add=T, lwd=1.5, pch=26, cap=.05)
