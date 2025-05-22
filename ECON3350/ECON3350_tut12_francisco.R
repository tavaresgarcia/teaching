# Tutorial 12: Multivariate Processes - III

# Answers by Eric Eisenstat
# https://sites.google.com/view/ericeisenstat

# Extra material by Francisco Tavares Garcia
# https://tavaresgarcia.github.io/


# House cleaning
rm(list = ls()) # clear Environment
if (!is.null(dev.list())) dev.off() # clear Plots
cat("\014") # clear Console

# For this tutorial, we need to load these packages:
# install.packages("dplyr")
# install.packages("zoo")
# install.packages("vars")
install.packages("urca")

library(dplyr)
library(zoo)
library(vars)
library(urca)

# Set working directory. To use the following line, save this script in the same
# folder as the data files.
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load the data in term_structure.csv
mydata <- read.delim("term_structure.csv", header = TRUE,  sep = ",")

dates <- as.yearmon(mydata$obs, format = "%YM%m")
T <- length(dates)

i90d <- mydata$I90D[-T]
i180d <- mydata$I180D[-T]
i3y <- mydata$I3Y[-T]
i5y <- mydata$I5Y[-T]
x <- cbind(i90d, i180d, i3y, i5y)

n <- ncol(x) # Number of columns (variables) in x

# 1.
#
# We will consider VARs with p= 1,...,20.
# Note: the VAR command does not allow estimating a VAR(0), so we will not worry
# about this specification.
VAR_est <- list()
ic_var <- matrix(nrow = 20,  ncol = 3)
colnames(ic_var) <- c("p", "aic", "bic")
for (p in 1:20){
  VAR_est[[p]] <- VAR(x, p)
  ic_var[p,] <- c(p, AIC(VAR_est[[p]]), BIC(VAR_est[[p]]))
}

ic_aic_var <- ic_var[order(ic_var[,2]),]
ic_bic_var <- ic_var[order(ic_var[,3]),]

# AIC and BIC have p = 2, 3, 4, 5 in the top 5;
# the rest are quite a bit inferior, so we will go with these.
adq_set_var <- as.matrix(ic_var[2:5,])
adq_idx_var <- c(2:5)

# Check the residuals: the vars package provides a few different tests through
# the serial.test function; we will use the LM test by setting type = "BG", but
# other tests are just as valid.
nmods <- length(adq_idx_var)
for (i in 1:nmods){
  p <- adq_idx_var[i]
  print(paste0("Checking VAR(", p, ")"))
  print(serial.test(VAR_est[[p]], lags.bg = 1, type = "BG"))
}

# Autocorrelations appear to be quite high for p = 2, so we remove it from the
# set. For p = 4, the p-value is lower than for p = 3 and p = 5. This is odd, so
# we won't remove p = 4 from the set, but will proceed with caution.

adq_set_var <- as.matrix(ic_var[3:5,])
adq_idx_var <- c(3:5)

# 2.
#
# We use the function "roots" to ascertain the stability of the estimated VARs.
nmods <- length(adq_idx_var)
for (i in 1:nmods){
  p <- adq_idx_var[i]
  print(paste0("VAR(", p, "): Maximum absolute eigenvalue is ", 
               max(vars::roots(VAR_est[[p]]))))
}

# We have at least one root that is close to a unit root, so we might consider
# imposing rank restrictions using the VECM representation. The package urca
# provides functions for working with VECMs, and in particular, the function
# ca.jo, which implements Johansen's trace and maximum eigenvalue tests. We will
# focus on the trace test in this exercise.
nmods <- length(adq_idx_var)
for (i in 1:nmods){
  p <- adq_idx_var[i]
  print(paste0("VAR(", p, ")"))
  print(summary(ca.jo(x, type = "trace", K = p)))
}

# Let us denote by VECM(p, r) a specification corresponding to a VAR(p), but
# with rank A(1) = r imposed. Recall that in the VECM form of a VAR(p), there
# are actually only p - 1 lags in the differenced series.

# For each VAR(p), p = 3, 4, 5, we obtain nearly identical inference regarding
# r. Namely, we can sequentially test:
#
# (test 1) H0: r = 0 vs H1: r >= 1 --> reject H0 at 1%
# (test 2) H0: r = 1 vs H1: r >= 2 --> reject H0 at 1%
# (test 3) H0: r = 2 vs H1: r >= 3 --> fail to reject H0 at 5%

# sequential testing in this order is justifiable; specifically, if we reject H0
# in test1, then we conclude that r is between 1 and 4; hence, it is not a
# contradiction to follow this test 2, in which both H0 and H1 are in the same
# set r = {1, 2, 3, 4} we ended up with after test 1; note that this is not as
# clear when the max eigenvalue test is used...

# importantly: when we fail to reject H0 r = 2 against H1: r >= 3 in test 3,
# this NOT evidence that r = 2 -- there is no justification to accept H0; the
# only reasonable conclusion is that VECM with r = 2 is NOT empirically
# distinguishable from VECMs with r = 3 or r = 4.

# However, since we cannot reject specifications with r < 4, this suggests that
# it may be reasonable to consider VECMs with r = 2 and r = 3 as alternative
# specifications to each VAR(p). In our notation this is VECM(p, 4) (which again
# is equivalent to the VECM with r = 4 and p - 1 differenced lags)

# 3.
#
# There is not a huge computational cost to consider a general class of VECM
# models with p = 3,...,6 and r = 0,...,4, so this is what we'll do. The
# information obtained in Q1/Q2 -- starting with a class of unrestricted VARs,
# then using Johansen's trace test to obtain inference on r -- can be useful to
# either reduce the overall number of VECMs we search over or to refine the
# adequate set of VECMs we end up constructing using the general search.

VECM_est <- list()
ic_vecm <- matrix(nrow = 4 * (1 + n),  ncol = 4)
colnames(ic_vecm) <- c("p", "r", "aic", "bic")
i <- 0
for (p in 3:6){
  for (r in 0:n){
    i <- i + 1
    if (r == n){
      VECM_est[[i]] <- VAR(x, p)
    }
    else if (r == 0){
      VECM_est[[i]] <- VAR(diff(x), p - 1)
    }
    else{
      VECM_est[[i]] <- vec2var(ca.jo(x, K = p), r)
    }
    ic_vecm[i,] <- c(p, r, AIC(VECM_est[[i]]),
                           BIC(VECM_est[[i]]))
  }
}

ic_aic_vecm <- ic_vecm[order(ic_vecm[,3]),][1:10,]
ic_bic_vecm <- ic_vecm[order(ic_vecm[,4]),][1:10,]

# find the intersection of AIC and BIC preferred sets
ic_int_vecm <- intersect(as.data.frame(ic_aic_vecm),
                         as.data.frame(ic_bic_vecm))

# the intersecting set has combinations of p = 3, 4 with r = 2, 3, 4;
# AIC and BIC values look comparable so we will proceed with this as the
# adequate set
adq_set_vecm <- as.matrix(arrange(as.data.frame(
                                  ic_int_vecm), p, r))
adq_idx_vecm <- match(data.frame(t(adq_set_vecm[, 1:2])),
                           data.frame(t(ic_vecm[, 1:2])))

# do a final check of the residuals
nmods <- length(adq_idx_vecm)
for (i in 1:nmods){
  p <- adq_set_vecm[i, 1]
  r <- adq_set_vecm[i, 2]
  print(paste0("Checking VECM(", p, ", ", r, ")"))
  print(serial.test(VECM_est[[adq_idx_vecm[i]]], lags.bg = 1, type = "BG"))
}

# Residuals analysis is similar to what we found with VAR(3) and VAR(4) models
# in Q1. This is to be expected -- VECM and VAR representations for the same p
# theoretically have the same residuals, so autocorrelation analysis should not
# be very different.
# We proceed with this as the adequate set, but cautious of the lower p-values
# produced for the residuals of VECM(4, r) models.

# 4.
#
# We will use p = 3, but the same exercise can be carried out with p = 4. The
# urca package provides an estimation routine for VECMs called cajorls().
# However, it does not provide standard errors for the estimated coefficients in
# the equilibrium relationships. This is unfortunately a recurring issue in
# software packages!
#
# We need to construct some measures of uncertainty manually. One way to do this
# is by using the LR test and testing restrictions on the beta matrix one
# coefficient at a time. The function blrtest can be used for this as follows.
p <- 3
for (r in 2:4){
  if (r < n){
    vec_pr <- ca.jo(x, type = "trace", spec = "transitory")
    beta_pr <- round(cajorls(vec_pr, r)$beta,4)
    
    bpvals_pr <- beta_pr
    bpvals_pr[,] <- NA
    
    for (i in (r + 1):n){
      for (j in 1:r){
        H <- beta_pr
        H[i, j] <- 0
        bpvals_pr[i, j] <- round(blrtest(vec_pr, H, r)@pval[1],4)
      }
    }
    
    print(paste0("Results for VECM(", p, ", ", r, "):"))
    print(beta_pr)
    print(bpvals_pr)
  }
}

# r=2: this is the case where we have two stochastic trends, and by Granger
# representation theorem, at least TWO variables MUST be I(1). But which???
#
# Recall from Tutorial 6 that we found evidence i90d is I(0). If this is the
# case, then the first equilibrium relationship, labelled "ect1", suggests that
# i90d, i3y and i5y are related in equilibrium. Consequently, either i3y and i5y
# are both I(0) OR they are both I(1) and cointegrated. If both were I(0), then
# we would have three I(0) variables, which contradicts the fact that we must
# have at least TWO I(1) variables. Hence, the only implication is that i3y and
# i5y are cointegrated.

# The second equilibrium relationship, labelled "ect2", suggests that i180d, i3y
# and i5y are related in equilibrium. Since i3y and i5y must be cointegrated,
# i180d cannot be I(0) -- if that was true, then there would be only one common
# stochastic trend, which again contradicts the fact that we must have two
# stochastic trends.

# This leaves us with the possibility that I180d, i3y and i5y are all I(1) and
# cointegrated. However, in this case we have inferred two linearly independent
# cointegrating relations, which again means that there is only one stochastic
# trend present.

# We see that if we start with i90d being I(0) and r=2, then we always arrive at
# a contradiction! This means that our inference from the VECM with r=2 leads us
# to conclude that i90d must be I(1), which is clearly not compatible with the
# inference obtained from ADF tests that lead to concluding i90d is I(0) with a
# high degree of confidence!

# r=3: this is the case where we have one stochastic trend, and by Granger
# representation theorem, at least one variable MUST be I(1). Again, if we
# assume i90d is I(0), then i5y must also be I(0) from equilibrium relationship
# "ect1". This DOES NOT contradict our inference from ADF tests, where we could
# not obtain conclusive evidence.

# Following on to equilibrium relationship "ect2", we see that i180d and i5y
# must also be in equilibrium, and hence, if i5y is I(0) so must be 180d. Then,
# because there must exist at least one variable that is I(1), this variable
# must be i3y. Unfortunately, equilibrium relationship "ect3" leads us to
# conclude that i3y and i5y are in equilibrium with a high degree of confidence.
# This is again a contradiction since there cannot exist an equilibrium between
# an I(0) variable and an I(1) variable.

# It looks like imposing rank restrictions on the VAR leads us to inference that
# is NOT compatible with the inference we obtained from ADF tests.

# 5.
#
vnames <- c("i90d", "i180d", "i3y", "i5y")
nmods <- length(adq_idx_vecm)
for (i in 1:4){
  for (j in 1:4){
    for (imod in 1:nmods){
      p <- adq_set_vecm[imod, 1]
      r <- adq_set_vecm[imod, 2]
      title_i_j <- paste0("VECM(", p, ", ", r,
                          "): Response of ", vnames[i],
                          " to a shock in ", vnames[j])
  
      irf_i_j <- irf(VECM_est[[adq_idx_vecm[imod]]],
                            n.ahead = 60,
                            response = vnames[i],
                            impulse = vnames[j],
                            boot = TRUE)
          
      plot(irf_i_j, main = title_i_j)
      cat("\r", title_i_j, "  ", sep = "")
    }
  }
}

# What really stands out from these plots is how sensitive they are to
# specifications in terms of r. This underscores the pitfall of blindly relying
# on a naive approach and ignoring specification uncertainty.

# For example, consider the responses of i5y. Looking first at the responses to
# i90d specific shocks, we find that short run responses (up to around 8
# quarters) are generally similar across different specifications. However,
# long-run responses are drastically different for r=2 compared to r=3 and r=4.
# A similar story emerges in the responses of i5y to 180d as well.

# Now, if we consider the naive approach of choosing a particular r based on the
# rule that it is the lowest rank for which H0 cannot be rejected by Johansen's
# trace test, we would select r=2 and ignore completely r=3 and r=4 as possibly
# valid specifications. This makes it clear how we may be led quite astray in
# our inference. Ignoring the responses with r=3 and r=4 would be quite
# misleading.

# Interestingly, responses of i5y to i3y are more similar between the r=2 and
# r=3 specifications, but differ in the long-run response obtained from the
# unrestricted (r=4) specification. Again, selecting any one "r" and ignoring
# uncertainty associated with alternative rank specifications can be very
# detrimental!

# Being transparent and interpreting results in light of this uncertainty is far
# more informative. In this case, we can generally conclude that inference on
# short-run responses entails a higher degree of confidence, but long-run
# responses are subject to substantial specification uncertainty.