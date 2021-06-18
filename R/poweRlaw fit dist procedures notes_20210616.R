########## Checking xmin
# PoweRlaw functions
# to estimate xmin: "estimate_xmin"
#   * lots going on here but uses "get_distance_statistic" to made cdfs
#   * "get_distance_statistic" calls "dist_cdf" to get fit cdf, which is used to get data cdf
#   * after calculating cdfs, "get_gof" is called to do KS test
#



fx = 1-exp(-pars.list[i]*(xi-xmin))
fx[xi<xmin] = 0
sx <- ((0:(n - 1))/n)[1:length(fx)]
dat[i] <- max(abs(sx-fx), na.rm=TRUE)
