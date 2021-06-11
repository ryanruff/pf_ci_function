# ==========================================
# Ryan Richard Ruff, MPH, PhD
# New York University
# ==========================================

pfci <- function(a, b, c, d) {
  # a function to calculate preventive fractions from cross-sectional studies.
  # input standard cells of 2x2 table a, b c, d for exposure/outcome
  # a = +/+, b = +/-, c = -/+, d = -/-
  # multiple CI calculations provided, just comment out the ones not needed
  # see Gargiullo et al (1995) for further detail
  n <- a+b+c+d
  pa <- a/n
  pb <- b/n
  pc <- c/n
  pd <- d/n
  # proportion exposed to intervention
  p = pa+pb
  # rr, pf, variance, CIs
  rr <- (pa/(pa+pb))/(pc/(pc+pd))
  pf <- p*(1-rr)
  v <- 1/(n^3)*((a*(n-a)*((d/c)^2))+(b*(n-b))+((a/c)^2)*(d*(((n*(c+d))/c)+((2*b*c)/a))))
  se <- sqrt(v)
  #No transformation
    lcl <- pf-(1.96*se)
    ucl <- pf+(1.96*se)
  #CIs with logit transformation, performs better in simulation for certain conditions
    #lcl <- log(pf/(1-pf))-(1.96*sqrt(pf*(1-pf))^-2*v)
    #ucl <- log(pf/(1-pf))+(1.96*sqrt(pf*(1-pf))^-2*v)
  #natural log transformation
    #lcl <- log(pf)-(1.96*(sqrt(pf)^-2*v))
    #ucl <- log(pf)+(1.96*(sqrt(pf)^-2*v))
  print(paste("Cell Probabilities", pa, pb, pc, pd))
  print(paste("Preventive Fraction is", pf))
  print(paste("95% Confidence Interval is", lcl, ",", ucl))
}

#Example
sink('PFcalc.txt')
pfci(105, 35, 106, 26)
sink()

