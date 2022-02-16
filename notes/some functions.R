z.test <- function(x, mu=0, s = sd(x), conf.level = .95) {
  
  z.score <- (mean(x)-mu)/s
  onetail <- round(pnorm(abs(z.score), lower.tail = FALSE),3)
  twosided <- 2*onetail
  z.score <- round(z.score, 3)
  zstar <- conf.level +(1-conf.level)/2
  lowconf <- mean(x)-(zstar*(s/sqrt(length(x))))
  highconf <- mean(x)+(zstar*(s/sqrt(length(x))))
  #If packaged, could use conf.level <- conf_level(x,CI,s)
  
  cat("z=  ", z.score, 
      "\np-value one-tailed: ",onetail,
      "\np-value two-tailed: ", twosided,
      "\nestimated mean: ", round(mean(x),3),
      "\nconfidence interval (", conf.level, "): ", lowconf, ",", highconf, 
      "\n"
      #confidence interval: conf.level[1], conf.level[2]
      )
}

one_prop_test <- function(x, n, p = .5, alternative = "two.sided", conf.level = .95) {
  
  phat <- x / n
  sd <- sqrt((p * (1 - p)) / n)
  z.score = (phat - p) / sd
  zstar <- round(abs(qnorm((1 - (conf.level))/2)),3)
  
  
  if ( alternative == "two.sided" ) {
    pval <- 2 * signif(pnorm(abs(z.score), lower.tail = FALSE), 3)
    test <- "not equal to "
    lowconf  <- round((phat - zstar * sqrt((phat * (1 - phat)) / n)), 3)
    highconf <- round((phat + zstar * sqrt((phat * (1 - phat)) / n)), 3)
  } else if ( alternative == "less") {
    pval <- signif(pnorm(z.score), 3)
    test <- "less than "
    lowconf  <- "-INF"
    highconf <- round((phat + zstar * sqrt((phat * (1 - phat)) / n)), 3)
  } else if ( alternative == "greater") {
    pval <- signif(pnorm(z.score, lower.tail = FALSE), 3)
    test <- "greater than "
    lowconf  <- round((phat - zstar * sqrt((phat * (1 - phat)) / n)), 3)
    highconf <- "INF"
  }
  
  if (n * p <10 || n * (1 - p) < 10) {
    cat("\n\n    Check for normality of p\U0302:    \n",
        "\nHypothesis Test:",
        "\nnp = ", n * p,
        "\nn(1-p) = ", n * (1 - p),
        "\nConfidence Interval: ",
        "\nnp\U0302 = ", n * p,
        "\nn(1-p\U0302) = ", n * (1 - p),
        "\nTesting requirements are not met.")
  } else {
    cat("\n\n    Check for normality of p\U0302:    \n",
        "\nHypothesis Test:",
        "\nnp = ", n * p,
        "\nn(1-p) = ", n * (1 - p),
        "\nConfidence Interval: ",
        "\nnp\U0302 = ", n * p,
        "\nn(1-p\U0302) = ", n * (1 - p))
  }
  
  cat( 
    "\n\n    One Proportion z-Test    \n",
    "\nz = ", round(z.score,4),", p-value = ", pval,
    "\nAlternative hypothesis: True population proportion is ", test, p, 
    "\n", (100 *conf.level), " percent confidence interval: ",
    "\n  ",lowconf, ", ", highconf,
    "\nSample Estimates: ",
    "\np\U0302 = ", signif(phat, 3),
    "\n", sep = ""
  )
}

normality_prop <- function(n, p) {
  #Check for normality of a one proportion dataset
  #if else determines whether to attach warning that requirements are not met
  if (n * p <10 || n * (1 - p) < 10) {
    cat("Check for normality of p\U0302:",
        "\nHypothesis Test:",
        "\nnp = ", n * p,
        "\nn(1-p) = ", n * (1 - p),
        "\nConfidence Interval: ",
        "\nnp\U0302 = ", n * p,
        "\nn(1-p\U0302) = ", n * (1 - p),
        "\nTesting requirements are not met.")
  } else {
    cat("Check for normality of p\U0302:",
        "\nHypothesis Test:",
        "\nnp = ", n * p,
        "\nn(1-p) = ", n * (1 - p),
        "\nConfidence Interval: ",
        "\nnp\U0302 = ", n * p,
        "\nn(1-p\U0302) = ", n * (1 - p))
  }
  
}

one_prop_CI <- function(x, n, conf.level = .95) {
  #Find Confidence interval with a given level of confidence for one proportion
  #find point estimate of p
  phat <- x / n 
  #calculate critical value (z*) using inverse CDF for normal distribution
  zstar <- round(abs(qnorm((1 - (conf.level))/2)),3) 
  #Generate low interval
  lowconf  <- round((phat - (zstar * sqrt((phat * (1 - phat)) / n))), 3)
  #generate high interval
  highconf <- round((phat + (zstar * sqrt((phat * (1 - phat)) / n))), 3)

  #print results
  cat( 
    "\nConfidence Interval (", conf.level, "): ", lowconf, ", ", highconf, 
    "\nCritical value (z*): ", zstar,
    "\n",sep = "" #removes default space between items in cat()
  )
}

sample_size <- function(m, conf.level = .95, pstar = NULL, zstar = NULL) {
  #Calculate the sample size needed for a given margin of error
  #Calculate a z* for the given Confidence level, or use a given z*
  if (is.null(zstar)) {
    zstar <- round(abs(qnorm((1 - (conf.level))/2)),3)
  }
  #Determine if earlier estimate (p*) is available
  if (is.null(pstar)) {
    #If no p* is available, use simplified sample size function and give warning
    s_size <- (zstar / m)^2 
    warning <- "Sample Size may be inaccurate if p < .3 or p > .7"
  } else { # If p* is available,
    #use standard sample size function and give no warning
    s_size <- (zstar / m)^2 * (pstar * (1 - pstar))
    warning <- ""
  } 
  #Print results
  cat(
    "\nSample Size for desired Margin of error", m,
    "\nn =", ceiling(s_size),
    "\n ", warning
  )
}

# Two Prop: x1, x2, n1, n1, P(maybe) alternative, confidence level.

two.prop.test <- function(x1, x2, n1, n2, alternative = "two.sided", conf.level = .95) {
  
  phat <- x / n
  sd <- sqrt((p * (1 - p)) / n)
  z.score = (phat - p) / sd
  zstar <- round(abs(qnorm((1 - (conf.level))/2)),3)
  lowconf  <- round((phat - zstar * sqrt((phat * (1 - phat)) / n)), 3)
  highconf <- round((phat + zstar * sqrt((phat * (1 - phat)) / n)), 3)
  
  if ( alternative == "two.sided" ) {
    pval <- 2 * round(pnorm(abs(z.score), lower.tail = FALSE), 4)
    test <- "not equal to "
  } else if ( alternative == "less") {
    pval <- round(pnorm(z.score), 4)
    test <- "less than "
  } else if ( alternative == "greater") {
    pval <- round(pnorm(z.score, lower.tail = FALSE), 4)
    test <- "greater than "
  }
  
  cat( 
    "    One Proportion Test    \n",
    "\nz:       ", round(z.score,4),
    "\np-value: ", pval,
    "\np-hat:   ", round(phat, 4),
    "\nAlternative hypothesis: True population mean is ", test, p, 
    "\nconfidence interval (", conf.level, "): ", lowconf, ", ", highconf, 
    "\n", sep = ""
  )
}



one_prop_test <- function(x, n, p = .5, alternative = "two.sided", conf.level = .95) {
  
  phat <- x / n
  sd <- sqrt((p * (1 - p)) / n)
  z.score = (phat - p) / sd
  zstar <- round(abs(qnorm((1 - (conf.level))/2)),3)
  lowconf  <- round((phat - zstar * sqrt((phat * (1 - phat)) / n)), 3)
  highconf <- round((phat + zstar * sqrt((phat * (1 - phat)) / n)), 3)
  
  if ( alternative == "two.sided" ) {
    pval <- 2 * signif(pnorm(abs(z.score), lower.tail = FALSE), 3)
    test <- "not equal to "
  } else if ( alternative == "less") {
    pval <- signif(pnorm(z.score), 3)
    test <- "less than "
  } else if ( alternative == "greater") {
    pval <- signif(pnorm(z.score, lower.tail = FALSE), 3)
    test <- "greater than "
  }
  
  cat( 
    "    One Proportion z-Test    \n",
    "\nz = ", round(z.score,4),", p-value = ", pval,
    "\nAlternative hypothesis: True population proportion is ", test, p, 
    "\n", (100 *conf.level), " percent confidence interval: ",
    "\n  ",lowconf, ", ", highconf,
    "\nSample Estimates: ",
    "\np-hat = ", signif(phat, 3),
    "\n", sep = ""
  )
}








g1 <- rnorm(40,69,2.5)
g2 <- rnorm(40,69,2.5)
Dat <- data.frame(values = c(g1,g2), group = rep(c(1,2), each = 40))
sTest <- t.test(values~group, data = Dat)
obsTStat <- sTest$statistic


N <- 3000
permutedTStats <- rep(NA, N)
for(i in 1:N)
{
  permutedData <- sample(Dat$values)
  permutedTest <- t.test(permutedData ~ Dat$group)
  permutedTStats[i] <- permutedTest$statistic
}

hist(permutedTStats)
abline(obsTStat)
sum(permutedTStats >= obsTStat) / N
sum(permutedTStats >= obsTStat) / N

roll <- function(n, sides = 6) {
  # generates n random integers between 1 and sides inclusive 
  # X in [1,sides]
  if(sides < 1 || sides %% 1 != 0) {
    stop("Cannot roll a die with non-integer or zero sides!")
  }
  trunc(runif(n,1,sides+1),0)
}

