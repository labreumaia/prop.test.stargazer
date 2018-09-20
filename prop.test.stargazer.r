##### Function to export proportion test to LaTeX  #####
#### Lucas de Abreu Maia ####
#### Department of Political Science ####
#### UCSD ####
#### lucasamaia.com ####
#### labreumaia@gmail.com ####

## Description
# This function takes the output of a two-way proportion test, from 
# the prop.test function, and prints the LaTeX code for a well-
# formated table, based on the stargazer  package.
## Arguments
# proportions - a matrix in which each row is the proportions of the 
# test, the first column contains the X proportions and the second 
# column contains the Y proportions.
# totals - a vector with the number of trials for each test.
# pval <-  a vector with the p values of each test
# row.names - an optional character vector with the names of the rows 
# for the output table.
# filename - an optional file path for the printed output. If abcent, 
# the output is printed to the console.
# digits - number of decimal digits to be printed.
# ... - other arguments to be passed to stargazer
## Notes
# This function works only with most of stargazer default values. So, 
# for example, the only float environment supported is "table", the 
# only significance character supported is "*" and the only 
# significance cutoffs supported are .9, .95 and .99.

prop.test.stargazer = function(proportions, totals, pval, 
  row.names = NULL, filename = NULL, digits = 2, ...){
  # Vectors with statistics
  Difference = proportions[, 1] - proportions[, 2]
  SE = sqrt(rowSums(proportions * (1 - proportions) / totals))
  # Data frame to be printed 
  df = cbind(proportions, Difference)
  # Renaming columns if needed
  if(!is.null(row.names)){
    rownames(df) = row.names
  }
  ## Exporting
  require(stargazer)
  # Capturing stargazer output to hack it
  out = capture.output(
    stargazer(df, digits = digits, notes = 
      c("$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01", 
        "Standard errors shown in parentheses"), ...)
  )
  # Adding stars
  index = grep("\\\\\\hline", out)[3]
  out[index + which(pval <= .1 & pval > .05)] = 
    diag(as.matrix(sapply(out[index + which(pval <= .1 & pval > .05)], 
          function(x) sapply(Difference[pval <= .1 & pval > .05], 
            function(y) gsub(formatC(y, format = 'f', digits = 2), 
            paste(round(y, 2), "{*}", sep = "^"), x)))))
  out[index + which(pval <= .05 & pval > .01)] = 
    diag(as.matrix(sapply(out[index + which(pval <= .05 & pval > 
            .01)], function(x) sapply(Difference[pval <= .05 & pval 
              > .01], function(y) gsub(formatC(y, format = 'f', 
                  digits = 2), paste(round(y, 2), "{**}", sep = "^"), 
                    x)))))
  out[index + which(pval <= .01)] = diag(as.matrix(sapply(out[index + 
            which(pval <= .01)], function(x) sapply(Difference[pval 
              <= .01], function(y) gsub(formatC(y, format = 'f', 
              digits = 2), paste(round(y, 2), "{***}", sep = "^"), 
              x)))))
  out = unlist(out)
  # Adding standard errors
  for(i in 1:nrow(df)){
    index = index + 1
    out = c(out[1:index], paste0(" & & ($", round(SE[i], digits), 
      "$) \\\\ "), out[(index + 1):length(out)])
    index = index + 1
  }
  # Exporting
  if(!is.null(filename)){
    cat(out, file = filename, sep = "\n")
    # Message
    cat(paste("\nLaTeX output printed to", filename, "\n", sep = " ", 
      collapse = ""))
  }else{
    cat(out, sep = "\n")
  }
}


## Example
# Generating proportions
x = 9:13
y = 13 - x

# Getting arguments for the function
proportions = rbind(prop.test(c(x[1], y[1]), rep(50, 2))$estimate, 
  prop.test(c(x[2], y[2]), rep(50, 2))$estimate, prop.test(c(x[3], 
      y[3]), rep(50, 2))$estimate, prop.test(c(x[4], y[4]), rep(50, 
      2))$estimate)
totals = rep(50, 4)
pval = c(prop.test(c(x[1], y[1]), rep(50, 2))$p.value, 
  prop.test(c(x[2], y[2]), rep(50, 2))$p.value, prop.test(c(x[3], 
    y[3]), rep(50, 2))$p.value, prop.test(c(x[4], y[4]), rep(50, 
      2))$p.value)

# Exporting
prop.test.stargazer(proportions, totals, pval, filename = "output/prop.test.stargazer.example.table.tex", title = "Output example", align = T)