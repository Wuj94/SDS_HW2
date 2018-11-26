# 1 # retrieving informations 

library('tseries')
library('zoo')
# 1) energy

# Load the package
require(tseries, quietly = TRUE)
?get.hist.quote
# Get Apple Inc. from NYSE

energy.instr <- c('APA', 'BHGE', 'BC', 'COG', 'SNP', 'COP', 'CRT',
                  'XEC', 'CHK','CVX')


for (i in 1:length(energy.instr)){
  print(energy.instr[i])
  aapl <- suppressWarnings(
    get.hist.quote(instrument=energy.instr[i], start="2003-01-01", end="2008-01-01",
                   quote= c("Open","Close"), provider="yahoo", drop=TRUE) )
  # Take a look
  class(aapl); names(aapl); head(aapl)
  
  # Build some relative performance measure like: Close/Open
  aapl$y <- aapl$Close/aapl$Open
  # Plot
  plot(aapl, main = energy.instr[i], xlab = "Year")
}

