library(quantmod)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(dplyr)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)

stock_codes <- c("SWM.AX", "CBA.AX", "WES.AX", "BEN.AX", "QAN.AX", "IAG.AX")

adj_close_prices <- NULL
for(ticker in stock_codes){
  adj_close_prices <- cbind(adj_close_prices, 
                            getSymbols.yahoo(ticker,
                                             from ="2018-01-01",
                                             to = Sys.Date(), 
                                             periodicity =  "daily", 
                                             auto.assign = FALSE)[,6])
}

returns <- na.omit(ROC(adj_close_prices))

portfolio <- portfolio.spec(assets = colnames(returns))

portfolio <- add.constraint(portfolio=portfolio, type="long_only")
portfolio <- add.constraint(portfolio=portfolio, type="box", min=.05, max=.5)

portfolio <- add.objective(portfolio=portfolio, type="return", name="mean")
portfolio <- add.objective(portfolio=portfolio, type="risk", name="StdDev")


optimal_portfolio <- optimize.portfolio(R=returns, 
                                        portfolio = portfolio, 
                                        optimize_method = "ROI", 
                                        trace = TRUE)
optimal_weights <- optimal_portfolio$weights
print(optimal_weights)

portfolio_returns <- Return.portfolio(returns, weights = optimal_weights)

benchmark_prices <- getSymbols.yahoo("^AXJO", from ="2018-01-01", to = Sys.Date(), periodicity =  "daily", auto.assign = FALSE)[,6]
benchmark_returns <- na.omit(ROC(benchmark_prices))

risk_free_rate <- .0144/252 #10 year Treasury Bond Rate

alpha <- CAPM.alpha(portfolio_returns, benchmark_returns, risk_free_rate)
beta <- CAPM.beta(portfolio_returns, benchmark_returns, risk_free_rate)
sharpe <- SharpeRatio(portfolio_returns, risk_free_rate)[1,1]

output <- as.data.frame(cbind(alpha, beta, sharpe))

print(optimal_weights)
print(output)
