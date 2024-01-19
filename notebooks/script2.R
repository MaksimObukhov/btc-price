#clear R memory
rm(list=ls())

# library('FitAR')
library('TSA')
library('tseries')
library('fUnitRoots')
library('FSAdata')
library('forecast')
library('lmtest')
library('dplyr')
library('AID')
library('nortest')


#function to check normality of data
normality.function <- function(model){

  #QQ plot
  qqnorm(y=model, main = "Quantile-Quantile Plot.")
  qqline(y=model, col = 2, lwd = 1, lty = 2)


  #Shapiro-Wilk test
  sw <- shapiro.test(model)

  #Kolmogorov-Smirnov test
  ks <- ks.test(model, "pnorm",0,1)


  table <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Test", "Hypotheses", "p-value"))
  table[nrow(table) + 1,] = list("Shapiro-Wilk test","Null Hypothesis: Data are normally distributed \t
                                 Alternative Hypothesis: Data are not normally distributed",sw$p.value)
  table[nrow(table) + 1,] = list("Kolmogorov-Smirnov test","Null Hypothesis: Data are normally distributed \t
                                 Alternative Hypothesis: Data are not normally distributed",ks$p.value)

  # kable(table, digits = 6, format = "html", row.names = TRUE) %>%
  #   kable_styling(full_width = F,
  #                 font_size = 12,
  #                 position = "center")

}


#function to check stationarity of data
stationarity.tests <- function(model){

  # ADF unitroot test
  adf <- adf.test(model)

  #PP-test
  pp <- pp.test(model)

  #KPSS test
  kpss <- kpss.test(model)

  table <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Test", "Hypotheses", "p-value"))
  table[nrow(table) + 1,] = list("Augmented Dickey-Fuller test","Null Hypothesis: Series is non-stationary \t
                                 Alternative Hypothesis: Series is stationary",adf$p.value)
  table[nrow(table) + 1,] = list("Phillips-Perron test","Null Hypothesis: Series is non-stationary \t
                                 Alternative Hypothesis: Series is stationary",pp$p.value)
  table[nrow(table) + 1,] = list("Kwiatkowski–Phillips–Schmidt–Shin test","Null Hypothesis: Series is stationary \t
                                 Alternative Hypothesis: Series is non-stationary",kpss$p.value)

  # kable(table, digits = 6, format = "html", row.names = TRUE) %>%
  #   kable_styling(full_width = F,
  #                 font_size = 12,
  #                 position = "center")
}


#function to sort scores by AIC and BIC values
sort.score <- function(x, score = c("bic", "aic")){
  if (score == "aic"){
    x[with(x, order(AIC)),]
  } else if (score == "bic") {
    x[with(x, order(BIC)),]
  } else {
    warning('score = "x" only accepts valid arguments ("aic","bic")')
  }
}

#function to carry out residual analysis
residual.analysis <- function(model, std = TRUE,start = 2, class = c("ARIMA","GARCH","ARMA-GARCH")[1]){
  # If you have an output from arima() function use class = "ARIMA"
  # If you have an output from garch() function use class = "GARCH"
  # If you have an output from ugarchfit() function use class = "ARMA-GARCH"

  if (class == "ARIMA"){
    if (std == TRUE){
      res.model = rstandard(model)
    }else{
      res.model = residuals(model)
    }
  }else if (class == "GARCH"){
    res.model = model$residuals[start:model$n.used]
  }else if (class == "ARMA-GARCH"){
    res.model = model@fit$residuals
  }else {
    stop("The argument 'class' must be either 'ARIMA' or 'GARCH' ")
  }
  par(mfrow=c(3,2))
  plot(res.model,type='o',ylab='Standardised residuals', main="Time series plot of standardised residuals")
  abline(h=0)
  hist(res.model,main="Histogram of standardised residuals")
  acf(res.model,main="ACF of standardised residuals")
  pacf(res.model, main = "PACF plot of standardised residuals")
  qqnorm(res.model,main="QQ plot of standardised residuals")
  qqline(res.model, col = 2)
  print(shapiro.test(res.model))
  # k=0
  # LBQPlot(res.model, lag.max = length(model$residuals)-1, StartLag = k + 1, k = 0, SquaredQ = FALSE)
}



BitCoin_df <- read.csv("data/df_week.csv")[, -1]
BitCoin_df <- BitCoin_df %>% na.omit()
BitCoin <- ts(BitCoin$btc_price, frequency = 1)

#check for n/a values
sum(is.na(BitCoin))

plot(BitCoin,
     ylab='Weekly Average Bitcoin Price (USD)',
     xlab = 'Time' ,
     main = "Time Series Plot of Monthly Bitcoin Prices.",
     col = "black",
     xaxt = "n"
)

axis(1, at = seq(2020,2023, by = 1))

x = zlag(BitCoin)
index = 2:length(BitCoin)
cat("Pearson's Correlation Coefficient: ",cor(BitCoin[index],x[index]))

plot(y=BitCoin,
     x=x,
     ylab='Weekly Average Bitcoin Price (USD)',
     xlab='Previous Week Average Bitcoin Price (USD)',
     main = 'Relationship between Pairs of Consecutive Weekly Bitcoin Price values',
     col = "blue")


t = time(BitCoin)
linearModel_fin = lm(BitCoin ~ t + btc_volume_24hr + btc_supply + btc_weekly_spread, data = BitCoin_df)
summary(linearModel_fin)

linearModel_macro = lm(BitCoin ~ t + us_real_interest_rate_percent + us_gdp_usd + us_inflation_rate_percent, data = BitCoin_df)
summary(linearModel_macro)

linearModel_all = lm(BitCoin ~ t + btc_volume_24hr + btc_supply + btc_weekly_spread + us_real_interest_rate_percent + us_gdp_usd + us_inflation_rate_percent, data = BitCoin_df)
summary(linearModel_all)


residual.analysis(linearModel_fin)
residual.analysis(linearModel_macro)
residual.analysis(linearModel_all)


par(mfrow=c(1,2))
acf(BitCoin, lag.max = 24, main="ACF of Bitcoin Series")
pacf(BitCoin, lag.max = 24, main="PACF of Bitcoin Series")

adf.test(BitCoin)
