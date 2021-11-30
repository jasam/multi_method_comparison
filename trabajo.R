if (input$demo_files == "BloodPressure") {
    data_for_table = dt_BloodPressure
}
if (input$demo_files == "Plasma") {
    data_for_table = dt_Plasma
}
if (input$demo_files == "T4") {
    data_for_table = dt_t4
}

library("mcr")
data("creatinine", package="mcr")
fit.lr <- mcreg(as.matrix(creatinine), method.reg="LinReg", na.rm=TRUE)
fit.wlr <- mcreg(as.matrix(creatinine), method.reg="WLinReg", na.rm=TRUE)
compareFit( fit.lr, fit.wlr )




data(creatinine,package="mcr")
x <- creatinine$serum.crea
y <- creatinine$plasma.crea
fit.lr = mcreg(x,y, method.reg="LinReg", na.rm=TRUE, identity = TRUE)
MCResult.plot(fit.lr, add.legend = FALSE, sub=" ")
              



m1 <- mcreg(x,y,method.reg="Deming", mref.name="serum.crea",
            mtest.name="plasma.crea", na.rm=TRUE)
m2 <- mcreg(x,y,method.reg="WDeming", method.ci="jackknife",
            mref.name="serum.crea",
            mtest.name="plasma.crea", na.rm=TRUE)
mcr::plot(m1, XLIM=c(0.5,3),YLIM=c(0.5,3), Legend=FALSE,
     Title="Deming vs. weighted Deming regression",
     Points.pch=19,ci.area=TRUE, ci.area.col=grey(0.9),
     identity=FALSE, Grid=FALSE, Sub="")
plot(m2, ci.area=FALSE, ci.border=TRUE, ci.border.col="red3",
     reg.col="red3", Legend=FALSE,add=TRUE,
     Points=FALSE, identity=FALSE, Grid=FALSE)
includeLegend(place="topleft",models=list(m1,m2),
              colors=c("darkblue","red"), design="1", digits=2)

####

x = dt_Plasma$reference
y = dt_Plasma$test_1
fit.lr = mcreg(x,y, method.reg="LinReg", na.rm=TRUE)
MCResult.plot(fit.lr)

####
library("mcr")
data("creatinine", package="mcr")

x = dt_Plasma$reference
y = dt_Plasma$test_1
fit.lr = mcr::mcreg(x,y, method.reg="LinReg", na.rm=TRUE)
mcr::MCResult.plot(fit.lr)


printSummary(fit.lr)
fit.lr@para

?mcreg

fit_l = mcreg(x,y, na.rm = TRUE)
MCResult.plotdifference(fit.lr,
                        plot.type = input$batype,
                        add.grid = TRUE)

####
library(rsconnect)
#devtools::check()
setwd("D:/OneDrive - Universitat Oberta de Catalunya/Master_UoC/TFM/repo/multi_method_comparison/")
rsconnect::setAccountInfo(name='jasam',
                          token='3BE3FE71B970ECBCF03126DDDE5A431F',
                          secret='GHpN4FLFEZYukqMuWytujwX+8HJ+7t/Q0z9Jrm7r')
rsconnect::deployApp()
# rsconnect::deployments("D:/OneDrive - Universitat Oberta de Catalunya/Master_UoC/TFM/multi_method_comparison/")
# data_app = rsconnect::applications("jasam")
# data_app = as.data.table(data_app)
# data_app[name == "multi_method_comparison", c("id", "updated_time")]
####

ncol(dt_BloodPressure)
vector_test = c("reference", "test_1", "test_2", "test_3")
grepl("test", vector_test)
column_names = names(dt_BloodPressure)
n_cols = sort(column_names[grepl("test", column_names)])
n_cols

####
library(broom)
dt_BloodPressure

methods = c("LinReg", "WLinReg", "Deming", "WDeming", "PaBa")

slopes = c()
models = c()
intercepts = c()
data_points = c()
intercept_interval = c()

for (method in methods) {
    x = dt_BloodPressure$reference
    y = dt_BloodPressure$test_1
    model = mcr::mcreg(x, y, method.reg=method, mref.name="Reference",
                       mtest.name="Test", na.rm=TRUE)
    slopes = c(slopes, mcr::getCoefficients(model)[2])
    intercepts = c(intercepts, mcr::getCoefficients(model)[1])
    models = c(models, method)
    data_points = c(data_points, nrow(model@data))
    ci_intercept = paste0(model@para[1,"LCI"], " - ", model@para[1,"UCI"])
    intercept_interval = c(intercept_interval, ci_intercept)
}
df_for_models = data.frame(slope = slopes,
                           intercept = intercepts,
                           model = models)

df_for_models = df_for_models[, c("model", "intercept", "slope")]
df_for_models

x = dt_BloodPressure$reference
y = dt_BloodPressure$test_1
model = mcr::mcreg(x, y, method.reg="LinReg", mref.name="Reference",
                   mtest.name="Test", na.rm=TRUE)
broom::tidy(mcr::getCoefficients(model))
mcr::getRegmethod(model)
mcr::MCResult.plotBias(model)
mcr::getData(model)
nrow(model@data)
broom::tidy(model@para)
paste0(model@para[1,"LCI"], " - ", model@para[1,"UCI"])
model@para[1,"UCI"]
paste0( ((1 - model@alpha) * 100 ), "%" )
mcr::MCResultAnalytical.printSummary(model)
##
data(creatinine,package="mcr")
x <- creatinine$serum.crea
y <- creatinine$plasma.crea

# Deming regression fit.
# The confidence intervals for regression coefficients
# are calculated with analytical method
model <- mcreg( x,y,error.ratio = 1,method.reg = "Deming", method.ci = "analytical",
                
                mref.name = "serum.crea", mtest.name = "plasma.crea", na.rm=TRUE )
# Now we calculate the systematical bias
# between the testmethod and the reference method
# at the medical decision points 1, 2 and 3

calcBias( model, x.levels = c(1,2,3))
###

data_loaded = dt_BloodPressure
#methods = c("Linear Regression", "Weighted Linear Reg", "Deming Regression", "Weighted Deming Reg", "Passing-Bablok Reg")
methods = c("LinReg", "WLinReg", "Deming", "WDeming", "PaBa")
slopes = c()
models = c()
intercepts = c()
for (method in methods) {
    x = data_loaded$reference
    y = data_loaded$test_1
    model = mcr::mcreg(x, y, method.reg=method, mref.name="Reference",
                       mtest.name="Test", na.rm=TRUE)
    slopes = c(slopes, mcr::getCoefficients(model)[2])
    intercepts = c(intercepts, mcr::getCoefficients(model)[1])
    method_aux = switch(method,
                       "LinReg" = "Ordinary Least Square",
                       "WLinReg" = "Weighted Ordinary Least Square",
                       "Deming" = "Deming",
                       "WDeming" = "Weighted Deming",
                       "Passing-Bablok")
    models = c(models, method_aux)
}
df_for_models = data.frame(slope = slopes,
                           intercept = intercepts,
                           model = models)

p = ggplot(data = data_loaded, aes(x = reference, y = test_1)) + 
    geom_point(colour = "black", fill = "white", shape = 21, stroke = 0.2) +
    geom_abline(data=df_for_models, mapping=aes(slope=slope, intercept=intercept, color=model)) +
    geom_abline(slope=1, intercept=0, color="red") +
    theme_bw(base_size = 12) 

p 

fig = ggplotly(p)

fig


switch(input$demo_files,
       "LinReg" = "Linear Regression",
       "WLinReg" = "2",
       "Deming" = "3",
       "WDeming" = "4",
       "5")

tinytex::install_tinytex()

plot(mtcars)

####
dt_BloodPressure
column_names = names(dt_BloodPressure)
cols_choice_method = sort(column_names[grepl("test", column_names)])
methods = c("LinReg", "WLinReg", "Deming", "WDeming", "PaBa")

slopes = c()
models = c()
intercepts = c()
test_methods = c()

for (test_method in cols_choice_method) {
    
    for (method in methods) {
        print(paste0(test_method, method))
        x = dt_BloodPressure$reference
        y = dt_BloodPressure %>% dplyr::select(!!test_method) %>% pull()
        model = mcr::mcreg(x, y, method.reg=method, mref.name="Reference",
                           mtest.name="Test", na.rm=TRUE)
        test_methods = c(test_methods, test_method)
        slopes = c(slopes, mcr::getCoefficients(model)[2])
        intercepts = c(intercepts, mcr::getCoefficients(model)[1])
        models = c(models, method)
    }
}


df_for_models = data.table(test_method = test_methods,
                           slope = slopes,
                           intercept = intercepts,
                           model = models)

df_for_models$slope_diff = abs(1 - df_for_models$slope)
df_for_models = df_for_models[order(df_for_models$slope_diff), ]
df_for_models

models = switch(models,
                                "LinReg" = "Ordinary Least Square",
                                "WLinReg" = "Weighted Ordinary Least Square",
                                "Deming" = "Deming",
                                "WDeming" = "Weighted Deming",
                                "Passing-Bablok")
df_for_models


####

# library(data.table)
# library(dplyr)
# 
# path = "D:/OneDrive - Universitat Oberta de Catalunya/Master_UoC/TFM/ejemplos/data_analise.csv"
# dt_analise = fread(path)
# 
# data_for_plot = dt_analise
# names(data_for_plot) = c("reference", "test_1")
# x = data_for_plot$reference
# y = data_for_plot %>% dplyr::select("test_1") %>% pull()
# fit.lr = mcr::mcreg(x,y, method.reg="LinReg", na.rm=TRUE)
# mcr::MCResult.plot(fit.lr)
# mcr::getCoefficients(fit.lr)
# 
# t.test(x, y)


devtools::install_github("ModelOriented/xai2shiny")


library("xai2shiny")
library("ranger")
library("DALEX")

# Creating ML models
model_rf <- ranger(survived ~ .,
                   data = titanic_imputed,
                   classification = TRUE, 
                   probability = TRUE)
model_glm <- glm(survived ~ .,
                 data = titanic_imputed,
                 family = "binomial")

# Creating DALEX explainers
explainer_rf <- explain(model_rf,
                        data = titanic_imputed[,-8],
                        y = titanic_imputed$survived)

explainer_glm <- explain(model_glm,
                         data = titanic_imputed[,-8],
                         y = titanic_imputed$survived)

xai2shiny::xai2shiny(explainer_glm, explainer_rf, 
                     directory = 'D:/OneDrive - Universitat Oberta de Catalunya/Repos/xai2shiny2/',
                     selected_variables = c('gender', 'age'),
                     run = T)

###

library(gcookbook)  # Load gcookbook for the PlantGrowth data set

# The base plot
pg_plot <- ggplot(PlantGrowth, aes(x = group, y = weight, fill = group)) +
    geom_boxplot()

# Change the legend labels
pg_plot +
    scale_fill_discrete(labels = c("Control2", "Treatment 221", "Treatment 2"))

nrow(dt_BloodPressure)


###

library(dplyr)
library(ggplot2)
pine_df <- Loblolly
glimpse(pine_df)

sample_df <- data.frame(sample_n(pine_df, size = nrow(pine_df) * 0.5) %>%
                            select(height) %>% 
                            arrange(desc(height)), 
                        sample_n(pine_df, size = nrow(pine_df) * 0.5) %>%
                            select(height) %>%
                            arrange(desc(height)))

names(sample_df) <- c("Sample_1", "Sample_2")

sample_df$Avg <- (sample_df$Sample_1 + sample_df$Sample_2) / 2
sample_df$dif <- sample_df$Sample_1 - sample_df$Sample_2

ggplot(sample_df, aes(x = Avg, y = dif)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = mean(sample_df$dif), colour = "blue", size = 0.5) +
    geom_hline(yintercept = mean(sample_df$dif) - (1.96 * sd(sample_df$dif)), colour = "red", size = 0.5) +
    geom_hline(yintercept = mean(sample_df$dif) + (1.96 * sd(sample_df$dif)), colour = "red", size = 0.5) +
    ylab("diff. Between Measures") +
    xlab("Average Measure")


dt_BloodPressure

dt_BloodPressure$dif <- dt_BloodPressure$test_1 - dt_BloodPressure$reference

y_low = round(mean(dt_BloodPressure$dif, na.rm = TRUE) - sd(dt_BloodPressure$dif) * 2, digits = 2)
y_up = round(mean(dt_BloodPressure$dif, na.rm = TRUE) + sd(dt_BloodPressure$dif) * 2, digits = 2)
y_mean =  round(mean(dt_BloodPressure$dif, na.rm = TRUE), digits = 2)

p = ggplot(dt_BloodPressure, aes(x = reference, y = dif)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = y_mean, colour = "blue", size = 0.5) +
    geom_hline(yintercept = y_low, 
               colour = "red", size = 0.5, linetype="dashed") +
    geom_hline(yintercept = y_up, 
               colour = "red", size = 0.5, linetype="dashed") +
    theme_bw(base_size = 12) +
    labs(title="Difference Plot", x="Reference Method", y="Test(y) - Reference(x)")+
    annotate("text", x = (max(dt_BloodPressure$reference) + 5), 
             y = (y_low - (y_low * .11)), label = paste0("-2 SD", "/n", y_low), color="red") +
    annotate("text", x = (max(dt_BloodPressure$reference) + 5), 
             y = (y_up + (y_up * .11)), label = paste0("+2 SD", "/n", y_up), color="red") +
    annotate("text", x = (max(dt_BloodPressure$reference) + 5), 
             y = (y_mean + (y_mean * 1.60)), label = paste0("Mean", "/n", y_mean), color="blue")

fig = ggplotly(p)

fig

x5 <- 11.23456789
round(x5, digits = 2) 
####

library(ggplot2)
library(grid)
library(gridExtra)
library(purrr)

dt_BloodPressure = map_df(dt_BloodPressure, as.character)

str(dt_BloodPressure)

###

#path = "D:/OneDrive - Universitat Oberta de Catalunya/Master_UoC/TFM/ejemplos/dt_blood.csv"
#data.table::fwrite(dt_BloodPressure, path)

####

remotes::install_github("Appsilon/data.validator")
install.packages("assertr")


library(assertr)
library(magrittr)
library(data.validator)

report <- data_validation_report()

validate(mtcars, name = "Verifying cars dataset") %>%
    validate_if(drat < 0, description = "Column drat has only positive values") %>%
    validate_cols(in_set(c(0, 2)), vs, am, description = "vs and am values equal 0 or 2 only") %>%
    validate_cols(within_n_sds(1), mpg, description = "mpg within 1 sds") %>%
    validate_rows(num_row_NAs, within_bounds(0, 2), vs, am, mpg, description = "not too many NAs in rows") %>%
    validate_rows(maha_dist, within_n_mads(10), everything(), description = "maha dist within 10 mads") %>%
    add_results(report)

between <- function(a, b) {
    function(x) { a <= x && x <= b }
}

validate(iris, name = "Verifying flower dataset") %>%
    validate_if(Sepal.Length > 0, description = "Sepal length is greater than 0") %>%
    validate_cols(between(0, 4), Sepal.Width, description = "Sepal width is between 0 and 4") %>%
    add_results(report)

print(report)

##
# install.packages(pacman)
library(pacman)
library(pso)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
pacman::p_load(pso, ggplot2, dplyr, quantmod, tidyr, plotly) 

# Read csv file directly into R
ticker_list <- read.csv("https://www1.nseindia.com/content/indices/ind_nifty50list.csv")
class(ticker_list)
## [1] "data.frame"

# Check if data was red in correctly
head(ticker_list[,1:3], 5)
##                                 Company.Name           Industry     Symbol
## 1 Adani Ports and Special Economic Zone Ltd.           SERVICES ADANIPORTS
## 2                          Asian Paints Ltd.     CONSUMER GOODS ASIANPAINT
## 3                             Axis Bank Ltd. FINANCIAL SERVICES   AXISBANK
## 4                            Bajaj Auto Ltd.         AUTOMOBILE BAJAJ-AUTO
## 5                         Bajaj Finance Ltd. FINANCIAL SERVICES BAJFINANCE

# Note that for India, tickers need to be appended with ".NS"
tickers <- paste0(ticker_list$Symbol, ".NS")

# Pull data using quantmod::getSymbols
# Since getsymbols assigns data for each ticker to a 
# separate variable, we'll use a loop to pull data for one
# ticker at a time and append to a data.frame
ticker_df <- data.frame()

pb <- txtProgressBar(min = 1, max = length(tickers), style = 3)

for(nms in tickers){
    df <- getSymbols(Symbols = nms, verbose = F, src = "yahoo", auto.assign = F)
    
    colnames(df) <- c("open", "high", "low", "close", "volume", "adjusted")
    df <- data.frame(df)
    df$ticker <- nms
    df$date <- rownames(df)
    ticker_df <- rbind(ticker_df, df)
    
    setTxtProgressBar(pb, which(tickers == nms))
}

# We'll need to do some data cleaning 
# Using only closing prices
prices_df <- pivot_wider(data = ticker_df, id_cols = "date", names_from = "ticker", values_from = "close")

# For simplicity, we'll remove all NAs
prices_df <- na.omit(prices_df)

# Check date range for which data is available
range(prices_df$date)
## [1] "2017-11-17" "2021-10-29"

# Check dimensions
dim(prices_df)
## [1] 973  51

# Chart to check if data has been downloaded correctly 
prices_df %>% 
    
    # Convert to long form for easy plotting with ggplot
    gather(key = "ticker", value = "price", -date) %>%
    
    # Attach industry
    left_join(ticker_list %>% 
                  mutate(ticker = paste0(Symbol, ".NS")) %>% 
                  select(ticker, industry = Industry),
              by = "ticker") %>% 
    mutate(date = as.Date(date)) %>% 
    
    # Showing only metals
    filter(industry == "METALS") %>% 
    
    # Plot with ggplot
    ggplot(aes(x = date, y = price, color = ticker)) + 
    geom_line(size = 0.8) + 
    theme_minimal() + 
    scale_color_brewer(palette = "RdBu") + 
    labs(title = "Closing Prices", 
         subtitle = "Nifty 50 metal stocks",
         x = "Date", 
         y = "Closing Price") + 
    theme(legend.position = "top", 
          legend.title = element_text(colour = "transparent"), 
          axis.title.x = element_text(face = "bold"), 
          axis.title.y = element_text(face = "bold"))

# Calculate daily returns
returns_df <- apply(prices_df[,-1], 2, function(vec){
    ret <- vec/lag(vec) - 1
    return(ret)
})

returns_df <- as.data.frame(returns_df)
returns_df <- returns_df[-1,]  ## Remove first row since that's NA

# Pre computing average returns and the covariance matrix
mean_returns <- sapply(returns_df, mean)
cov_mat <- cov(returns_df)

obj_func <- function(wts, 
                     risk_av = 10, 
                     lambda1 = 10, 
                     lambda2 = 1e2, 
                     ret_vec, cov_mat){
    
    # Some matrix multiplication  
    port_returns <- ret_vec %*% wts
    port_risk <- t(wts) %*% cov_mat %*% wts
    
    # Objective function 
    # Note that alpha is the risk aversion parameter
    # Higher the value of alpha the more conservative the portfolio
    obj <- port_returns - risk_av * port_risk
    
    # Full investment penalisation 
    obj <- obj - lambda1 * (sum(wts) - 1)^2
    
    # Returning negative since the optimiser does minimisation by default 
    # We need maximisation
    return(-obj)
}

# Calculate average returns and covariance matrix for 2 assets
mean_returns_small <- apply(returns_df[,1:2], 2, mean)
cov_mat_small <- cov(returns_df[,1:2])

pso_optim <- function(obj_func,
                      c1 = 0.05,
                      c2 = 0.05,
                      w = 0.8,
                      init_fact = 0.1,
                      n_particles = 20,
                      n_dim = 2,
                      n_iter = 50,
                      upper = 1,
                      lower = 0,
                      n_avg = 10,
                      ...){
    
    # Initialise positions
    X <- matrix(runif(n_particles * n_dim), nrow = n_particles)
    
    # Ensure upper and lower bounds are respected
    X <- X * (upper - lower) + lower
    
    # Initialise velocities
    dX <- matrix(runif(n_particles * n_dim) * init_fact, ncol = n_dim)
    dX <- dX * (upper - lower) + lower
    
    # Get first personal and global bests
    pbest <- X
    pbest_obj <- apply(X, 1, obj_func, ...)
    
    gbest <- pbest[which.min(pbest_obj),]
    gbest_obj <- min(pbest_obj)
    
    # Initialise an empty data frame to store results
    loc_df <- data.frame(X, iter = 0, obj = pbest_obj)
    iter <- 1
    
    while(iter < n_iter){
        
        # Find updated velocities 
        dX <- w * dX + c1*runif(1)*(pbest - X) + c2*runif(1)*t(gbest - t(X))
        
        # Update positions
        X <- X + dX
        
        # Calculate objective function
        obj <- apply(X, 1, obj_func, ...)
        
        # Update local and global bests
        idx <- which(obj <= pbest_obj)
        pbest[idx,] <- X[idx,]
        pbest_obj[idx] <- obj[idx]
        
        idx <- which.min(pbest_obj)
        gbest <- pbest[idx,]
        gbest_obj <- min(pbest_obj)
        
        # Update iteration and store locations
        iter <- iter + 1
        loc_df <- rbind(loc_df, data.frame(X, iter = iter, obj = pbest_obj))
    }
    
    # Create list containing relevant items to be returned
    lst <- list(X = loc_df, obj = gbest_obj, obj_loc = gbest)
    return(lst)
}

out <- pso_optim(obj_func,
                 ret_vec = mean_returns_small, 
                 cov_mat = cov_mat_small,
                 lambda1 = 10, risk_av = 100,
                 n_particles = 100,
                 n_dim = 2,
                 n_iter = 200,
                 upper = 1, lower = 0, 
                 c1 = 0.02, c2 = 0.02, w = 0.05, init_fact = 0.01)

# Check if weights add to one
sum(out$obj_loc)
## [1] 1.00063

grid <- expand.grid(x = seq(0, 1, by = 0.01), 
                    y = seq(0, 1, by = 0.01))

grid$obj <- apply(grid, 1, obj_func, ret_vec = mean_returns_small, cov_mat = cov_mat_small, 
                  lambda1 = 10, risk_av = 100)

# Interactive 3D scatter plot with mesh
p <- plot_ly() %>% 
    add_mesh(data = grid, x = ~x, y = ~y, z = ~obj, inherit = F, color = "red") %>% 
    add_markers(data = out$X, x = ~X1, y = ~X2, z = ~obj, color = ~ iter, inherit = F, 
                marker = list(size = 2))

htmlwidgets::saveWidget(p, "plotly.html") 

# Interactive 3D scatter plot
plot_ly(out$X, x = ~X1, y = ~X2, z = ~obj) %>% 
    add_markers(size = 1) %>% 
    add_mesh(data = grid, x = ~x, y = ~y, z = ~obj, inherit = F)

n_stocks <- ncol(returns_df)
opt <- psoptim(par = rep(0, n_stocks),
               fn = obj_func,
               ret_vec = mean_returns, 
               cov_mat = cov_mat,
               lambda1 = 10, risk_av = 1000,
               lower = rep(0, n_stocks),
               upper = rep(1, n_stocks),
               control = list(maxit = 200, s = 100, maxit.stagnate = 500))

paste("Portfolio returns:", round(opt$par %*% mean_returns, 5))
## [1] "Portfolio returns: 0.00075"
paste("Portfolio Std dev:", round(sqrt(opt$par %*% cov_mat %*% opt$par), 5))
## [1] "Portfolio Std dev: 0.00986"

# Check if weights add up to one 
sum(opt$par)
## [1] 0.9902846

# Benchmark portfolio 
# For now let's use an equally weighted portfolio  
bench_wts <- rep(1/n_stocks, n_stocks)
bench_returns <- as.matrix(returns_df) %*% t(t(bench_wts))

# Update the objective function 
obj_func_TE <- function(wts,  
                        risk_av = 10, 
                        lambda1 = 10,  
                        lambda2 = 50, 
                        ret_vec, cov_mat){
    
    # Some matrix multiplication  
    port_returns <- ret_vec %*% wts
    port_risk <- t(wts) %*% cov_mat %*% wts
    port_returns_ts <- as.matrix(returns_df) %*% t(t(wts))
    
    obj <- port_returns - risk_av * port_risk
    obj <- obj - lambda1 * (sum(wts) - 1)^2
    
    # Tracking error 
    obj <- obj - lambda2 * sd(port_returns_ts - bench_returns)
    
    return(-obj) 
}

opt <- psoptim(par = rep(0, n_stocks),
               fn = obj_func_TE,
               ret_vec = mean_returns, 
               cov_mat = cov_mat,
               lambda1 = 10, risk_av = 1000,
               lower = rep(0, n_stocks),
               upper = rep(1, n_stocks),
               control = list(maxit = 200, s = 100, maxit.stagnate = 500))

paste("Portfolio returns:", round(opt$par %*% mean_returns, 5))
## [1] "Portfolio returns: 0.00074"
paste("Portfolio Std dev:", round(sqrt(opt$par %*% cov_mat %*% opt$par), 5))
## [1] "Portfolio Std dev: 0.01212"

# Check if weights add up to one 
sum(opt$par)
## [1] 0.9876818

df <- getSymbols(Symbols = nms, verbose = F, src = "yahoo", auto.assign = F)

data.env <- new.env()
getSymbols("AAPL")
getSymbols(Symbols = "AAPL", verbose = F, src = "yahoo", auto.assign = F)


####
set.seed(1)
psoptim(rep(NA,2),function(x) 20+sum(x^2-10*cos(2*pi*x)),
        lower=-5,upper=5,control=list(abstol=1e-8))

set.seed(2)
o2 <- psoptim(p,control=list(trace=1,REPORT=50,reltol=1e-4))
show(o2)

## Ackley
set.seed(1)
p <- test.problem("ackley",10)
o1 <- psoptim(p,control=list(trace=1,REPORT=50))
show(o1)
