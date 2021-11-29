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
setwd("D:/OneDrive - Universitat Oberta de Catalunya/Master_UoC/TFM/multi_method_comparison/")
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
