################################################################################
## CLPM Triangulation Analysis
################################################################################

## Load Libraries
library(MASS)
library(tidyverse)
library(ggplot2)
library(psych)

## Function to generate Initial, Change, and Final scores

set.seed(1234)

gen_data <- function(n=10000,
                     rho=.5, # Correlation between Initial (Y1) and Change (X)
                     cl=.2, # Effect of X on Y
                     stability=.5, # Stability of Y
                     yMean=0, 
                     ySd=1,
                     xMean=0,
                     xSd=1,
                     resid=1.5 # Residual variance in Final (Y2)
                     ) {
    
    ## Generate initial variables
    xyCov <- rho*xSd*ySd
    sigma <- matrix(c(ySd^2, xyCov, xyCov, xSd^2), 2, 2)
    mu <- c(yMean, xMean)
    data <- as.data.frame(mvrnorm(n = n, mu = mu, Sigma = sigma))
    names(data) <- c("initial", "change")

    ## Create outcome
    data$final <- data$initial * stability +
        data$change * cl +
        rnorm(n, mean = 0, sd = resid)

    return(data)
}


compare_models <- function(data) {
    model1 <- lm(final ~ change + initial, data = data)
    model2 <- lm(initial ~ change + final, data = data)
    diff  <- cor(data$initial, data$final - data$initial)
    comparison <- list(
        'Final on Change' = coef(model1)[2],
        'Initial on Change' = coef(model2)[2],
        difference = diff
    )
    return(comparison)
}

################################################################################
## Test one set of parameters
################################################################################

data <- gen_data(rho=0, cl=1, stability=.1, resid=0)
cor(data)
describe(data)
compare_models(data)


################################################################################
## Run simulation
################################################################################

## Select values to simulate
## Omit 0 for CL and Stability because they can create perfectly correlated
## predictors

rhos <- seq(0, .9, by = .3)
cls <- seq(.1, 1, by = .3)
stabilities <- seq(.1, 1, by = .3)
resids <- seq(0, .9, by = .3)

values <- expand.grid(rho=rhos, cl=cls, stability=stabilities, resid=resids)

output <- matrix(
    nrow = nrow(values),
    ncol = 3
)

for (i in 1:nrow(values)) {
    output[i, ] <- unlist(
        compare_models(gen_data(
            rho = values[i, 1],
            cl = values[i, 2],
            stability = values[i, 3],
            resid = values[i, 4],
            yMean = 0,
            ySd = 1,
            xMean = 0,
            xSd = 1
        ))
    )
}

final <- cbind(values, output)
names(final)[5:7] <- c("standard", "reversed", "difference")

################################################################################
## Plots
################################################################################

#### Main plots for reversed regression
## Plot data with residual variance
final %>%
    select(rho, cl, stability, resid, reversed) %>%
    filter(resid > 0) %>%
    ggplot(aes(x = cl, y = reversed, group = stability)) +
    facet_wrap(~rho + resid, ncol = 3) +
    geom_line(aes(color = stability))

## Plot only data with no residual variance
final %>%
    select(rho, cl, stability, resid, reversed) %>%
    filter(resid == 0) %>%
    ggplot(aes(x = cl, y = reversed, group = stability)) +
    facet_wrap(~rho, nrow = 1) +
    geom_line(aes(color = stability))

#### Main plots for difference
## Plot data with residual variance
final %>%
    select(rho, cl, stability, resid, difference) %>%
    filter(resid > 0) %>%
    ggplot(aes(x = cl, y = difference, group = stability)) +
    facet_wrap(~rho + resid, ncol = 3) +
    geom_line(aes(color = stability))

## Plot only data with no residual variance
final %>%
    select(rho, cl, stability, resid, difference) %>%
    filter(resid == 0) %>%
    ggplot(aes(x = cl, y = difference, group = stability)) +
    facet_wrap(~rho, nrow = 1) +
    geom_line(aes(color = stability))



################################################################################
## Original Paper Data
################################################################################

orig <- read_csv("data_all.csv")
library(osfr)

## Get Data (code from original authors)

project <- osf_retrieve_node("https://osf.io/dpvzr/") ## link to the project at OSF
files <- osf_ls_files(project) ## files linked to the project
datatemp <- osf_download(files[files$name=="data_all.csv",], conflicts="overwrite") ## downloading the csv data file
data_all <- read.csv(datatemp$local_path, header=T) ## creating an object with the data

summary(lm(subs_g ~ add_rem + initial_g, data = data_all[which(data_all$add==1),]))
summary(lm(subs_g ~ diff_g + initial_g, data = data_all[which(data_all$add==1),]))
summary(lm(initial_g ~ diff_g + subs_g, data = data_all[which(data_all$add==1),]))
summary(lm(diff_g ~ subs_g + initial_g, data = data_all[which(data_all$add==1),]))

## No need to control for initial:

summary(lm(subs_g ~ diff_g, data = data_all[which(data_all$add==1),]))
