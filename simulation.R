################################################################################
## Final Code
################################################################################
library(MASS)
library(tidyverse)
library(ggplot2)
library(psych)

## Function to generate Initial, Change, and Final scores

set.seed(1234)

gen_data <- function(n=10000,
                     rho=.5,
                     cl=.2,
                     stability=.5,
                     yMean=10,
                     ySd=2,
                     xMean=10,
                     xSd=2,
                     resid=1.5) {
    
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


## ## Old
## gen_data <- function(n=10000, rho=.5, cl=.2, stability=.5, mean=10, sd=2, resid=1.5) {
##     ## Generate a normally distributed variable
##     initial <- rnorm(n, mean = mean, sd = sd)

##     ## Generate a second normally distributed variable
##     temp <- rnorm(n, mean = mean, sd = sd)

##     ## Combine them to create a correlated variable
##     change <- rho * initial + sqrt(1 - rho^2) * temp

##     ## Create outcome
##     final <- initial * stability + change * cl + rnorm(n, mean = 0, sd = resid)

##     ## Create dataframe
##     data <- data.frame(initial, change, final)

##     return(data)
## }

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


## Test one data set
data <- gen_data(rho=0, cl=.7, stability=.5, resid=1)
cor(data)
describe(data)
compare_models(data)


################################################################################
## Run simulation
################################################################################

rhos <- seq(0, .9, by = .3)
cls <- seq(.1, 1, by = .3)
stabilities <- seq(.1, 1, by = .3)
resids <- seq(0, 3, by = 1)

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
            resid = values[i, 4]
        ))
    )
}

final <- cbind(values, output)
names(final)[5:7] <- c("standard", "reversed", "difference")

################################################################################
## Plots
################################################################################
final %>%
    select(rho, cl, stability, resid, reversed) %>%
    filter(resid > 0) %>%
    ggplot(aes(x = cl, y = reversed, group = stability)) +
    facet_wrap(~rho + resid, ncol = 3) +
    geom_line(aes(color = stability))


final %>%
    select(rho, cl, stability, resid, reversed) %>%
    filter(resid == 0) %>%
    ggplot(aes(x = cl, y = reversed, group = stability)) +
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
