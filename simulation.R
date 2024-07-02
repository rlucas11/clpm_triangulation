################################################################################
## CLPM Triangulation Analysis
################################################################################

## Load Libraries
library(MASS)
library(tidyverse)
library(ggplot2)
library(psych)

## Function to generate Initial, Change, and Final scores

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
    diff  <- cor(data$change, data$final - data$initial)
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

data <- gen_data(rho=.3, cl=.75, stability=.25, resid=.3)
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
cls <- seq(0, 1, by = .25)
stabilities <- seq(0, 1, by = .25)
resids <- seq(.3, .9, by = .3)
sims <- 20

values <- expand.grid(rho=rhos, cl=cls, stability=stabilities, resid=resids)

output <- matrix(NA,
    nrow = sims * nrow(values),
    ncol = 7
)

for (s in 1:sims) {
    for (i in 1:nrow(values)) {
        row_num <- (s - 1) * nrow(values) + i
        output[row_num, ] <- unlist(
            c(
                unlist(
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
                ),
                values[i, ]
            )
        )
    }
}


final <- as.data.frame(output)
names(final) <- c("standard", "reversed", "difference", names(values))

final_agg <- final %>%
    group_by(rho, cl, stability, resid) %>%
    summarise(
        standard = mean(standard),
        reversed = mean(reversed),
        difference = mean(difference)
    ) %>%
    ungroup()

write_csv(final_agg, "simulated_data.csv")

################################################################################
## Plots
################################################################################

## Use previously simulated data
final_agg <- read_csv("simulated_data.csv")

#### Main plots for reversed regression
## Plot data with residual variance
final_agg %>%
    select(rho, cl, stability, resid, reversed) %>%
    ggplot(aes(x = cl, y = reversed, group = stability)) +
    facet_wrap(~rho + resid, ncol = 3, labeller=label_both) +
    geom_line(aes(color = stability)) +
    geom_hline(yintercept=0, color = "red")

ggsave("coef2.png")

#### Main plots for difference
## Plot data with residual variance
final_agg %>%
    select(rho, cl, stability, resid, difference) %>%
    ggplot(aes(x = cl, y = difference, group = stability)) +
    facet_wrap(~rho + resid, ncol = 3, labeller=label_both) +
    geom_line(aes(color = stability)) +
    geom_hline(yintercept=0, color = "red")

ggsave("coef3.png")



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

summary(lm(subs_g ~ add_rem + initial_g, data = data_all[which(data_all$add==1),]))
summary(lm(subs_g ~ add_rem, data = data_all[which(data_all$add==1),]))


################################################################################
## Test from correlation matrix
################################################################################

library(lavaan)


reversal <- function(xy1, y1y2, xy2) {
    cor_mat <- matrix(
        c(
            1, xy1, xy2,
            xy1, 1, y1y2,
            xy2, y1y2, 1
        ),
        nrow = 3, ncol = 3
    )


    colnames(cor_mat) <- c("x", "y1", "y2")
    rownames(cor_mat) <- c("x", "y1", "y2")

    lm1 <- "y1 ~ x + y2"
    fit <- sem(lm1, sample.cov = cor_mat, sample.nobs = 10000)
    return(coef(fit))
}


## Specify correlation matrix
## Not some values are not admissable
xy1 <- -.006614715
y1y2 <- .091569729
xy2 <- .995171173
reversal(xy1, y1y2, xy2)

xy1 <- .3007470
y1y2 <- .5563501
xy2 <- .4620675
reversal(xy1, y1y2, xy2)



## Compare to generating model
data <- gen_data(rho=.3, cl=.2, stability=.3, resid=.5)
cor(data)
describe(data)
compare_models(data)

bx <- (xy1 - xy2*y1y2)/(1 - y1y2^2)
bx

by2 <- (y1y2 - xy2*xy1)/(1 - y1y2^2)
by2
