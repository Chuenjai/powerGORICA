#library(MASS)   # to call mvrnorm
#library(tsDyn) # to call VAR.sim
#library(lavaan) # to call riclpmModel
#library(restriktor) # to call GORICA
#library(magick) # read model image
#library(Rfssa) #download data from github

library(devtools)
library(roxygen2) # to call roxygenise()
#usethis::use_mit_license()
#usethis::use_r("GORICApower")
#roxygenise();      # Builds the help files
#devtools::document() #Don't use this
#usethis::use_package("MASS", "tsDyn", "lavaan", "restriktor", "magick", "Rfssa", type = "Depends") #Don't use this
######################################################################################################################
#' Model
#'
#' Selecting the RI-CLPM
#' @param n The number of variables which is represented bi- (n = 2) or tri-variate (n = 3) model
#' @param model Determine wave-independent (model = 1) or wave-specific parameters model (model = 2)
#' @param M The number of measurement occasions (The number of measurement occasions should range from 3 to 6)
#' @return The review of the image of the model
#' @examples
#' Model(n = 3, model = 2, M = 6)
#' @import magick
#' @export
Model <- function(n, model, M) {

  if (n == 2) {
    if (model == 1) {
      if (M == 3) {
        RICLPM_2V3W_wave_independent <- image_read("https://raw.githubusercontent.com/Chuenjai/RICLPM-images/main/RICLPM_2V3W_wave_independent.png")
        RICLPM_2V3W_wave_independent <- image_scale(RICLPM_2V3W_wave_independent, geometry = "860") #: resize proportionally to width: 200px
        print(RICLPM_2V3W_wave_independent)

      } else if (M == 4) {
        RICLPM_2V4W_wave_independent <- image_read("https://raw.githubusercontent.com/Chuenjai/RICLPM-images/main/RICLPM_2V4W_wave_independent.png")
        RICLPM_2V4W_wave_independent <- image_scale(RICLPM_2V4W_wave_independent, geometry = "860") #: resize proportionally to width: 200px
        print(RICLPM_2V4W_wave_independent)

      } else if (M == 5) {
        RICLPM_2V5W_wave_independent <- image_read("https://raw.githubusercontent.com/Chuenjai/RICLPM-images/main/RICLPM_2V5W_wave_independent.png")
        RICLPM_2V5W_wave_independent <- image_scale(RICLPM_2V5W_wave_independent, geometry = "860") #: resize proportionally to width: 200px
        print(RICLPM_2V5W_wave_independent)

      } else if (M == 6) {
        RICLPM_2V6W_wave_independent <- image_read("https://raw.githubusercontent.com/Chuenjai/RICLPM-images/main/RICLPM_2V6W_wave_independent.png")
        RICLPM_2V6W_wave_independent <- image_scale(RICLPM_2V6W_wave_independent, geometry = "860") #: resize proportionally to width: 200px
        print(RICLPM_2V6W_wave_independent)

      } else {
        print("The number of measurement occasions should range from 3 to 6")
      }

    } else if (model == 2) {
      if (M == 3) {
        RICLPM_2V3W_wave_specific <- image_read("https://raw.githubusercontent.com/Chuenjai/RICLPM-images/main/RICLPM_2V3W_wave_specific.png")
        RICLPM_2V3W_wave_specific <- image_scale(RICLPM_2V3W_wave_specific, geometry = "860") #: resize proportionally to width: 200px
        print(RICLPM_2V3W_wave_specific)

      } else if (M == 4) {
        RICLPM_2V4W_wave_specific <- image_read("https://raw.githubusercontent.com/Chuenjai/RICLPM-images/main/RICLPM_2V4W_wave_specific.png")
        RICLPM_2V4W_wave_specific <- image_scale(RICLPM_2V4W_wave_specific, geometry = "860") #: resize proportionally to width: 200px
        print(RICLPM_2V4W_wave_specific)

      } else if (M == 5) {
        RICLPM_2V5W_wave_specific <- image_read("https://raw.githubusercontent.com/Chuenjai/RICLPM-images/main/RICLPM_2V5W_wave_specific.png")
        RICLPM_2V5W_wave_specific <- image_scale(RICLPM_2V5W_wave_specific, geometry = "860") #: resize proportionally to width: 200px
        print(RICLPM_2V5W_wave_specific)

      } else if (M == 6) {
        RICLPM_2V6W_wave_specific <- image_read("https://raw.githubusercontent.com/Chuenjai/RICLPM-images/main/RICLPM_2V6W_wave_specific.png")
        RICLPM_2V6W_wave_specific <- image_scale(RICLPM_2V6W_wave_specific, geometry = "860") #: resize proportionally to width: 200px
        print(RICLPM_2V6W_wave_specific)

      } else {
        print("Invalid model, model == 1: wave-independent parameters model and model == 2: wave-specific parameters model")
      }
    }

  } else if (n == 3) {
    if (model == 1) {
      if (M == 3) {
        RICLPM_3V3W_wave_independent <- image_read("https://raw.githubusercontent.com/Chuenjai/RICLPM-images/main/RICLPM_3V3W_wave_independent.png")
        RICLPM_3V3W_wave_independent <- image_scale(RICLPM_3V3W_wave_independent, geometry = "860") #: resize proportionally to width: 200px
        print(RICLPM_3V3W_wave_independent)

      } else if (M == 4) {
        RICLPM_3V4W_wave_independent <- image_read("https://raw.githubusercontent.com/Chuenjai/RICLPM-images/main/RICLPM_3V4W_wave_independent.png")
        RICLPM_3V4W_wave_independent <- image_scale(RICLPM_3V4W_wave_independent, geometry = "860") #: resize proportionally to width: 200px
        print(RICLPM_3V4W_wave_independent)

      } else if (M == 5) {
        RICLPM_3V5W_wave_independent <- image_read("https://raw.githubusercontent.com/Chuenjai/RICLPM-images/main/RICLPM_3V5W_wave_independent.png")
        RICLPM_3V5W_wave_independent <- image_scale(RICLPM_3V5W_wave_independent, geometry = "860") #: resize proportionally to width: 200px
        print(RICLPM_3V5W_wave_independent)

      } else if (M == 6) {
        RICLPM_3V6W_wave_independent <- image_read("https://raw.githubusercontent.com/Chuenjai/RICLPM-images/main/RICLPM_3V6W_wave_independent.png")
        RICLPM_3V6W_wave_independent <- image_scale(RICLPM_3V6W_wave_independent, geometry = "860") #: resize proportionally to width: 200px
        print(RICLPM_3V6W_wave_independent)

      }
    } else if (model == 2) {
      if (M == 3) {
        RICLPM_3V3W_wave_specific <- image_read("https://raw.githubusercontent.com/Chuenjai/RICLPM-images/main/RICLPM_3V3W_wave_specific.png")
        RICLPM_3V3W_wave_specific <- image_scale(RICLPM_3V3W_wave_specific, geometry = "860") #: resize proportionally to width: 200px
        print(RICLPM_3V3W_wave_specific)

      } else if (M == 4) {
        RICLPM_3V4W_wave_specific <- image_read("https://raw.githubusercontent.com/Chuenjai/RICLPM-images/main/RICLPM_3V4W_wave_specific.png")
        RICLPM_3V4W_wave_specific <- image_scale(RICLPM_3V4W_wave_specific, geometry = "860") #: resize proportionally to width: 200px
        print(RICLPM_3V4W_wave_specific)

      } else if (M == 5) {
        RICLPM_3V5W_wave_specific <- image_read("https://raw.githubusercontent.com/Chuenjai/RICLPM-images/main/RICLPM_3V5W_wave_specific.png")
        RICLPM_3V5W_wave_specific <- image_scale(RICLPM_3V5W_wave_specific, geometry = "860") #: resize proportionally to width: 200px
        print(RICLPM_3V5W_wave_specific)

      } else if (M == 6) {
        RICLPM_3V6W_wave_specific <- image_read("https://raw.githubusercontent.com/Chuenjai/RICLPM-images/main/RICLPM_3V6W_wave_specific.png")
        RICLPM_3V6W_wave_specific <- image_scale(RICLPM_3V6W_wave_specific, geometry = "860") #: resize proportionally to width: 200px
        print(RICLPM_3V6W_wave_specific)

      }
    } else {
      print("Invalid model, model == 1: wave-independent parameters model and model == 2: wave-specific parameters model")
    }

  } else {

    print("The number of variables should be 2 or 3, two variables for a bivariate model and three variables for a trivariate model")

  }


} # End function


#' GORICA.power
#'
#' Calculate the GORICA power
#' @param n The number of variables which is represented bi- (n = 2) or tri-variate (n = 3) model
#' @param model Determine wave-independent (model = 1) or wave-specific parameters model (model = 2)
#' @param M The number of measurement occasions (The number of measurement occasions should range from 3 to 6)
#' @param nsim The number of iterations in simulations
#' @param p The number of persons
#' @param B The matrix of population parameters
#' @param H1 The hypothesis of interest
#' @param Omega_pop The matrix of variances-covariances of the  population parameters
#' @return The GORICA power, the GORICA weights, mean of the estimates, loklik.weights, and penalty.weights
#' @examples
#' H1 <- "abs(beta6) < abs(gamma6); abs(phi6) < abs(epsilon6); abs(psi6) < abs(chi6); abs(beta5) < abs(gamma5); abs(phi5) < abs(epsilon5); abs(psi5) < abs(chi5); abs(beta4) < abs(gamma4); abs(phi4) < abs(epsilon4); abs(psi4) < abs(chi4); abs(beta3) < abs(gamma3); abs(phi3) < abs(epsilon3); abs(psi3) < abs(chi3); abs(beta2) < abs(gamma2); abs(phi2) < abs(epsilon2); abs(psi2) < abs(chi2)"
#' GORICA.power(n = 3, model = 2, M = 6, nsim = 3, p = 100, B = matrix(c(0.22, 0.1, 0.1, 0.4, 0.28, 0.1, 0.4, 0.4, 0.28),byrow = T, 3), H1, Omega_pop <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), byrow = T, ncol = 3))
#' @import MASS
#' @import tsDyn
#' @import lavaan
#' @import restriktor
#' @export
GORICA.power <- function(n, model, nsim, p, B, H1,  M, Omega_pop) {

  result <- NULL

  set.seed(123)

  N <- 100 # Number of iterations from VAR(1) model per person - and use only last M of those N iterations

  ############# Wave-independent parameters model in a bivariate model ################################################

  independ3measureBi <- function(nsim, p, B, H1,  M, Omega_pop) {

    print("The bivariate RI-CLPM for wave-independent parameters model with 3 measurement occasions is generating")

    q <- 2   # number of variables
    varxy <- matrix(NA, nrow = p, ncol = q * M, dimnames = list(NULL, c("x1", "x2", "x3", "y1", "y2", "y3")))
    data <- matrix(NA, nrow = p, ncol = q * M, dimnames = list(NULL, c("x1", "x2", "x3", "y1", "y2", "y3")))

    par.est1 <- array(NA, dim=c(nsim, 4))

    GORICA.weights <- array(NA, dim=c(nsim, 2)) # 2 >> we have H1 and complementH1 now
    colnames(GORICA.weights) <- c("H1","complementH1")

    loglik.weights <- array(NA, dim=c(nsim, 2))
    colnames(loglik.weights) <- c("H1","complementH1")

    penalty.weights <- array(NA, dim=c(nsim, 2))
    colnames(penalty.weights) <- c("H1","complementH1")

    nrposdef <- 0
    round <- 1 # set this to count number of iteration
    WarningTeller <- 0
    countNaN <- 0
    countNotPosDef <- 0
    tellerOK <- 0
    simteller <- 1

    while (simteller <= nsim) {
      #ignore warning
      oldw <- getOption("warn")
      options(warn = -1)
      print(paste0("Iteration ", simteller))

      for (j in 1:p) {
        var1 <- VAR.sim(B = B, n = N, include = "none")
        varxy[j, ] <- var1[(N - M + 1):N, ]
      }

      xy <- mvrnorm(n = p, mu = rep(0, 2), Sigma = Omega_pop, empirical = FALSE)
      data[, 1:M] <- varxy[, 1:M] + xy[, 1]
      data[, (M + 1):(2 * M)] <- varxy[, (M + 1):(2 * M)] + xy[, 2] # RICLPMdata, we use both models on the same data set

      RICLPMdata <- scale(data) # standardization

      riclpmModel<-
        '
      kappa =~ 1*x1 + 1*x2 + 1*x3
      omega =~ 1*y1 + 1*y2 + 1*y3

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1

      # Random intercept parts
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      kappa ~~ omega #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3

      #constrain autoregression and cross-lagged effects to be the same across both lags.
      p3 ~ alpha*p2 + beta*q2
      p2 ~ alpha*p1 + beta*q1

      q3 ~ delta*q2 + gamma*p2
      q2 ~ delta*q1 + gamma*p1

      p1 ~~ p1 #variance
      p2 ~~ u*p2
      p3 ~~ u*p3
      q1 ~~ q1 #variance
      q2 ~~ v*q2
      q3 ~~ v*q3

      p1 ~~ q1 #p1 and q1 covariance
      p2 ~~ uv*q2 #p2 and q2 covariance should also be constrained to be the same
      p3 ~~ uv*q3 #p2 and q2 covariance'

      riclpmConstrainedsim <- lavaan(riclpmModel, data = data,
                                     missing = 'ML',
                                     int.ov.free = F,
                                     int.lv.free = F,
                                     auto.fix.first = F,
                                     auto.fix.single = F,
                                     auto.cov.lv.x = F,
                                     auto.cov.y = F,
                                     auto.var = F)
      summary(riclpmConstrainedsim, standardized = T)
      warning <- any(is.na(parameterEstimates(riclpmConstrainedsim))[c(7:15,22:38),])
      notposdef <- any(eigen(lavInspect(riclpmConstrainedsim, "cov.lv"))$values <= 1e-8)#

      if(warning == TRUE) {
        countNaN <- countNaN+1
        WarningTeller <- WarningTeller + 1
      } else if(notposdef == TRUE) {
        countNotPosDef <- countNotPosDef+1
        WarningTeller <- WarningTeller + 1

      } else { # if no warning, then do rest of code
        print(paste0("Model does not contain NAs and cov mx of latents is pos def"))

        # subtract parameter estimates
        est1<-coef(riclpmConstrainedsim)[c(10,11,14,15)]   ##
        names(est1) <- c("alpha","beta","delta", "gamma")
        vcov<-lavInspect(riclpmConstrainedsim, "vcov")[c(10,14), c(10,14)]  ###
        par.est1[simteller,] <- est1
        meanEst1 <- apply(par.est1, 2, mean, na.rm=TRUE)
        names(meanEst1) <- c("alpha","beta","delta", "gamma")


        if (all(eigen(vcov)$values > 1e-10)) { #*

          eigen<-eigen(vcov)$values > 1e-10


          goricaResults1 <- goric(riclpmConstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")


          # Store output

          GORICA.weights[simteller,] <- goricaResults1$result[,7]
          loglik.weights[simteller,] <- goricaResults1$result[,5]
          penalty.weights[simteller,] <- goricaResults1$result[,6]

          simteller<- simteller+1
        }

        GORICA.power <- sum(GORICA.weights[,1] > GORICA.weights[,2])*(100/nsim)
        #ignore warnings
        on.exit(options(warn = oldw))
        round <- round + 1
      }
    }
    return(list(GORICA.power = GORICA.power, GORICA.weights = GORICA.weights, mean.estimates = meanEst1, loglik.weights = loglik.weights, penalty.weights = penalty.weights))
  }

  independ4measureBi <- function(nsim, p, B, H1,  M, Omega_pop) {

    print("The bivariate RI-CLPM for wave-independent parameters model with 4 measurement occasions is generating")

    q <- 2   # number of variables
    varxy <- array(NA, c(p, q * M)) # create array with nrow=p and ncol=n in order to keep simulate data
    colnames(varxy)<-c("x1", "x2", "x3","x4", "y1", "y2", "y3","y4")
    RICLPMdata <- array(NA, c(p, q * M))
    colnames(RICLPMdata)<-c("x1", "x2", "x3","x4", "y1", "y2", "y3","y4")

    par.est1 <- array(NA, dim=c(nsim, 4))

    GORICA.weights <- array(NA, dim=c(nsim, 2)) # 2 >> we have H1 and complementH1 now
    colnames(GORICA.weights) <- c("H1","complementH1")

    loglik.weights <- array(NA, dim=c(nsim, 2))
    colnames(loglik.weights) <- c("H1","complementH1")

    penalty.weights <- array(NA, dim=c(nsim, 2))
    colnames(penalty.weights) <- c("H1","complementH1")

    nrposdef <- 0
    round <-1 # set this to count number of iteration
    WarningTeller <- 0
    countNaN <- 0
    countNotPosDef <-0
    tellerOK <- 0
    simteller<- 1

    while(simteller<=nsim){

      #ignore warning
      oldw <- getOption("warn")
      options(warn = -1)

      print(paste0("Iteration", simteller))

      for(j in 1:p){
        var1 <- VAR.sim(B=B, n=N, include="none")
        varxy[j,]<-var1[(N-M+1):N,]
      }

      xy <- mvrnorm(n = p, mu = rep(0,2), Sigma = Omega_pop, empirical=FALSE)
      RICLPMdata[, 1:M] <- varxy[, 1:M] + xy[,1]
      RICLPMdata[, (M+1):(2*M)] <- varxy[, (M+1):(2*M)] + xy[,2]

      RICLPMdata <- scale(RICLPMdata) #standardizing

      # Fitting RI-CLPM
      riclpmModel<-
        '
      kappa =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
      omega =~ 1*y1 + 1*y2 + 1*y3 + 1*y4

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1


      # Random intercept part
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      kappa ~~ omega #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4

      #constrain autoregression and cross-lagged effects to be the same across both lags.

      p4 ~ alpha*p3 + beta*q3
      p3 ~ alpha*p2 + beta*q2
      p2 ~ alpha*p1 + beta*q1


      q4 ~ delta*q3 + gamma*p3
      q3 ~ delta*q2 + gamma*p2
      q2 ~ delta*q1 + gamma*p1


      p1 ~~ p1 #variance
      p2 ~~ u*p2
      p3 ~~ u*p3
      p4 ~~ u*p4

      q1 ~~ q1 #variance
      q2 ~~ v*q2
      q3 ~~ v*q3
      q4 ~~ v*q4


      p1 ~~ q1 #p1 and q1 covariance
      p2 ~~ uv*q2 #p2 and q2 covariance should also be constrained to be the same
      p3 ~~ uv*q3 #p2 and q2 covariance
      p4 ~~ uv*q4'


      riclpmConstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                     missing = 'ML',
                                     int.ov.free = F,
                                     int.lv.free = F,
                                     auto.fix.first = F,
                                     auto.fix.single = F,
                                     auto.cov.lv.x = F,
                                     auto.cov.y = F,
                                     auto.var = F)

      warning <- any(is.na(parameterEstimates(riclpmConstrainedsim))[c(9:19,28:51),])
      notposdef <- any(eigen(lavInspect(riclpmConstrainedsim, "cov.lv"))$values <= 1e-8)

      if(warning == TRUE) {
        countNaN <- countNaN+1
        WarningTeller <- WarningTeller + 1
      } else if(notposdef == TRUE) {
        countNotPosDef <- countNotPosDef+1
        WarningTeller <- WarningTeller + 1
      } else { # if no warning, then do rest of code
        print(paste0("Model does not contain NAs and cov mx of latents is pos def"))

        # subtract for alpha, beta, delta, gamma
        est1<-coef(riclpmConstrainedsim)[c(12,13, 18, 19)]
        names(est1) <- c("alpha","beta", "delta", "gamma")
        vcov<-lavInspect(riclpmConstrainedsim, "vcov")[c(12,13, 18, 19), c(12,13, 18, 19)]
        par.est1[simteller,] <- est1
        meanEst1 <- apply(par.est1, 2, mean, na.rm=TRUE)
        names(meanEst1) <- c("alpha","beta","delta", "gamma")


        if (all(eigen(vcov)$values > 1e-10)) {

          eigen<-eigen(vcov)$values > 1e-10

          goricaResults1 <- goric(riclpmConstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")
          # Store output

          GORICA.weights[simteller,] <- goricaResults1$result[,7]
          loglik.weights[simteller,] <- goricaResults1$result[,5]
          penalty.weights[simteller,] <- goricaResults1$result[,6]

          simteller<- simteller+1
        } else{nrposdef<- nrposdef+1}

        GORICA.power <- sum(GORICA.weights[,1] > GORICA.weights[,2])*(100/nsim)
        #ignore warnings
        on.exit(options(warn = oldw))
        round <- round + 1
      }
    }
    return(list(GORICA.power = GORICA.power, GORICA.weights = GORICA.weights, mean.estimates = meanEst1, loglik.weights = loglik.weights, penalty.weights = penalty.weights))
  }

  independ5measureBi <- function(nsim, p, B, H1,  M, Omega_pop) {

    print("The bivariate RI-CLPM for wave-independent parameters model with 5 measurement occasions is generating")

    q <- 2   # number of variables
    varxy <- array(NA, c(p, q * M)) # create array with nrow=p and ncol=n in order to keep simulate data
    colnames(varxy)<-c("x1", "x2", "x3","x4","x5", "y1", "y2", "y3","y4","y5")
    RICLPMdata <- array(NA, c(p, q * M))
    colnames(RICLPMdata)<-c("x1", "x2", "x3","x4","x5", "y1", "y2", "y3","y4","y5")

    par.est1 <- array(NA, dim=c(nsim, 4))

    GORICA.weights <- array(NA, dim=c(nsim, 2)) # 2 >> we have H1 and complementH1 now
    colnames(GORICA.weights) <- c("H1","complementH1")

    loglik.weights <- array(NA, dim=c(nsim, 2))
    colnames(loglik.weights) <- c("H1","complementH1")

    penalty.weights <- array(NA, dim=c(nsim, 2))
    colnames(penalty.weights) <- c("H1","complementH1")

    nrposdef <- 0
    round <-1 # set this to count number of iteration
    WarningTeller <- 0
    countNaN <- 0
    countNotPosDef <-0
    tellerOK <- 0
    simteller<- 1

    while(simteller<=nsim){
      #for (simteller in 1:nsim) {

      #ignore warning
      oldw <- getOption("warn")
      options(warn = -1)

      print(paste0("Iteration", simteller))

      for(j in 1:p){
        var1 <- VAR.sim(B=B, n=N, include="none")
        varxy[j,]<-var1[(N-M+1):N,]
      }

      xy <- mvrnorm(n = p, mu = rep(0,2), Sigma = Omega_pop, empirical=FALSE)
      RICLPMdata[, 1:M] <- varxy[, 1:M] + xy[,1]
      RICLPMdata[, (M+1):(2*M)] <- varxy[, (M+1):(2*M)] + xy[,2]

      RICLPMdata <- scale(RICLPMdata) #standardizing

      # Fitting RI-CLPM
      riclpmModel<-
        '
      kappa =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
      omega =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      x5 ~ mu5*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1
      y5 ~ pi5*1


      # Random intercept part
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      kappa ~~ omega #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      p5 =~ 1*x5
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4
      q5 =~ 1*y5

    #constrain autoregression and cross-lagged effects to be the same across both lags.

      p5 ~ alpha*p4 + beta*q4
      p4 ~ alpha*p3 + beta*q3
      p3 ~ alpha*p2 + beta*q2
      p2 ~ alpha*p1 + beta*q1


      q5 ~ delta*q4 + gamma*p4
      q4 ~ delta*q3 + gamma*p3
      q3 ~ delta*q2 + gamma*p2
      q2 ~ delta*q1 + gamma*p1


      p1 ~~ p1 #variance
      p2 ~~ u*p2
      p3 ~~ u*p3
      p4 ~~ u*p4
      p5 ~~ u*p5

      q1 ~~ q1 #variance
      q2 ~~ v*q2
      q3 ~~ v*q3
      q4 ~~ v*q4
      q5 ~~ v*q5


      p1 ~~ q1 #p1 and q1 covariance
      p2 ~~ uv*q2 #p2 and q2 covariance should also be constrained to be the same
      p3 ~~ uv*q3 #p2 and q2 covariance
      p4 ~~ uv*q4
      p5 ~~ uv*q5'


      riclpmConstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                     missing = 'ML',
                                     int.ov.free = F,
                                     int.lv.free = F,
                                     auto.fix.first = F,
                                     auto.fix.single = F,
                                     auto.cov.lv.x = F,
                                     auto.cov.y = F,
                                     auto.var = F)

      warning <- any(is.na(parameterEstimates(riclpmConstrainedsim))[c(11:23,34:64),])
      notposdef <- any(eigen(lavInspect(riclpmConstrainedsim, "cov.lv"))$values <= 1e-8)

      if(warning == TRUE) {
        countNaN <- countNaN+1
        WarningTeller <- WarningTeller + 1
      } else if(notposdef == TRUE) {
        countNotPosDef <- countNotPosDef+1
        WarningTeller <- WarningTeller + 1
      } else { # if no warning, then do rest of code
        print(paste0("Model does not contain NAs and cov mx of latents is pos def"))

        # subtract for alpha, beta, delta, gamma
        est1<-coef(riclpmConstrainedsim)[c(14, 15, 22, 23)]
        names(est1) <- c("alpha", "beta", "delta", "gamma")
        vcov<-lavInspect(riclpmConstrainedsim, "vcov")[c(14, 15, 22, 23), c(14, 15, 22, 23)]
        par.est1[simteller,] <- est1
        meanEst1 <- apply(par.est1, 2, mean, na.rm=TRUE)
        names(meanEst1) <- c("alpha","beta","delta", "gamma")


        if (all(eigen(vcov)$values > 1e-10)) {

          eigen<-eigen(vcov)$values > 1e-10

          goricaResults1 <- goric(riclpmConstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

          # Store output

          GORICA.weights[simteller,] <- goricaResults1$result[,7]
          loglik.weights[simteller,] <- goricaResults1$result[,5]
          penalty.weights[simteller,] <- goricaResults1$result[,6]

          simteller<- simteller+1
        } else{nrposdef<- nrposdef+1}

        GORICA.power <- sum(GORICA.weights[,1] > GORICA.weights[,2])*(100/nsim)
        #ignore warnings
        on.exit(options(warn = oldw))
        round <- round + 1
      }
    }
    return(list(GORICA.power = GORICA.power, GORICA.weights = GORICA.weights, mean.estimates = meanEst1, loglik.weights = loglik.weights, penalty.weights = penalty.weights))
  }

  independ6measureBi <- function(nsim, p, B, H1,  M, Omega_pop) {

    print("The bivariate RI-CLPM for wave-independent parameters model with 6 measurement occasions is generating")

    q <- 2   # number of variables
    varxy <- array(NA, c(p, q * M)) # create array with nrow=p and ncol=n in order to keep simulate data
    colnames(varxy)<-c("x1", "x2", "x3","x4", "x5", "x6", "y1", "y2", "y3","y4", "y5", "y6")
    RICLPMdata <- array(NA, c(p, q * M))
    colnames(RICLPMdata)<-c("x1", "x2", "x3","x4","x5", "x6", "y1", "y2", "y3","y4","y5", "y6")

    par.est1 <- array(NA, dim=c(nsim, 4))

    GORICA.weights <- array(NA, dim=c(nsim, 2)) # 2 >> we have H1 and complementH1 now
    colnames(GORICA.weights) <- c("H1","complementH1")

    loglik.weights <- array(NA, dim=c(nsim, 2))
    colnames(loglik.weights) <- c("H1","complementH1")

    penalty.weights <- array(NA, dim=c(nsim, 2))
    colnames(penalty.weights) <- c("H1","complementH1")

    nrposdef <- 0
    round <-1 # set this to count number of iteration
    WarningTeller <- 0
    countNaN <- 0
    countNotPosDef <-0
    tellerOK <- 0
    simteller<- 1

    while(simteller <= nsim){
      #for (simteller in 1:nsim) {

      #ignore warning
      oldw <- getOption("warn")
      options(warn = -1)

      print(paste0("Iteration", simteller))

      for(j in 1:p){
        var1 <- VAR.sim(B=B, n=N, include="none")
        varxy[j,]<-var1[(N-M+1):N,]
      }

      xy <- mvrnorm(n = p, mu = rep(0,2), Sigma = Omega_pop, empirical=FALSE)
      RICLPMdata[, 1:M] <- varxy[, 1:M] + xy[,1]
      RICLPMdata[, (M+1):(2*M)] <- varxy[, (M+1):(2*M)] + xy[,2]

      RICLPMdata <- scale(RICLPMdata) #standardizing

      # Fitting RI-CLPM
      riclpmModel<-
        '
      kappa =~ 1*x1 + 1*x2 + 1*x3+1*x4 + 1*x5 + 1*x6
      omega =~ 1*y1 + 1*y2 + 1*y3+1*y4 + 1*y5 + 1*y6

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      x5 ~ mu5*1
      x6 ~ mu6*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1
      y5 ~ pi5*1
      y6 ~ pi6*1

      # Random intercept part
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      kappa ~~ omega #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      p5 =~ 1*x5
      p6 =~ 1*x6
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4
      q5 =~ 1*y5
      q6 =~ 1*y6

      #constrain autoregression and cross-lagged effects to be the same across both lags.
      p6 ~ alpha*p5 + beta*q5
      p5 ~ alpha*p4 + beta*q4
      p4 ~ alpha*p3 + beta*q3
      p3 ~ alpha*p2 + beta*q2
      p2 ~ alpha*p1 + beta*q1

      q6 ~ delta*q5 + gamma*p5
      q5 ~ delta*q4 + gamma*p4
      q4 ~ delta*q3 + gamma*p3
      q3 ~ delta*q2 + gamma*p2
      q2 ~ delta*q1 + gamma*p1


      p1 ~~ p1 #variance
      p2 ~~ u*p2
      p3 ~~ u*p3
      p4 ~~ u*p4
      p5 ~~ u*p5
      p6 ~~ u*p6
      q1 ~~ q1 #variance
      q2 ~~ v*q2
      q3 ~~ v*q3
      q4 ~~ v*q4
      q5 ~~ v*q5
      q6 ~~ v*q6

      p1 ~~ q1 #p1 and q1 covariance
      p2 ~~ uv*q2 #p2 and q2 covariance should also be constrained to be the same
      p3 ~~ uv*q3 #p2 and q2 covariance
      p4 ~~ uv*q4
      p5 ~~ uv*q5
      p6 ~~ uv*q6'

      riclpmConstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                     missing = 'ML',
                                     int.ov.free = F,
                                     int.lv.free = F,
                                     auto.fix.first = F,
                                     auto.fix.single = F,
                                     auto.cov.lv.x = F,
                                     auto.cov.y = F,
                                     auto.var = F)

      warning <- any(is.na(parameterEstimates(riclpmConstrainedsim))[c(13:27,40:77),])
      notposdef <- any(eigen(lavInspect(riclpmConstrainedsim, "cov.lv"))$values <= 1e-8)

      if(warning == TRUE) {
        countNaN <- countNaN+1
        WarningTeller <- WarningTeller + 1
      } else if(notposdef == TRUE) {
        countNotPosDef <- countNotPosDef+1
        WarningTeller <- WarningTeller + 1
      } else { # if no warning, then do rest of code
        print(paste0("Model does not contain NAs and cov mx of latents is pos def"))

        # subtract for alpha, beta, delta, gamma
        est1<-coef(riclpmConstrainedsim)[c(16, 17, 26, 27)]
        names(est1) <- c("alpha", "beta", "delta", "gamma")
        vcov<-lavInspect(riclpmConstrainedsim, "vcov")[c(16, 17, 26, 27), c(16, 17, 26, 27)]
        par.est1[simteller,] <- est1
        meanEst1 <- apply(par.est1, 2, mean, na.rm=TRUE)
        names(meanEst1) <- c("alpha","beta","delta", "gamma")

        if (all(eigen(vcov)$values > 1e-10)) {

          eigen<-eigen(vcov)$values > 1e-10
          goricaResults1 <- goric(riclpmConstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")
          # Store output

          GORICA.weights[simteller,] <- goricaResults1$result[,7]
          loglik.weights[simteller,] <- goricaResults1$result[,5]
          penalty.weights[simteller,] <- goricaResults1$result[,6]

          simteller<- simteller+1
        } else{nrposdef<- nrposdef+1}

        GORICA.power <- sum(GORICA.weights[,1] > GORICA.weights[,2])*(100/nsim)
        #ignore warnings
        on.exit(options(warn = oldw))
        round <- round + 1
      }
    }
    return(list(GORICA.power = GORICA.power, GORICA.weights = GORICA.weights, mean.estimates = meanEst1, loglik.weights = loglik.weights, penalty.weights = penalty.weights))
  }

  ################# Wave-specific parameters model in a bivariate model ################################################

  specific3measureBi <- function(nsim, p, B, H1,  M, Omega_pop) {

    print("The bivariate RI-CLPM for wave-specific parameters model with 3 measurement occasions is generating")

    q <- 2   # number of variables
    varxy <- matrix(NA, nrow = p, ncol = q * M, dimnames = list(NULL, c("x1", "x2", "x3", "y1", "y2", "y3")))
    data <- matrix(NA, nrow = p, ncol = q * M, dimnames = list(NULL, c("x1", "x2", "x3", "y1", "y2", "y3")))

    par.est1 <- array(NA, dim=c(nsim, 8))

    GORICA.weights <- array(NA, dim=c(nsim, 2)) # 2 >> we have H1 and complementH1 now
    colnames(GORICA.weights) <- c("H1","complementH1")

    loglik.weights <- array(NA, dim=c(nsim, 2))
    colnames(loglik.weights) <- c("H1","complementH1")

    penalty.weights <- array(NA, dim=c(nsim, 2))
    colnames(penalty.weights) <- c("H1","complementH1")

    nrposdef <- 0
    round <- 1 # set this to count number of iteration
    WarningTeller <- 0
    countNaN <- 0
    countNotPosDef <- 0
    tellerOK <- 0
    simteller <- 1

    while (simteller <= nsim) {
      #ignore warning
      oldw <- getOption("warn")
      options(warn = -1)
      print(paste0("Iteration ", simteller))

      for (j in 1:p) {
        var1 <- VAR.sim(B = B, n = N, include = "none")
        varxy[j, ] <- var1[(N - M + 1):N, ]
      }

      xy <- mvrnorm(n = p, mu = rep(0, 2), Sigma = Omega_pop, empirical = FALSE)
      data[, 1:M] <- varxy[, 1:M] + xy[, 1]
      data[, (M + 1):(2 * M)] <- varxy[, (M + 1):(2 * M)] + xy[, 2] # RICLPMdata, we use both models on the same data set

      riclpmModel2 <-
        '

      #Intercepts
      kappa =~ 1*x1 + 1*x2 + 1*x3
      omega =~ 1*y1 + 1*y2 + 1*y3

      x1 ~ mu1*1
      x2 ~ mu2*1
      x3 ~ mu3*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1

      # Variances and covariance of the random intercepts
      kappa  ~~ kappa 	# variance
      omega  ~~ omega 	# variance
      kappa  ~~ omega 	# covariance

      # Latent variables for autoregressive and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3

      # Unconstrained autoregressive and cross-lagged effects
      # Specify the name of the standardized estimates
      p3 ~ alpha3*p2 + beta3*q2
      p2 ~ alpha2*p1 + beta2*q1
      q3 ~ delta3*q2 + gamma3*p2
      q2 ~ delta2*q1 + gamma2*p1
      p1 ~~ p1
      p2 ~~ u2*p2
      p3 ~~ u3*p3
      q1 ~~ q1
      q2 ~~ v2*q2
      q3 ~~ v3*q3
      p1 ~~ q1
      p2 ~~ u2v2*q2
      p3 ~~ u3v3*q3 '

      riclpmUnconstrained <-  lavaan(riclpmModel2, data = data,
                                     missing = 'ML',
                                     int.ov.free = F,
                                     int.lv.free = F,
                                     auto.fix.first = F,
                                     auto.fix.single = F,
                                     auto.cov.lv.x = F,
                                     auto.cov.y = F,
                                     auto.var = F)
      summary(riclpmUnconstrained, standardized = T)
      warning <- any(is.na(parameterEstimates(riclpmUnconstrained))[c(7:15,22:38),])
      notposdef <- any(eigen(lavInspect(riclpmUnconstrained, "cov.lv"))$values <= 1e-8)#

      if(warning == TRUE) {
        countNaN <- countNaN+1
        WarningTeller <- WarningTeller + 1
      } else if(notposdef == TRUE) {
        countNotPosDef <- countNotPosDef+1
        WarningTeller <- WarningTeller + 1

      } else { # if no warning, then do rest of code
        print(paste0("Model does not contain NAs and cov mx of latents is pos def"))

        # subtract standardized parameter estimates
        StdEst <- standardizedsolution(riclpmUnconstrained, type = "std.nox",
                                       se = TRUE, zstat = TRUE, pvalue = TRUE,
                                       ci = TRUE, level = 0.95, cov.std = TRUE,
                                       remove.eq = TRUE, remove.ineq = TRUE,
                                       remove.def = FALSE, partable = NULL,
                                       GLIST = NULL, est = NULL)

        est1 <- StdEst[c(22:29),5]
        names(est1) <- c("alpha3","beta3", "alpha2","beta2", "delta3", "gamma3", "delta2", "gamma2")
        vcov <- lavInspect(riclpmUnconstrained, "vcov.std.nox")[c(10:17), c(10:17)]
        par.est1[simteller,] <- est1
        meanEst1 <- apply(par.est1, 2, mean, na.rm=TRUE)
        names(meanEst1) <- c("alpha3","beta3", "alpha2","beta2", "delta3", "gamma3", "delta2", "gamma2")


        if (all(eigen(vcov)$values > 1e-10)) { #*

          eigen<-eigen(vcov)$values > 1e-10

          goricaResults1 <- goric(riclpmUnconstrained, hypotheses = list(H1), comparison = "complement", type = "gorica")

          # Store output

          GORICA.weights[simteller,] <- goricaResults1$result[,7]
          loglik.weights[simteller,] <- goricaResults1$result[,5]
          penalty.weights[simteller,] <- goricaResults1$result[,6]

          simteller<- simteller+1
        }

        GORICA.power <- sum(GORICA.weights[,1] > GORICA.weights[,2])*(100/nsim)
        #ignore warnings
        on.exit(options(warn = oldw))
        round <- round + 1
      }
    }
    return(list(GORICA.power = GORICA.power, GORICA.weights = GORICA.weights, mean.estimates = meanEst1, loglik.weights = loglik.weights, penalty.weights = penalty.weights))
  }

  specific4measureBi <- function(nsim, p, B, H1,  M, Omega_pop) {

    print("The bivariate RI-CLPM for wave-specific parameters model with 4 measurement occasions is generating")

    q <- 2   # number of variables
    varxy <- array(NA, c(p, q * M)) # create array with nrow=p and ncol=n in order to keep simulate data
    colnames(varxy)<-c("x1", "x2", "x3","x4", "y1", "y2", "y3","y4")
    RICLPMdata <- array(NA, c(p, q * M))
    colnames(RICLPMdata)<-c("x1", "x2", "x3","x4", "y1", "y2", "y3","y4")

    par.est1 <- array(NA, dim=c(nsim, 12))

    GORICA.weights <- array(NA, dim=c(nsim, 2)) # 2 >> we have H1 and complementH1 now
    colnames(GORICA.weights) <- c("H1","complementH1")

    loglik.weights <- array(NA, dim=c(nsim, 2))
    colnames(loglik.weights) <- c("H1","complementH1")

    penalty.weights <- array(NA, dim=c(nsim, 2))
    colnames(penalty.weights) <- c("H1","complementH1")

    nrposdef <- 0
    round <-1 # set this to count number of iteration
    WarningTeller <- 0
    countNaN <- 0
    countNotPosDef <-0
    tellerOK <- 0
    simteller<- 1

    while(simteller<=nsim){

      #ignore warning
      oldw <- getOption("warn")
      options(warn = -1)

      print(paste0("Iteration", simteller))

      for(j in 1:p){
        var1 <- VAR.sim(B=B, n=N, include="none")
        varxy[j,]<-var1[(N-M+1):N,]
      }

      xy <- mvrnorm(n = p, mu = rep(0,2), Sigma = Omega_pop, empirical=FALSE)
      RICLPMdata[, 1:M] <- varxy[, 1:M] + xy[,1]
      RICLPMdata[, (M+1):(2*M)] <- varxy[, (M+1):(2*M)] + xy[,2]


      # Fitting RI-CLPM
      riclpmModel2 <-
        '
      kappa =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
      omega =~ 1*y1 + 1*y2 + 1*y3 + 1*y4

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1


      # Random intercept part
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      kappa ~~ omega #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4

      #Unconstrain autoregression and cross-lagged effects to be the same across both lags.

      p4 ~ alpha4*p3 + beta4*q3
      p3 ~ alpha3*p2 + beta3*q2
      p2 ~ alpha2*p1 + beta2*q1


      q4 ~ delta4*q3 + gamma4*p3
      q3 ~ delta3*q2 + gamma3*p2
      q2 ~ delta2*q1 + gamma2*p1


      p1 ~~ p1 #variance
      p2 ~~ u2*p2
      p3 ~~ u3*p3
      p4 ~~ u4*p4

      q1 ~~ q1 #variance
      q2 ~~ v2*q2
      q3 ~~ v3*q3
      q4 ~~ v4*q4


      p1 ~~ q1 #p1 and q1 covariance
      p2 ~~ u2v2*q2 #p2 and q2 covariance should also be constrained to be the same
      p3 ~~ u3v3*q3 #p3 and q3 covariance
      p4 ~~ u4v4*q4'

      riclpmUnconstrainedsim <- lavaan(riclpmModel2, data = RICLPMdata,
                                       missing = 'ML',
                                       int.ov.free = F,
                                       int.lv.free = F,
                                       auto.fix.first = F,
                                       auto.fix.single = F,
                                       auto.cov.lv.x = F,
                                       auto.cov.y = F,
                                       auto.var = F)

      warning <- any(is.na(parameterEstimates(riclpmUnconstrainedsim))[c(9:19,28:51),])
      notposdef <- any(eigen(lavInspect(riclpmUnconstrainedsim, "cov.lv"))$values <= 1e-8)

      if(warning == TRUE) {
        countNaN <- countNaN+1
        WarningTeller <- WarningTeller + 1
      } else if(notposdef == TRUE) {
        countNotPosDef <- countNotPosDef+1
        WarningTeller <- WarningTeller + 1
      } else { # if no warning, then do rest of code
        print(paste0("Model does not contain NAs and cov mx of latents is pos def"))

        # subtract for alpha, beta, delta, gamma (standardized estimates)
        StdEst <- standardizedsolution(riclpmUnconstrainedsim, type = "std.nox",
                                       se = TRUE, zstat = TRUE, pvalue = TRUE,
                                       ci = TRUE, level = 0.95, cov.std = TRUE,
                                       remove.eq = TRUE, remove.ineq = TRUE,
                                       remove.def = FALSE, partable = NULL,
                                       GLIST = NULL, est = NULL)

        est1 <- StdEst[c(28:39),5]
        names(est1) <- c("alpha4", "beta4", "alpha3", "beta3", "alpha2", "beta2",  "delta4", "gamma4", "delta3", "gamma3", "delta2", "gamma2")
        vcov<-lavInspect(riclpmUnconstrainedsim, "vcov.std.nox")[c(12:23), c(12:23)]
        par.est1[simteller,] <- est1
        meanEst1 <- apply(par.est1, 2, mean, na.rm=TRUE)
        names(meanEst1) <- c("alpha4", "beta4", "alpha3", "beta3", "alpha2", "beta2",  "delta4", "gamma4", "delta3", "gamma3", "delta2", "gamma2")


        if (all(eigen(vcov)$values > 1e-10)) {

          eigen<-eigen(vcov)$values > 1e-10

          goricaResults1 <- goric(riclpmUnconstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")
          # Store output

          GORICA.weights[simteller,] <- goricaResults1$result[,7]
          loglik.weights[simteller,] <- goricaResults1$result[,5]
          penalty.weights[simteller,] <- goricaResults1$result[,6]

          simteller<- simteller+1
        } else{nrposdef<- nrposdef+1}

        GORICA.power <- sum(GORICA.weights[,1] > GORICA.weights[,2])*(100/nsim)
        #ignore warnings
        on.exit(options(warn = oldw))
        round <- round + 1
      }
    }
    return(list(GORICA.power = GORICA.power, GORICA.weights = GORICA.weights, mean.estimates = meanEst1, loglik.weights = loglik.weights, penalty.weights = penalty.weights))
  }

  specific5measureBi <- function(nsim, p, B, H1,  M, Omega_pop) {

    print("The bivariate RI-CLPM for wave-specific parameters model with 5 measurement occasions is generating")

    q <- 2   # number of variables
    varxy <- array(NA, c(p, q * M)) # create array with nrow=p and ncol=n in order to keep simulate data
    colnames(varxy)<-c("x1", "x2", "x3","x4","x5", "y1", "y2", "y3","y4","y5")
    RICLPMdata <- array(NA, c(p, q * M))
    colnames(RICLPMdata)<-c("x1", "x2", "x3","x4","x5", "y1", "y2", "y3","y4","y5")

    par.est1 <- array(NA, dim=c(nsim, 16))

    GORICA.weights <- array(NA, dim=c(nsim, 2)) # 2 >> we have H1 and complementH1 now
    colnames(GORICA.weights) <- c("H1","complementH1")

    loglik.weights <- array(NA, dim=c(nsim, 2))
    colnames(loglik.weights) <- c("H1","complementH1")

    penalty.weights <- array(NA, dim=c(nsim, 2))
    colnames(penalty.weights) <- c("H1","complementH1")

    nrposdef <- 0
    round <-1 # set this to count number of iteration
    WarningTeller <- 0
    countNaN <- 0
    countNotPosDef <-0
    tellerOK <- 0
    simteller<- 1

    while(simteller<=nsim){
      #for (simteller in 1:nsim) {

      #ignore warning
      oldw <- getOption("warn")
      options(warn = -1)

      print(paste0("Iteration", simteller))

      for(j in 1:p){
        var1 <- VAR.sim(B=B, n=N, include="none")
        varxy[j,]<-var1[(N-M+1):N,]
      }

      xy <- mvrnorm(n = p, mu = rep(0,2), Sigma = Omega_pop, empirical=FALSE)
      RICLPMdata[, 1:M] <- varxy[, 1:M] + xy[,1]
      RICLPMdata[, (M+1):(2*M)] <- varxy[, (M+1):(2*M)] + xy[,2]


      # Fitting RI-CLPM
      riclpmModel2 <-
        '
      kappa =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
      omega =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      x5 ~ mu5*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1
      y5 ~ pi5*1


      # Random intercept part
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      kappa ~~ omega #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      p5 =~ 1*x5
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4
      q5 =~ 1*y5

      #Unconstrain autoregression and cross-lagged effects to be the same across both lags.

      p5 ~ alpha5*p4 + beta5*q4
      p4 ~ alpha4*p3 + beta4*q3
      p3 ~ alpha3*p2 + beta3*q2
      p2 ~ alpha2*p1 + beta2*q1


      q5 ~ delta5*q4 + gamma5*p4
      q4 ~ delta4*q3 + gamma4*p3
      q3 ~ delta3*q2 + gamma3*p2
      q2 ~ delta2*q1 + gamma2*p1


      p1 ~~ p1 #variance
      p2 ~~ u2*p2
      p3 ~~ u3*p3
      p4 ~~ u4*p4
      p5 ~~ u5*p5

      q1 ~~ q1 #variance
      q2 ~~ v2*q2
      q3 ~~ v3*q3
      q4 ~~ v4*q4
      q5 ~~ v5*q5


      p1 ~~ q1 #p1 and q1 covariance
      p2 ~~ u2v2*q2 #p2 and q2 covariance should also be constrained to be the same
      p3 ~~ u3v3*q3 #p2 and q2 covariance
      p4 ~~ u4v4*q4
      p5 ~~ u5v5*q5'


      riclpmUnconstrainedsim <- lavaan(riclpmModel2, data = RICLPMdata,
                                       missing = 'ML',
                                       int.ov.free = F,
                                       int.lv.free = F,
                                       auto.fix.first = F,
                                       auto.fix.single = F,
                                       auto.cov.lv.x = F,
                                       auto.cov.y = F,
                                       auto.var = F)

      warning <- any(is.na(parameterEstimates(riclpmUnconstrainedsim))[c(11:23,34:64),])
      notposdef <- any(eigen(lavInspect(riclpmUnconstrainedsim, "cov.lv"))$values <= 1e-8)

      if(warning == TRUE) {
        countNaN <- countNaN+1
        WarningTeller <- WarningTeller + 1
      } else if(notposdef == TRUE) {
        countNotPosDef <- countNotPosDef+1
        WarningTeller <- WarningTeller + 1
      } else { # if no warning, then do rest of code
        print(paste0("Model does not contain NAs and cov mx of latents is pos def"))

        # subtract for alpha, beta, delta, gamma (standardized estimates)
        StdEst <- standardizedsolution(riclpmUnconstrainedsim, type = "std.nox",
                                       se = TRUE, zstat = TRUE, pvalue = TRUE,
                                       ci = TRUE, level = 0.95, cov.std = TRUE,
                                       remove.eq = TRUE, remove.ineq = TRUE,
                                       remove.def = FALSE, partable = NULL,
                                       GLIST = NULL, est = NULL)

        est1 <- StdEst[c(34:49),5]
        names(est1) <- c("alpha5", "beta5", "alpha4", "beta4", "alpha3", "beta3", "alpha2", "beta2",  "delta5", "gamma5",  "delta4", "gamma4", "delta3", "gamma3", "delta2", "gamma2")
        par.est1[simteller,] <- est1
        meanEst1 <- apply(par.est1, 2, mean, na.rm=TRUE)
        names(meanEst1) <- c("alpha5", "beta5", "alpha4", "beta4", "alpha3", "beta3", "alpha2", "beta2",  "delta5", "gamma5",  "delta4", "gamma4", "delta3", "gamma3", "delta2", "gamma2")
        vcov <- lavInspect(riclpmUnconstrainedsim, "vcov.std.nox")[c(14:29), c(14:29)]
        par.est1[simteller,] <- est1
        meanEst1 <- apply(par.est1, 2, mean, na.rm=TRUE)
        names(meanEst1) <- c("alpha5", "beta5", "alpha4", "beta4", "alpha3", "beta3", "alpha2", "beta2",  "delta5", "gamma5",  "delta4", "gamma4", "delta3", "gamma3", "delta2", "gamma2")

        if (all(eigen(vcov)$values > 1e-10)) {

          eigen<-eigen(vcov)$values > 1e-10

          goricaResults1 <- goric(riclpmUnconstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

          # Store output

          GORICA.weights[simteller,] <- goricaResults1$result[,7]
          loglik.weights[simteller,] <- goricaResults1$result[,5]
          penalty.weights[simteller,] <- goricaResults1$result[,6]

          simteller<- simteller+1
        } else{nrposdef<- nrposdef+1}

        GORICA.power <- sum(GORICA.weights[,1] > GORICA.weights[,2])*(100/nsim)

        #ignore warnings
        on.exit(options(warn = oldw))
        round <- round + 1
      }
    }
    return(list(GORICA.power = GORICA.power, GORICA.weights = GORICA.weights, mean.estimates = meanEst1, loglik.weights = loglik.weights, penalty.weights = penalty.weights))
  }

  specific6measureBi <- function(nsim, p, B, H1,  M, Omega_pop) {

    print("The bivariate RI-CLPM for wave-specific parameters model with 6 measurement occasions is generating")

    q <- 2   # number of variables
    varxy <- array(NA, c(p, q * M)) # create array with nrow=p and ncol=n in order to keep simulate data
    colnames(varxy)<-c("x1", "x2", "x3","x4", "x5", "x6", "y1", "y2", "y3","y4", "y5", "y6")
    RICLPMdata <- array(NA, c(p, q * M))
    colnames(RICLPMdata)<-c("x1", "x2", "x3","x4","x5", "x6", "y1", "y2", "y3","y4","y5", "y6")

    par.est1 <- array(NA, dim=c(nsim, 20))

    GORICA.weights <- array(NA, dim=c(nsim, 2)) # 2 >> we have H1 and complementH1 now
    colnames(GORICA.weights) <- c("H1","complementH1")

    loglik.weights <- array(NA, dim=c(nsim, 2))
    colnames(loglik.weights) <- c("H1","complementH1")

    penalty.weights <- array(NA, dim=c(nsim, 2))
    colnames(penalty.weights) <- c("H1","complementH1")

    nrposdef <- 0
    round <-1 # set this to count number of iteration
    WarningTeller <- 0
    countNaN <- 0
    countNotPosDef <-0
    tellerOK <- 0
    simteller<- 1

    while(simteller <= nsim){
      #for (simteller in 1:nsim) {

      #ignore warning
      oldw <- getOption("warn")
      options(warn = -1)

      print(paste0("Iteration", simteller))

      for(j in 1:p){
        var1 <- VAR.sim(B=B, n=N, include="none")
        varxy[j,]<-var1[(N-M+1):N,]
      }

      xy <- mvrnorm(n = p, mu = rep(0,2), Sigma = Omega_pop, empirical=FALSE)
      RICLPMdata[, 1:M] <- varxy[, 1:M] + xy[,1]
      RICLPMdata[, (M+1):(2*M)] <- varxy[, (M+1):(2*M)] + xy[,2]


      # Fitting RI-CLPM
      riclpmModel2 <-
        '
      kappa =~ 1*x1 + 1*x2 + 1*x3+1*x4 + 1*x5 + 1*x6
      omega =~ 1*y1 + 1*y2 + 1*y3+1*y4 + 1*y5 + 1*y6

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      x5 ~ mu5*1
      x6 ~ mu6*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1
      y5 ~ pi5*1
      y6 ~ pi6*1

      # Random intercept part
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      kappa ~~ omega #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      p5 =~ 1*x5
      p6 =~ 1*x6
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4
      q5 =~ 1*y5
      q6 =~ 1*y6

      #Unconstrain autoregression and cross-lagged effects to be the same across both lags.
      p6 ~ alpha6*p5 + beta6*q5
      p5 ~ alpha5*p4 + beta5*q4
      p4 ~ alpha4*p3 + beta4*q3
      p3 ~ alpha3*p2 + beta3*q2
      p2 ~ alpha2*p1 + beta2*q1

      q6 ~ delta6*q5 + gamma6*p5
      q5 ~ delta5*q4 + gamma5*p4
      q4 ~ delta4*q3 + gamma4*p3
      q3 ~ delta3*q2 + gamma3*p2
      q2 ~ delta2*q1 + gamma2*p1


      p1 ~~ p1 #variance
      p2 ~~ u2*p2
      p3 ~~ u3*p3
      p4 ~~ u4*p4
      p5 ~~ u5*p5
      p6 ~~ u6*p6
      q1 ~~ q1 #variance
      q2 ~~ v2*q2
      q3 ~~ v3*q3
      q4 ~~ v4*q4
      q5 ~~ v5*q5
      q6 ~~ v6*q6

      p1 ~~ q1 #p1 and q1 covariance
      p2 ~~ u2v2*q2 #p2 and q2 covariance should also be constrained to be the same
      p3 ~~ u3v3*q3 #p2 and q2 covariance
      p4 ~~ u4v4*q4
      p5 ~~ u5v5*q5
      p6 ~~ u6v6*q6'

      riclpmUnconstrainedsim <- lavaan(riclpmModel2, data = RICLPMdata,
                                       missing = 'ML',
                                       int.ov.free = F,
                                       int.lv.free = F,
                                       auto.fix.first = F,
                                       auto.fix.single = F,
                                       auto.cov.lv.x = F,
                                       auto.cov.y = F,
                                       auto.var = F)

      warning <- any(is.na(parameterEstimates(riclpmUnconstrainedsim))[c(13:27,40:77),])
      notposdef <- any(eigen(lavInspect(riclpmUnconstrainedsim, "cov.lv"))$values <= 1e-8)

      if(warning == TRUE) {
        countNaN <- countNaN+1
        WarningTeller <- WarningTeller + 1
      } else if(notposdef == TRUE) {
        countNotPosDef <- countNotPosDef+1
        WarningTeller <- WarningTeller + 1
      } else { # if no warning, then do rest of code
        print(paste0("Model does not contain NAs and cov mx of latents is pos def"))

        # subtract for alpha, beta, delta, gamma (standardized estimates)
        StdEst <- standardizedsolution(riclpmUnconstrainedsim, type = "std.nox",
                                       se = TRUE, zstat = TRUE, pvalue = TRUE,
                                       ci = TRUE, level = 0.95, cov.std = TRUE,
                                       remove.eq = TRUE, remove.ineq = TRUE,
                                       remove.def = FALSE, partable = NULL,
                                       GLIST = NULL, est = NULL)

        est1 <- StdEst[c(40:59),5]
        names(est1) <- c("alpha6", "beta6", "alpha5", "beta5", "alpha4", "beta4", "alpha3", "beta3", "alpha2", "beta2",  "delta6", "gamma6",  "delta5", "gamma5",  "delta4", "gamma4", "delta3", "gamma3", "delta2", "gamma2")
        vcov<-lavInspect(riclpmUnconstrainedsim, "vcov.std.nox")[c(16:35), c(16:35)]
        par.est1[simteller,] <- est1
        meanEst1 <- apply(par.est1, 2, mean, na.rm=TRUE)
        names(meanEst1) <- c("alpha6", "beta6", "alpha5", "beta5", "alpha4", "beta4", "alpha3", "beta3", "alpha2", "beta2",  "delta6", "gamma6",  "delta5", "gamma5",  "delta4", "gamma4", "delta3", "gamma3", "delta2", "gamma2")

        if (all(eigen(vcov)$values > 1e-10)) {

          eigen<-eigen(vcov)$values > 1e-10
          goricaResults1 <- goric(riclpmUnconstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")
          # Store output

          GORICA.weights[simteller,] <- goricaResults1$result[,7]
          loglik.weights[simteller,] <- goricaResults1$result[,5]
          penalty.weights[simteller,] <- goricaResults1$result[,6]

          simteller<- simteller+1
        } else{nrposdef<- nrposdef+1}

        GORICA.power <- sum(GORICA.weights[,1] > GORICA.weights[,2])*(100/nsim)
        #ignore warnings
        on.exit(options(warn = oldw))
        round <- round + 1
      }
    }
    return(list(GORICA.power = GORICA.power, GORICA.weights = GORICA.weights, mean.estimates = meanEst1, loglik.weights = loglik.weights, penalty.weights = penalty.weights))
  }


  ############# Wave-independent parameters model in a tri-variate model ################################################

  independ3measureTri <- function(nsim, p, B, H1,  M, Omega_pop) {

    print("The Trivariate RI-CLPM for wave-independent parameters model with 3 measurement occasions is generating")

    q <- 3   # number of variables

    varxy <- array(NA, c(p, q*M)) # create array with nrow=p and ncol=n to keep simulate data
    colnames(varxy)<-c("x1", "x2", "x3", "y1", "y2", "y3", "z1", "z2", "z3")
    RICLPMdata <- array(NA, c(p, q*M))
    colnames(RICLPMdata)<-c("x1", "x2", "x3", "y1", "y2", "y3", "z1", "z2", "z3")

    par.est1 <- array(NA, dim=c(nsim, 9))

    GORICA.weights <- array(NA, dim=c(nsim, 2)) # 2 >> we have H1 and complementH1 now
    colnames(GORICA.weights) <- c("H1","complementH1")

    loglik.weights <- array(NA, dim=c(nsim, 2))
    colnames(loglik.weights) <- c("H1","complementH1")

    penalty.weights <- array(NA, dim=c(nsim, 2))
    colnames(penalty.weights) <- c("H1","complementH1")

    nrposdef <- 0
    round <- 1 # set this to count number of iteration
    WarningTeller <- 0
    countNaN <- 0
    countNotPosDef <- 0
    tellerOK <- 0
    simteller <- 1

    while (simteller <= nsim) {
      #ignore warning
      oldw <- getOption("warn")
      options(warn = -1)
      print(paste0("Iteration ", simteller))

      for(j in 1:p){
        var1 <- VAR.sim(B=B, n=N, include="none")
        varxy[j,]<-var1[(N-M+1):N,]
      }

      xy <- mvrnorm(n = p, mu = rep(0,3), Sigma = Omega_pop, empirical=FALSE)
      RICLPMdata[, 1:M] <- varxy[, 1:M] + xy[,1]
      RICLPMdata[, (M+1):(2*M)] <- varxy[, (M+1):(2*M)] + xy[,2]
      RICLPMdata[, (M+4):(3*M)] <- varxy[, (M+4):(3*M)] + xy[,3]

      #Chuenjai
      RICLPMdata <- scale(RICLPMdata) #standardizing

      # Fitting RI-CLPM
      riclpmModel<-
        '
      kappa =~ 1*x1 + 1*x2 + 1*x3
      omega =~ 1*y1 + 1*y2 + 1*y3
      zeta  =~ 1*z1 + 1*z2 + 1*z3

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      z1 ~ lambda1*1
      z2 ~ lambda2*1
      z3 ~ lambda3*1

      # Random intercept parts
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      zeta  ~~ zeta  #variance
      kappa ~~ omega #covariance
      kappa ~~ zeta  #covariance
      omega ~~ zeta  #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      r1 =~ 1*z1
      r2 =~ 1*z2
      r3 =~ 1*z3

      #constrain autoregression and cross-lagged effects to be the same across both lags.
      p3 ~ alpha*p2 + beta*q2 + phi*r2
      p2 ~ alpha*p1 + beta*q1 + phi*r1

      q3 ~ delta*q2 + gamma*p2 + psi*r2
      q2 ~ delta*q1 + gamma*p1 + psi*r1

      r3 ~ tau*r2 + epsilon*p2 + chi*q2
      r2 ~ tau*r1 + epsilon*p1 + chi*q1

      p1 ~~ p1 #variance
      p2 ~~ u*p2
      p3 ~~ u*p3
      q1 ~~ q1 #variance
      q2 ~~ v*q2
      q3 ~~ v*q3
      r1 ~~ r1 #variance
      r2 ~~ s*r2
      r3 ~~ s*r3

      p1 ~~ q1 #p1 and q1 covariance
      p1 ~~ r1 #p1 and r1 covariance
      q1 ~~ r1 #q1 and r1 covariance
      p2 ~~ uv*q2 #p2 and q2 covariance should also be constrained to be the same
      p2 ~~ us*r2
      q2 ~~ vs*r2
      p3 ~~ uv*q3 #p2 and q2 covariance
      p3 ~~ us*r3
      q3 ~~ vs*r3'

      riclpmConstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                     missing = 'ML',
                                     int.ov.free = F,
                                     int.lv.free = F,
                                     auto.fix.first = F,
                                     auto.fix.single = F,
                                     auto.cov.lv.x = F,
                                     auto.cov.y = F,
                                     auto.var = F)

      warning <- any(is.na(parameterEstimates(riclpmConstrainedsim, standardized = T))[c(10:24,34:69),])
      notposdef <- any(eigen(lavInspect(riclpmConstrainedsim, "cov.lv"))$values <= 1e-8)#*

      if(warning == TRUE) {
        countNaN <- countNaN+1
        WarningTeller <- WarningTeller + 1
      } else if(notposdef == TRUE) {
        countNotPosDef <- countNotPosDef+1
        WarningTeller <- WarningTeller + 1

      } else { # if no warning, then do rest of code
        print(paste0("Model does not contain NAs and cov mx of latents is pos def"))

        # subtract parameter estimates
        est1<-coef(riclpmConstrainedsim)[c(16:18, 22:24,28:30)]
        names(est1) <- c("alpha", "beta", "phi", "delta", "gamma", "psi", "tau", "epsilon", "chi")
        vcov<-lavInspect(riclpmConstrainedsim, "vcov")[c(16:18, 22:24,28:30), c(16:18, 22:24,28:30)]
        par.est1[simteller,] <- est1
        meanEst1 <- apply(par.est1, 2, mean, na.rm=TRUE)
        names(meanEst1) <- c("alpha", "beta", "phi", "delta", "gamma", "psi", "tau", "epsilon", "chi")

        if (all(eigen(vcov)$values > 1e-10)) { #*

          eigen<-eigen(vcov)$values > 1e-10

          goricaResults1 <- goric(riclpmConstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

          # Store output

          GORICA.weights[simteller,] <- goricaResults1$result[,7]
          loglik.weights[simteller,] <- goricaResults1$result[,5]
          penalty.weights[simteller,] <- goricaResults1$result[,6]

          simteller<- simteller+1
        }

        GORICA.power <- sum(GORICA.weights[,1] > GORICA.weights[,2])*(100/nsim)
        #ignore warnings
        on.exit(options(warn = oldw))
        round <- round + 1
      }
    }
    return(list(GORICA.power = GORICA.power, GORICA.weights = GORICA.weights, mean.estimates = meanEst1, loglik.weights = loglik.weights, penalty.weights = penalty.weights))
  }

  independ4measureTri <- function(nsim, p, B, H1,  M, Omega_pop) {

    print("The Trivariate RI-CLPM for wave-independent parameters model with 4 measurement occasions is generating")

    q <- 3   # number of variables
    varxy <- array(NA, c(p, q*M)) # create array with nrow=p and ncol=n to keep simulate data
    colnames(varxy)<-c("x1", "x2", "x3", "x4", "y1", "y2", "y3", "y4", "z1", "z2", "z3", "z4")
    RICLPMdata <- array(NA, c(p, q*M))
    colnames(RICLPMdata)<-c("x1", "x2", "x3", "x4", "y1", "y2", "y3", "y4", "z1", "z2", "z3", "z4")

    par.est1 <- array(NA, dim=c(nsim, 9))

    GORICA.weights <- array(NA, dim=c(nsim, 2)) # 2 >> we have H1 and complementH1 now
    colnames(GORICA.weights) <- c("H1","complementH1")

    loglik.weights <- array(NA, dim=c(nsim, 2))
    colnames(loglik.weights) <- c("H1","complementH1")

    penalty.weights <- array(NA, dim=c(nsim, 2))
    colnames(penalty.weights) <- c("H1","complementH1")

    nrposdef <- 0
    round <-1 # set this to count number of iteration
    WarningTeller <- 0
    countNaN <- 0
    countNotPosDef <-0
    tellerOK <- 0
    simteller<- 1

    while(simteller<=nsim){

      #ignore warning
      oldw <- getOption("warn")
      options(warn = -1)

      print(paste0("Iteration", simteller))

      for(j in 1:p){
        var1 <- VAR.sim(B=B, n=N, include="none")
        varxy[j,]<-var1[(N-M+1):N,]
      }

      xy <- mvrnorm(n = p, mu = rep(0,3), Sigma = Omega_pop, empirical=FALSE)
      RICLPMdata[, 1:M] <- varxy[, 1:M] + xy[,1]
      RICLPMdata[, (M+1):(2*M)] <- varxy[, (M+1):(2*M)] + xy[,2]
      RICLPMdata[, (M+4):(3*M)] <- varxy[, (M+4):(3*M)] + xy[,3]

      RICLPMdata <- scale(RICLPMdata) #standardizing

      # Fitting RI-CLPM
      riclpmModel<-
        '
      kappa =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
      omega =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
      zeta  =~ 1*z1 + 1*z2 + 1*z3 + 1*z4

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1
      z1 ~ lambda1*1
      z2 ~ lambda2*1
      z3 ~ lambda3*1
      z4 ~ lambda4*1

      # Random intercept parts
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      zeta  ~~ zeta  #variance
      kappa ~~ omega #covariance
      kappa ~~ zeta  #covariance
      omega ~~ zeta  #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4
      r1 =~ 1*z1
      r2 =~ 1*z2
      r3 =~ 1*z3
      r4 =~ 1*z4

      #constrain autoregression and cross-lagged effects to be the same across both lags.
      p4 ~ alpha*p3 + beta*q3 + phi*r3
      p3 ~ alpha*p2 + beta*q2 + phi*r2
      p2 ~ alpha*p1 + beta*q1 + phi*r1

      q4 ~ delta*q3 + gamma*p3 + psi*r3
      q3 ~ delta*q2 + gamma*p2 + psi*r2
      q2 ~ delta*q1 + gamma*p1 + psi*r1

      r4 ~ tau*r3 + epsilon*p3 + chi*q3
      r3 ~ tau*r2 + epsilon*p2 + chi*q2
      r2 ~ tau*r1 + epsilon*p1 + chi*q1

      p1 ~~ p1 #variance
      p2 ~~ u*p2
      p3 ~~ u*p3
      p4 ~~ u*p4
      q1 ~~ q1 #variance
      q2 ~~ v*q2
      q3 ~~ v*q3
      q4 ~~ v*q4
      r1 ~~ r1 #variance
      r2 ~~ s*r2
      r3 ~~ s*r3
      r4 ~~ s*r4

      p1 ~~ q1 #p1 and q1 covariance
      p1 ~~ r1 #p1 and r1 covariance
      q1 ~~ r1 #q1 and r1 covariance
      p2 ~~ uv*q2 #p2 and q2 covariance should also be constrained to be the same
      p2 ~~ us*r2
      q2 ~~ vs*r2
      p3 ~~ uv*q3 #p3 and q3 covariance
      p3 ~~ us*r3
      q3 ~~ vs*r3
      p4 ~~ uv*q4 #p4 and q4 covariance
      p4 ~~ us*r4
      q4 ~~ vs*r4'


      riclpmConstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                     missing = 'ML',
                                     int.ov.free = F,
                                     int.lv.free = F,
                                     auto.fix.first = F,
                                     auto.fix.single = F,
                                     auto.cov.lv.x = F,
                                     auto.cov.y = F,
                                     auto.var = F)

      warning <- any(is.na(parameterEstimates(riclpmConstrainedsim))[c(13:30,43:93),])
      notposdef <- any(eigen(lavInspect(riclpmConstrainedsim, "cov.lv"))$values <= 1e-8)

      if(warning == TRUE) {
        countNaN <- countNaN+1
        WarningTeller <- WarningTeller + 1
      } else if(notposdef == TRUE) {
        countNotPosDef <- countNotPosDef+1
        WarningTeller <- WarningTeller + 1
      } else { # if no warning, then do rest of code
        print(paste0("Model does not contain NAs and cov mx of latents is pos def"))

        # subtract for alpha, beta, delta, gamma
        est1<-coef(riclpmConstrainedsim)[c(19:21, 28:30,37:39)]
        names(est1) <- c("alpha", "beta", "phi", "delta", "gamma", "psi", "tau", "epsilon", "chi")
        vcov<-lavInspect(riclpmConstrainedsim, "vcov")[c(19:21, 28:30,37:39), c(19:21, 28:30,37:39)]
        par.est1[simteller,] <- est1
        meanEst1 <- apply(par.est1, 2, mean, na.rm=TRUE)
        names(meanEst1) <- c("alpha", "beta", "phi", "delta", "gamma", "psi", "tau", "epsilon", "chi")

        if (all(eigen(vcov)$values > 1e-10)) {

          eigen<-eigen(vcov)$values > 1e-10

          goricaResults1 <- goric(riclpmConstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")
          # Store output

          GORICA.weights[simteller,] <- goricaResults1$result[,7]
          loglik.weights[simteller,] <- goricaResults1$result[,5]
          penalty.weights[simteller,] <- goricaResults1$result[,6]

          simteller<- simteller+1
        } else{nrposdef<- nrposdef+1}

        GORICA.power <- sum(GORICA.weights[,1] > GORICA.weights[,2])*(100/nsim)
        #ignore warnings
        on.exit(options(warn = oldw))
        round <- round + 1
      }
    }
    return(list(GORICA.power = GORICA.power, GORICA.weights = GORICA.weights, mean.estimates = meanEst1, loglik.weights = loglik.weights, penalty.weights = penalty.weights))
  }

  independ5measureTri <- function(nsim, p, B, H1,  M, Omega_pop) {

    print("The Trivariate RI-CLPM for wave-independent parameters model with 5 measurement occasions is generating")

    q <- 3   # number of variables

    varxy <- array(NA, c(p, q*M)) # create array with nrow=p and ncol=n to keep simulate data
    colnames(varxy)<-c("x1", "x2", "x3", "x4", "x5", "y1", "y2", "y3", "y4", "y5", "z1", "z2", "z3", "z4", "z5")
    RICLPMdata <- array(NA, c(p, q*M))
    colnames(RICLPMdata)<-c("x1", "x2", "x3", "x4", "x5", "y1", "y2", "y3", "y4", "y5", "z1", "z2", "z3", "z4", "z5")

    par.est1 <- array(NA, dim=c(nsim, 9))

    GORICA.weights <- array(NA, dim=c(nsim, 2)) # 2 >> we have H1 and complementH1 now
    colnames(GORICA.weights) <- c("H1","complementH1")

    loglik.weights <- array(NA, dim=c(nsim, 2))
    colnames(loglik.weights) <- c("H1","complementH1")

    penalty.weights <- array(NA, dim=c(nsim, 2))
    colnames(penalty.weights) <- c("H1","complementH1")

    nrposdef <- 0
    round <-1 # set this to count number of iteration
    WarningTeller <- 0
    countNaN <- 0
    countNotPosDef <-0
    tellerOK <- 0
    simteller<- 1

    while(simteller<=nsim){
      #for (simteller in 1:nsim) {

      #ignore warning
      oldw <- getOption("warn")
      options(warn = -1)

      print(paste0("Iteration", simteller))

      for(j in 1:p){
        var1 <- VAR.sim(B=B, n=N, include="none")
        varxy[j,]<-var1[(N-M+1):N,]
      }

      xy <- mvrnorm(n = p, mu = rep(0,3), Sigma = Omega_pop, empirical=FALSE)
      RICLPMdata[, 1:M] <- varxy[, 1:M] + xy[,1]
      RICLPMdata[, (M+1):(2*M)] <- varxy[, (M+1):(2*M)] + xy[,2]
      RICLPMdata[, (M+4):(3*M)] <- varxy[, (M+4):(3*M)] + xy[,3]

      RICLPMdata <- scale(RICLPMdata) #standardizing

      # Fitting RI-CLPM
      riclpmModel<-
        '
      kappa =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
      omega =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
      zeta  =~ 1*z1 + 1*z2 + 1*z3 + 1*z4 + 1*z5

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      x5 ~ mu5*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1
      y5 ~ pi5*1
      z1 ~ lambda1*1
      z2 ~ lambda2*1
      z3 ~ lambda3*1
      z4 ~ lambda4*1
      z5 ~ lambda5*1

      # Random intercept parts
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      zeta  ~~ zeta  #variance
      kappa ~~ omega #covariance
      kappa ~~ zeta  #covariance
      omega ~~ zeta  #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      p5 =~ 1*x5
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4
      q5 =~ 1*y5
      r1 =~ 1*z1
      r2 =~ 1*z2
      r3 =~ 1*z3
      r4 =~ 1*z4
      r5 =~ 1*z5

      #constrain autoregression and cross-lagged effects to be the same across both lags.
      p5 ~ alpha*p4 + beta*q4 + phi*r4
      p4 ~ alpha*p3 + beta*q3 + phi*r3
      p3 ~ alpha*p2 + beta*q2 + phi*r2
      p2 ~ alpha*p1 + beta*q1 + phi*r1

      q5 ~ delta*q4 + gamma*p4 + psi*r4
      q4 ~ delta*q3 + gamma*p3 + psi*r3
      q3 ~ delta*q2 + gamma*p2 + psi*r2
      q2 ~ delta*q1 + gamma*p1 + psi*r1

      r5 ~ tau*r4 + epsilon*p4 + chi*q4
      r4 ~ tau*r3 + epsilon*p3 + chi*q3
      r3 ~ tau*r2 + epsilon*p2 + chi*q2
      r2 ~ tau*r1 + epsilon*p1 + chi*q1

      p1 ~~ p1 #variance
      p2 ~~ u*p2
      p3 ~~ u*p3
      p4 ~~ u*p4
      p5 ~~ u*p5
      q1 ~~ q1 #variance
      q2 ~~ v*q2
      q3 ~~ v*q3
      q4 ~~ v*q4
      q5 ~~ v*q5
      r1 ~~ r1 #variance
      r2 ~~ s*r2
      r3 ~~ s*r3
      r4 ~~ s*r4
      r5 ~~ s*r5

      p1 ~~ q1 #p1 and q1 covariance
      p1 ~~ r1 #p1 and r1 covariance
      q1 ~~ r1 #q1 and r1 covariance
      p2 ~~ uv*q2 #p2 and q2 covariance should also be constrained to be the same
      p2 ~~ us*r2
      q2 ~~ vs*r2
      p3 ~~ uv*q3 #p3 and q3 covariance
      p3 ~~ us*r3
      q3 ~~ vs*r3
      p4 ~~ uv*q4 #p4 and q4 covariance
      p4 ~~ us*r4
      q4 ~~ vs*r4
      p5 ~~ uv*q5 #p5 and q5 covariance
      p5 ~~ us*r5
      q5 ~~ vs*r5'


      riclpmConstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                     missing = 'ML',
                                     int.ov.free = F,
                                     int.lv.free = F,
                                     auto.fix.first = F,
                                     auto.fix.single = F,
                                     auto.cov.lv.x = F,
                                     auto.cov.y = F,
                                     auto.var = F)

      warning <- any(is.na(parameterEstimates(riclpmConstrainedsim))[c(16:36,52:117),])
      notposdef <- any(eigen(lavInspect(riclpmConstrainedsim, "cov.lv"))$values <= 1e-8)

      if(warning == TRUE) {
        countNaN <- countNaN+1
        WarningTeller <- WarningTeller + 1
      } else if(notposdef == TRUE) {
        countNotPosDef <- countNotPosDef+1
        WarningTeller <- WarningTeller + 1
      } else { # if no warning, then do rest of code
        print(paste0("Model does not contain NAs and cov mx of latents is pos def"))

        # subtract for alpha, beta, delta, gamma
        est1<-coef(riclpmConstrainedsim)[c(22:24, 34:36,46:48)]
        names(est1) <- c("alpha", "beta", "phi", "delta", "gamma", "psi", "tau", "epsilon", "chi")
        vcov<-lavInspect(riclpmConstrainedsim, "vcov")[c(22:24, 34:36,46:48), c(22:24, 34:36,46:48)]
        par.est1[simteller,] <- est1
        meanEst1 <- apply(par.est1, 2, mean, na.rm=TRUE)
        names(meanEst1) <- c("alpha", "beta", "phi", "delta", "gamma", "psi", "tau", "epsilon", "chi")

        if (all(eigen(vcov)$values > 1e-10)) {

          eigen<-eigen(vcov)$values > 1e-10

          goricaResults1 <- goric(riclpmConstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

          # Store output

          GORICA.weights[simteller,] <- goricaResults1$result[,7]
          loglik.weights[simteller,] <- goricaResults1$result[,5]
          penalty.weights[simteller,] <- goricaResults1$result[,6]

          simteller<- simteller+1
        } else{nrposdef<- nrposdef+1}

        GORICA.power <- sum(GORICA.weights[,1] > GORICA.weights[,2])*(100/nsim)
        #ignore warnings
        on.exit(options(warn = oldw))
        round <- round + 1
      }
    }
    return(list(GORICA.power = GORICA.power, GORICA.weights = GORICA.weights, mean.estimates = meanEst1, loglik.weights = loglik.weights, penalty.weights = penalty.weights))
  }

  independ6measureTri <- function(nsim, p, B, H1,  M, Omega_pop) {

    print("The Trivariate RI-CLPM for wave-inindependent parameters model with 6 measurement occasions is generating")

    q <- 3   # number of variables
    varxy <- array(NA, c(p, q*M)) # create array with nrow=p and ncol=n to keep simulate data
    colnames(varxy)<-c("x1", "x2", "x3", "x4", "x5", "x6", "y1", "y2", "y3", "y4", "y5", "y6", "z1", "z2", "z3", "z4", "z5", "z6")
    RICLPMdata <- array(NA, c(p, q*M))
    colnames(RICLPMdata)<-c("x1", "x2", "x3", "x4", "x5", "x6", "y1", "y2", "y3", "y4", "y5", "y6", "z1", "z2", "z3", "z4", "z5", "z6")

    par.est1 <- array(NA, dim=c(nsim, 9))

    GORICA.weights <- array(NA, dim=c(nsim, 2)) # 2 >> we have H1 and complementH1 now
    colnames(GORICA.weights) <- c("H1","complementH1")

    loglik.weights <- array(NA, dim=c(nsim, 2))
    colnames(loglik.weights) <- c("H1","complementH1")

    penalty.weights <- array(NA, dim=c(nsim, 2))
    colnames(penalty.weights) <- c("H1","complementH1")

    nrposdef <- 0
    round <-1 # set this to count number of iteration
    WarningTeller <- 0
    countNaN <- 0
    countNotPosDef <-0
    tellerOK <- 0
    simteller<- 1

    while(simteller <= nsim){
      #for (simteller in 1:nsim) {

      #ignore warning
      oldw <- getOption("warn")
      options(warn = -1)

      print(paste0("Iteration", simteller))

      for(j in 1:p){
        var1 <- VAR.sim(B=B, n=N, include="none")
        varxy[j,]<-var1[(N-M+1):N,]
      }

      xy <- mvrnorm(n = p, mu = rep(0,3), Sigma = Omega_pop, empirical=FALSE)
      RICLPMdata[, 1:M] <- varxy[, 1:M] + xy[,1]
      RICLPMdata[, (M+1):(2*M)] <- varxy[, (M+1):(2*M)] + xy[,2]
      RICLPMdata[, (M+4):(3*M)] <- varxy[, (M+4):(3*M)] + xy[,3]

      RICLPMdata <- scale(RICLPMdata) #standardizing

      # Fitting RI-CLPM
      riclpmModel<-
        '
      kappa =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5 + 1*x6
      omega =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5 + 1*y6
      zeta  =~ 1*z1 + 1*z2 + 1*z3 + 1*z4 + 1*z5 + 1*z6

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      x5 ~ mu5*1
      x6 ~ mu6*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1
      y5 ~ pi5*1
      y6 ~ pi6*1
      z1 ~ lambda1*1
      z2 ~ lambda2*1
      z3 ~ lambda3*1
      z4 ~ lambda4*1
      z5 ~ lambda5*1
      z6 ~ lambda6*1

      # Random intercept parts
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      zeta  ~~ zeta  #variance
      kappa ~~ omega #covariance
      kappa ~~ zeta  #covariance
      omega ~~ zeta  #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      p5 =~ 1*x5
      p6 =~ 1*x6
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4
      q5 =~ 1*y5
      q6 =~ 1*y6
      r1 =~ 1*z1
      r2 =~ 1*z2
      r3 =~ 1*z3
      r4 =~ 1*z4
      r5 =~ 1*z5
      r6 =~ 1*z6

      #constrain autoregression and cross-lagged effects to be the same across both lags.
      p6 ~ alpha*p5 + beta*q5 + phi*r5
      p5 ~ alpha*p4 + beta*q4 + phi*r4
      p4 ~ alpha*p3 + beta*q3 + phi*r3
      p3 ~ alpha*p2 + beta*q2 + phi*r2
      p2 ~ alpha*p1 + beta*q1 + phi*r1

      q6 ~ delta*q5 + gamma*p5 + psi*r5
      q5 ~ delta*q4 + gamma*p4 + psi*r4
      q4 ~ delta*q3 + gamma*p3 + psi*r3
      q3 ~ delta*q2 + gamma*p2 + psi*r2
      q2 ~ delta*q1 + gamma*p1 + psi*r1

      r6 ~ tau*r5 + epsilon*p5 + chi*q5
      r5 ~ tau*r4 + epsilon*p4 + chi*q4
      r4 ~ tau*r3 + epsilon*p3 + chi*q3
      r3 ~ tau*r2 + epsilon*p2 + chi*q2
      r2 ~ tau*r1 + epsilon*p1 + chi*q1

      p1 ~~ p1 #variance
      p2 ~~ u*p2
      p3 ~~ u*p3
      p4 ~~ u*p4
      p5 ~~ u*p5
      p6 ~~ u*p6
      q1 ~~ q1 #variance
      q2 ~~ v*q2
      q3 ~~ v*q3
      q4 ~~ v*q4
      q5 ~~ v*q5
      q6 ~~ v*q6
      r1 ~~ r1 #variance
      r2 ~~ s*r2
      r3 ~~ s*r3
      r4 ~~ s*r4
      r5 ~~ s*r5
      r6 ~~ s*r6

      p1 ~~ q1 #p1 and q1 covariance
      p1 ~~ r1 #p1 and r1 covariance
      q1 ~~ r1 #q1 and r1 covariance
      p2 ~~ uv*q2 #p2 and q2 covariance should also be constrained to be the same
      p2 ~~ us*r2
      q2 ~~ vs*r2
      p3 ~~ uv*q3 #p3 and q3 covariance
      p3 ~~ us*r3
      q3 ~~ vs*r3
      p4 ~~ uv*q4 #p4 and q4 covariance
      p4 ~~ us*r4
      q4 ~~ vs*r4
      p5 ~~ uv*q5 #p5 and q5 covariance
      p5 ~~ us*r5
      q5 ~~ vs*r5
      p6 ~~ uv*q6 #p6 and q6 covariance
      p6 ~~ us*r6
      q6 ~~ vs*r6'


      riclpmConstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                     missing = 'ML',
                                     int.ov.free = F,
                                     int.lv.free = F,
                                     auto.fix.first = F,
                                     auto.fix.single = F,
                                     auto.cov.lv.x = F,
                                     auto.cov.y = F,
                                     auto.var = F)

      warning <- any(is.na(parameterEstimates(riclpmConstrainedsim))[c(19:42,61:141),])
      notposdef <- any(eigen(lavInspect(riclpmConstrainedsim, "cov.lv"))$values <= 1e-8)

      if(warning == TRUE) {
        countNaN <- countNaN+1
        WarningTeller <- WarningTeller + 1
      } else if(notposdef == TRUE) {
        countNotPosDef <- countNotPosDef+1
        WarningTeller <- WarningTeller + 1
      } else { # if no warning, then do rest of code
        print(paste0("Model does not contain NAs and cov mx of latents is pos def"))

        # subtract for alpha, beta, delta, gamma
        est1<-coef(riclpmConstrainedsim)[c(25:27, 40:42,55:57)]
        names(est1) <- c("alpha", "beta", "phi", "delta", "gamma", "psi", "tau", "epsilon", "chi")
        vcov<-lavInspect(riclpmConstrainedsim, "vcov")[c(25:27, 40:42,55:57), c(25:27, 40:42,55:57)]
        par.est1[simteller,] <- est1
        meanEst1 <- apply(par.est1, 2, mean, na.rm=TRUE)
        names(meanEst1) <- c("alpha", "beta", "phi", "delta", "gamma", "psi", "tau", "epsilon", "chi")

        if (all(eigen(vcov)$values > 1e-10)) {

          eigen<-eigen(vcov)$values > 1e-10
          goricaResults1 <- goric(riclpmConstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")
          # Store output

          GORICA.weights[simteller,] <- goricaResults1$result[,7]
          loglik.weights[simteller,] <- goricaResults1$result[,5]
          penalty.weights[simteller,] <- goricaResults1$result[,6]

          simteller<- simteller+1
        } else{nrposdef<- nrposdef+1}

        GORICA.power <- sum(GORICA.weights[,1] > GORICA.weights[,2])*(100/nsim)
        #ignore warnings
        on.exit(options(warn = oldw))
        round <- round + 1
      }
    }
    return(list(GORICA.power = GORICA.power, GORICA.weights = GORICA.weights, mean.estimates = meanEst1, loglik.weights = loglik.weights, penalty.weights = penalty.weights))
  }

  ################### Wave-specific parameters model in a tri-variate model ################################################

  specific3measureTri <- function(nsim, p, B, H1,  M, Omega_pop) {

    print("The Trivariate RI-CLPM for wave-specific parameters model with 3 measurement occasions is generating")

    q <- 3   # number of variables

    varxy <- array(NA, c(p, q*M)) # create array with nrow=p and ncol=n to keep simulate data
    colnames(varxy)<-c("x1", "x2", "x3", "y1", "y2", "y3", "z1", "z2", "z3")
    RICLPMdata <- array(NA, c(p, q*M))
    colnames(RICLPMdata)<-c("x1", "x2", "x3", "y1", "y2", "y3", "z1", "z2", "z3")

    par.est1 <- array(NA, dim=c(nsim, 18))

    GORICA.weights <- array(NA, dim=c(nsim, 2)) # 2 >> we have H1 and complementH1 now
    colnames(GORICA.weights) <- c("H1","complementH1")

    loglik.weights <- array(NA, dim=c(nsim, 2))
    colnames(loglik.weights) <- c("H1","complementH1")

    penalty.weights <- array(NA, dim=c(nsim, 2))
    colnames(penalty.weights) <- c("H1","complementH1")

    nrposdef <- 0
    round <- 1 # set this to count number of iteration
    WarningTeller <- 0
    countNaN <- 0
    countNotPosDef <- 0
    tellerOK <- 0
    simteller <- 1

    while (simteller <= nsim) {
      #ignore warning
      oldw <- getOption("warn")
      options(warn = -1)
      print(paste0("Iteration ", simteller))

      for(j in 1:p){
        var1 <- VAR.sim(B=B, n=N, include="none")
        varxy[j,]<-var1[(N-M+1):N,]
      }

      xy <- mvrnorm(n = p, mu = rep(0,3), Sigma = Omega_pop, empirical=FALSE)
      RICLPMdata[, 1:M] <- varxy[, 1:M] + xy[,1]
      RICLPMdata[, (M+1):(2*M)] <- varxy[, (M+1):(2*M)] + xy[,2]
      RICLPMdata[, (M+4):(3*M)] <- varxy[, (M+4):(3*M)] + xy[,3]

      # Fitting RI-CLPM
      riclpmModel<-
        '
      kappa =~ 1*x1 + 1*x2 + 1*x3
      omega =~ 1*y1 + 1*y2 + 1*y3
      zeta  =~ 1*z1 + 1*z2 + 1*z3

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      z1 ~ lambda1*1
      z2 ~ lambda2*1
      z3 ~ lambda3*1

      # Random intercept parts
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      zeta  ~~ zeta  #variance
      kappa ~~ omega #covariance
      kappa ~~ zeta  #covariance
      omega ~~ zeta  #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      r1 =~ 1*z1
      r2 =~ 1*z2
      r3 =~ 1*z3

      #Unconstrain autoregression and cross-lagged effects to be the same across both lags.
      p3 ~ alpha3*p2 + beta3*q2 + phi3*r2
      p2 ~ alpha2*p1 + beta2*q1 + phi2*r1

      q3 ~ delta3*q2 + gamma3*p2 + psi3*r2
      q2 ~ delta2*q1 + gamma2*p1 + psi2*r1

      r3 ~ tau3*r2 + epsilon3*p2 + chi3*q2
      r2 ~ tau2*r1 + epsilon2*p1 + chi2*q1

      p1 ~~ p1 #variance
      p2 ~~ u2*p2
      p3 ~~ u3*p3
      q1 ~~ q1 #variance
      q2 ~~ v2*q2
      q3 ~~ v3*q3
      r1 ~~ r1 #variance
      r2 ~~ s2*r2
      r3 ~~ s3*r3

      p1 ~~ q1 #p1 and q1 covariance
      p1 ~~ r1 #p1 and r1 covariance
      q1 ~~ r1 #q1 and r1 covariance
      p2 ~~ u2v2*q2 #p2 and q2 covariance should also be constrained to be the same
      p2 ~~ u2s2*r2
      q2 ~~ v2s2*r2
      p3 ~~ u3v3*q3 #p2 and q2 covariance
      p3 ~~ u3s3*r3
      q3 ~~ v3s3*r3'

      riclpmUnconstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                       missing = 'ML',
                                       int.ov.free = F,
                                       int.lv.free = F,
                                       auto.fix.first = F,
                                       auto.fix.single = F,
                                       auto.cov.lv.x = F,
                                       auto.cov.y = F,
                                       auto.var = F)

      warning <- any(is.na(parameterEstimates(riclpmUnconstrainedsim))[c(10:24,34:69),])
      notposdef <- any(eigen(lavInspect(riclpmUnconstrainedsim, "cov.lv"))$values <= 1e-8)#*


      if(warning == TRUE) {
        countNaN <- countNaN+1
        WarningTeller <- WarningTeller + 1
      } else if(notposdef == TRUE) {
        countNotPosDef <- countNotPosDef+1
        WarningTeller <- WarningTeller + 1

      } else { # if no warning, then do rest of code
        print(paste0("Model does not contain NAs and cov mx of latents is pos def"))

        # subtract standardized estimates
        StdEst <- standardizedsolution(riclpmUnconstrainedsim, type = "std.nox",
                                       se = TRUE, zstat = TRUE, pvalue = TRUE,
                                       ci = TRUE, level = 0.95, cov.std = TRUE,
                                       remove.eq = TRUE, remove.ineq = TRUE,
                                       remove.def = FALSE, partable = NULL,
                                       GLIST = NULL, est = NULL)

        est1 <- StdEst[c(34:51),5]
        names(est1) <- c("alpha3", "beta3", "phi3", "alpha2", "beta2", "phi2", "delta3", "gamma3", "psi3", "delta2", "gamma2", "psi2", "tau3", "epsilon3", "chi3", "tau2", "epsilon2", "chi2")
        vcov <- lavInspect(riclpmUnconstrainedsim, "vcov.std.nox")[c(16:33), c(16:33)]
        par.est1[simteller,] <- est1
        meanEst1 <- apply(par.est1, 2, mean, na.rm=TRUE)
        names(meanEst1) <- c("alpha3", "beta3", "phi3", "alpha2", "beta2", "phi2", "delta3", "gamma3", "psi3", "delta2", "gamma2", "psi2", "tau3", "epsilon3", "chi3", "tau2", "epsilon2", "chi2")

        if (all(eigen(vcov)$values > 1e-10)) { #*

          eigen<-eigen(vcov)$values > 1e-10

          goricaResults1 <- goric(riclpmUnconstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

          # Store output

          GORICA.weights[simteller,] <- goricaResults1$result[,7]
          loglik.weights[simteller,] <- goricaResults1$result[,5]
          penalty.weights[simteller,] <- goricaResults1$result[,6]

          simteller<- simteller+1
        }

        GORICA.power <- sum(GORICA.weights[,1] > GORICA.weights[,2])*(100/nsim)
        #ignore warnings
        on.exit(options(warn = oldw))
        round <- round + 1
      }
    }
    return(list(GORICA.power = GORICA.power, GORICA.weights = GORICA.weights, mean.estimates = meanEst1, loglik.weights = loglik.weights, penalty.weights = penalty.weights))
  }

  specific4measureTri <- function(nsim, p, B, H1,  M, Omega_pop) {

    print("The Trivariate RI-CLPM for wave-specific parameters model with 4 measurement occasions is generating")

    q <- 3   # number of variables
    varxy <- array(NA, c(p, q*M)) # create array with nrow=p and ncol=n to keep simulate data
    colnames(varxy)<-c("x1", "x2", "x3", "x4", "y1", "y2", "y3", "y4", "z1", "z2", "z3", "z4")
    RICLPMdata <- array(NA, c(p, q*M))
    colnames(RICLPMdata)<-c("x1", "x2", "x3", "x4", "y1", "y2", "y3", "y4", "z1", "z2", "z3", "z4")

    par.est1 <- array(NA, dim=c(nsim, 27))

    GORICA.weights <- array(NA, dim=c(nsim, 2)) # 2 >> we have H1 and complementH1 now
    colnames(GORICA.weights) <- c("H1","complementH1")

    loglik.weights <- array(NA, dim=c(nsim, 2))
    colnames(loglik.weights) <- c("H1","complementH1")

    penalty.weights <- array(NA, dim=c(nsim, 2))
    colnames(penalty.weights) <- c("H1","complementH1")

    nrposdef <- 0
    round <-1 # set this to count number of iteration
    WarningTeller <- 0
    countNaN <- 0
    countNotPosDef <-0
    tellerOK <- 0
    simteller<- 1

    while(simteller<=nsim){

      #ignore warning
      oldw <- getOption("warn")
      options(warn = -1)

      print(paste0("Iteration", simteller))

      for(j in 1:p){
        var1 <- VAR.sim(B=B, n=N, include="none")
        varxy[j,]<-var1[(N-M+1):N,]
      }

      xy <- mvrnorm(n = p, mu = rep(0,3), Sigma = Omega_pop, empirical=FALSE)
      RICLPMdata[, 1:M] <- varxy[, 1:M] + xy[,1]
      RICLPMdata[, (M+1):(2*M)] <- varxy[, (M+1):(2*M)] + xy[,2]
      RICLPMdata[, (M+4):(3*M)] <- varxy[, (M+4):(3*M)] + xy[,3]


      # Fitting RI-CLPM
      riclpmModel<-
        '
      kappa =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
      omega =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
      zeta  =~ 1*z1 + 1*z2 + 1*z3 + 1*z4

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1
      z1 ~ lambda1*1
      z2 ~ lambda2*1
      z3 ~ lambda3*1
      z4 ~ lambda4*1

      # Random intercept parts
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      zeta  ~~ zeta  #variance
      kappa ~~ omega #covariance
      kappa ~~ zeta  #covariance
      omega ~~ zeta  #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4
      r1 =~ 1*z1
      r2 =~ 1*z2
      r3 =~ 1*z3
      r4 =~ 1*z4

      #Unconstrain autoregression and cross-lagged effects.
      p4 ~ alpha4*p3 + beta4*q3 + phi4*r3
      p3 ~ alpha3*p2 + beta3*q2 + phi3*r2
      p2 ~ alpha2*p1 + beta2*q1 + phi2*r1

      q4 ~ delta4*q3 + gamma4*p3 + psi4*r3
      q3 ~ delta3*q2 + gamma3*p2 + psi3*r2
      q2 ~ delta2*q1 + gamma2*p1 + psi2*r1

      r4 ~ tau4*r3 + epsilon4*p3 + chi4*q3
      r3 ~ tau3*r2 + epsilon3*p2 + chi3*q2
      r2 ~ tau2*r1 + epsilon2*p1 + chi2*q1

      p1 ~~ p1 #variance
      p2 ~~ u2*p2
      p3 ~~ u3*p3
      p4 ~~ u4*p4
      q1 ~~ q1 #variance
      q2 ~~ v2*q2
      q3 ~~ v3*q3
      q4 ~~ v4*q4
      r1 ~~ r1 #variance
      r2 ~~ s2*r2
      r3 ~~ s3*r3
      r4 ~~ s4*r4

      p1 ~~ q1 #p1 and q1 covariance
      p1 ~~ r1 #p1 and r1 covariance
      q1 ~~ r1 #q1 and r1 covariance
      p2 ~~ u2v2*q2 #p2 and q2 covariance should also be constrained to be the same
      p2 ~~ u2s2*r2
      q2 ~~ v2s2*r2
      p3 ~~ u3v3*q3 #p3 and q3 covariance
      p3 ~~ u3s3*r3
      q3 ~~ v3s3*r3
      p4 ~~ u4v4*q4 #p4 and q4 covariance
      p4 ~~ u4s4*r4
      q4 ~~ v4s4*r4'


      riclpmUnconstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                       missing = 'ML',
                                       int.ov.free = F,
                                       int.lv.free = F,
                                       auto.fix.first = F,
                                       auto.fix.single = F,
                                       auto.cov.lv.x = F,
                                       auto.cov.y = F,
                                       auto.var = F)

      warning <- any(is.na(parameterEstimates(riclpmUnconstrainedsim))[c(13:30,43:93),])
      notposdef <- any(eigen(lavInspect(riclpmUnconstrainedsim, "cov.lv"))$values <= 1e-8)

      if(warning == TRUE) {
        countNaN <- countNaN+1
        WarningTeller <- WarningTeller + 1
      } else if(notposdef == TRUE) {
        countNotPosDef <- countNotPosDef+1
        WarningTeller <- WarningTeller + 1
      } else { # if no warning, then do rest of code
        print(paste0("Model does not contain NAs and cov mx of latents is pos def"))

        # subtract standardized estimates
        StdEst <- standardizedsolution(riclpmUnconstrainedsim, type = "std.nox",
                                       se = TRUE, zstat = TRUE, pvalue = TRUE,
                                       ci = TRUE, level = 0.95, cov.std = TRUE,
                                       remove.eq = TRUE, remove.ineq = TRUE,
                                       remove.def = FALSE, partable = NULL,
                                       GLIST = NULL, est = NULL)

        est1 <- StdEst[c(43:69),5]
        names(est1) <- c("alpha4", "beta4", "phi4", "alpha3", "beta3", "phi3", "alpha2", "beta2", "phi2", "delta4", "gamma4", "psi4", "delta3", "gamma3", "psi3", "delta2", "gamma2", "psi2", "tau4", "epsilon4", "chi4", "tau3", "epsilon3", "chi3", "tau2", "epsilon2", "chi2")
        vcov <- lavInspect(riclpmUnconstrainedsim, "vcov.std.nox")[c(19:45), c(19:45)]
        par.est1[simteller,] <- est1
        meanEst1 <- apply(par.est1, 2, mean, na.rm=TRUE)
        names(meanEst1) <- c("alpha4", "beta4", "phi4", "alpha3", "beta3", "phi3", "alpha2", "beta2", "phi2", "delta4", "gamma4", "psi4", "delta3", "gamma3", "psi3", "delta2", "gamma2", "psi2", "tau4", "epsilon4", "chi4", "tau3", "epsilon3", "chi3", "tau2", "epsilon2", "chi2")

        if (all(eigen(vcov)$values > 1e-10)) {

          eigen<-eigen(vcov)$values > 1e-10

          goricaResults1 <- goric(riclpmUnconstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

          # Store output

          GORICA.weights[simteller,] <- goricaResults1$result[,7]
          loglik.weights[simteller,] <- goricaResults1$result[,5]
          penalty.weights[simteller,] <- goricaResults1$result[,6]

          simteller<- simteller+1
        } else{nrposdef<- nrposdef+1}

        GORICA.power <- sum(GORICA.weights[,1] > GORICA.weights[,2])*(100/nsim)
        #ignore warnings
        on.exit(options(warn = oldw))
        round <- round + 1
      }
    }
    return(list(GORICA.power = GORICA.power, GORICA.weights = GORICA.weights, mean.estimates = meanEst1, loglik.weights = loglik.weights, penalty.weights = penalty.weights))
  }

  specific5measureTri <- function(nsim, p, B, H1,  M, Omega_pop) {

    print("The Trivariate RI-CLPM for wave-specific parameters model with 5 measurement occasions is generating")

    q <- 3   # number of variables

    varxy <- array(NA, c(p, q*M)) # create array with nrow=p and ncol=n to keep simulate data
    colnames(varxy)<-c("x1", "x2", "x3", "x4", "x5", "y1", "y2", "y3", "y4", "y5", "z1", "z2", "z3", "z4", "z5")
    RICLPMdata <- array(NA, c(p, q*M))
    colnames(RICLPMdata)<-c("x1", "x2", "x3", "x4", "x5", "y1", "y2", "y3", "y4", "y5", "z1", "z2", "z3", "z4", "z5")

    par.est1 <- array(NA, dim=c(nsim, 36))

    GORICA.weights <- array(NA, dim=c(nsim, 2)) # 2 >> we have H1 and complementH1 now
    colnames(GORICA.weights) <- c("H1","complementH1")

    loglik.weights <- array(NA, dim=c(nsim, 2))
    colnames(loglik.weights) <- c("H1","complementH1")

    penalty.weights <- array(NA, dim=c(nsim, 2))
    colnames(penalty.weights) <- c("H1","complementH1")

    nrposdef <- 0
    round <-1 # set this to count number of iteration
    WarningTeller <- 0
    countNaN <- 0
    countNotPosDef <-0
    tellerOK <- 0
    simteller<- 1

    while(simteller<=nsim){
      #for (simteller in 1:nsim) {

      #ignore warning
      oldw <- getOption("warn")
      options(warn = -1)

      print(paste0("Iteration", simteller))

      for(j in 1:p){
        var1 <- VAR.sim(B=B, n=N, include="none")
        varxy[j,]<-var1[(N-M+1):N,]
      }

      xy <- mvrnorm(n = p, mu = rep(0,3), Sigma = Omega_pop, empirical=FALSE)
      RICLPMdata[, 1:M] <- varxy[, 1:M] + xy[,1]
      RICLPMdata[, (M+1):(2*M)] <- varxy[, (M+1):(2*M)] + xy[,2]
      RICLPMdata[, (M+4):(3*M)] <- varxy[, (M+4):(3*M)] + xy[,3]


      # Fitting RI-CLPM
      riclpmModel<-
        '
      kappa =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
      omega =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
      zeta  =~ 1*z1 + 1*z2 + 1*z3 + 1*z4 + 1*z5

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      x5 ~ mu5*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1
      y5 ~ pi5*1
      z1 ~ lambda1*1
      z2 ~ lambda2*1
      z3 ~ lambda3*1
      z4 ~ lambda4*1
      z5 ~ lambda5*1

      # Random intercept parts
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      zeta  ~~ zeta  #variance
      kappa ~~ omega #covariance
      kappa ~~ zeta  #covariance
      omega ~~ zeta  #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      p5 =~ 1*x5
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4
      q5 =~ 1*y5
      r1 =~ 1*z1
      r2 =~ 1*z2
      r3 =~ 1*z3
      r4 =~ 1*z4
      r5 =~ 1*z5

      #Unconstrain autoregression and cross-lagged effects.
      p5 ~ alpha5*p4 + beta5*q4 + phi5*r4
      p4 ~ alpha4*p3 + beta4*q3 + phi4*r3
      p3 ~ alpha3*p2 + beta3*q2 + phi3*r2
      p2 ~ alpha2*p1 + beta2*q1 + phi2*r1

      q5 ~ delta5*q4 + gamma5*p4 + psi5*r4
      q4 ~ delta4*q3 + gamma4*p3 + psi4*r3
      q3 ~ delta3*q2 + gamma3*p2 + psi3*r2
      q2 ~ delta2*q1 + gamma2*p1 + psi2*r1

      r5 ~ tau5*r4 + epsilon5*p4 + chi5*q4
      r4 ~ tau4*r3 + epsilon4*p3 + chi4*q3
      r3 ~ tau3*r2 + epsilon3*p2 + chi3*q2
      r2 ~ tau2*r1 + epsilon2*p1 + chi2*q1

      p1 ~~ p1 #variance
      p2 ~~ u2*p2
      p3 ~~ u3*p3
      p4 ~~ u4*p4
      p5 ~~ u5*p5
      q1 ~~ q1 #variance
      q2 ~~ v2*q2
      q3 ~~ v3*q3
      q4 ~~ v4*q4
      q5 ~~ v5*q5
      r1 ~~ r1 #variance
      r2 ~~ s2*r2
      r3 ~~ s3*r3
      r4 ~~ s4*r4
      r5 ~~ s5*r5

      p1 ~~ q1 #p1 and q1 covariance
      p1 ~~ r1 #p1 and r1 covariance
      q1 ~~ r1 #q1 and r1 covariance
      p2 ~~ u2v2*q2 #p2 and q2 covariance should also be constrained to be the same
      p2 ~~ u2s2*r2
      q2 ~~ v2s2*r2
      p3 ~~ u3v3*q3 #p3 and q3 covariance
      p3 ~~ u3s3*r3
      q3 ~~ v3s3*r3
      p4 ~~ u4v4*q4 #p4 and q4 covariance
      p4 ~~ u4s4*r4
      q4 ~~ v4s4*r4
      p5 ~~ u5v5*q5 #p5 and q5 covariance
      p5 ~~ u5s5*r5
      q5 ~~ v5s5*r5'


      riclpmUnconstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                       missing = 'ML',
                                       int.ov.free = F,
                                       int.lv.free = F,
                                       auto.fix.first = F,
                                       auto.fix.single = F,
                                       auto.cov.lv.x = F,
                                       auto.cov.y = F,
                                       auto.var = F)

      warning <- any(is.na(parameterEstimates(riclpmUnconstrainedsim))[c(16:36,52:117),])
      notposdef <- any(eigen(lavInspect(riclpmUnconstrainedsim, "cov.lv"))$values <= 1e-8)

      if(warning == TRUE) {
        countNaN <- countNaN+1
        WarningTeller <- WarningTeller + 1
      } else if(notposdef == TRUE) {
        countNotPosDef <- countNotPosDef+1
        WarningTeller <- WarningTeller + 1
      } else { # if no warning, then do rest of code
        print(paste0("Model does not contain NAs and cov mx of latents is pos def"))

        # subtract standardized estimates
        StdEst <- standardizedsolution(riclpmUnconstrainedsim, type = "std.nox",
                                       se = TRUE, zstat = TRUE, pvalue = TRUE,
                                       ci = TRUE, level = 0.95, cov.std = TRUE,
                                       remove.eq = TRUE, remove.ineq = TRUE,
                                       remove.def = FALSE, partable = NULL,
                                       GLIST = NULL, est = NULL)

        est1 <- StdEst[c(52:87),5]
        names(est1) <- c("alpha5", "beta5", "phi5", "alpha4", "beta4", "phi4", "alpha3", "beta3", "phi3", "alpha2", "beta2", "phi2", "delta5", "gamma5", "psi5",  "delta4", "gamma4", "psi4", "delta3", "gamma3", "psi3", "delta2", "gamma2", "psi2", "tau5", "epsilon5", "chi5", "tau4", "epsilon4", "chi4", "tau3", "epsilon3", "chi3", "tau2", "epsilon2", "chi2")
        vcov<-lavInspect(riclpmUnconstrainedsim, "vcov.std.nox")[c(22:57), c(22:57)]
        par.est1[simteller,] <- est1
        meanEst1 <- apply(par.est1, 2, mean, na.rm=TRUE)
        names(meanEst1) <- c("alpha5", "beta5", "phi5", "alpha4", "beta4", "phi4", "alpha3", "beta3", "phi3", "alpha2", "beta2", "phi2", "delta5", "gamma5", "psi5",  "delta4", "gamma4", "psi4", "delta3", "gamma3", "psi3", "delta2", "gamma2", "psi2", "tau5", "epsilon5", "chi5", "tau4", "epsilon4", "chi4", "tau3", "epsilon3", "chi3", "tau2", "epsilon2", "chi2")

        if (all(eigen(vcov)$values > 1e-10)) {

          eigen<-eigen(vcov)$values > 1e-10

          goricaResults1 <- goric(riclpmUnconstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

          # Store output

          GORICA.weights[simteller,] <- goricaResults1$result[,7]
          loglik.weights[simteller,] <- goricaResults1$result[,5]
          penalty.weights[simteller,] <- goricaResults1$result[,6]

          simteller<- simteller+1
        } else{nrposdef<- nrposdef+1}

        GORICA.power <- sum(GORICA.weights[,1] > GORICA.weights[,2])*(100/nsim)
        #ignore warnings
        on.exit(options(warn = oldw))
        round <- round + 1
      }
    }
    return(list(GORICA.power = GORICA.power, GORICA.weights = GORICA.weights, mean.estimates = meanEst1, loglik.weights = loglik.weights, penalty.weights = penalty.weights))
  }

  specific6measureTri <- function(nsim, p, B, H1,  M, Omega_pop) {

    print("The Trivariate RI-CLPM for wave-specific parameters model with 6 measurement occasions is generating")

    q <- 3   # number of variables
    varxy <- array(NA, c(p, q*M)) # create array with nrow=p and ncol=n to keep simulate data
    colnames(varxy)<-c("x1", "x2", "x3", "x4", "x5", "x6", "y1", "y2", "y3", "y4", "y5", "y6", "z1", "z2", "z3", "z4", "z5", "z6")
    RICLPMdata <- array(NA, c(p, q*M))
    colnames(RICLPMdata)<-c("x1", "x2", "x3", "x4", "x5", "x6", "y1", "y2", "y3", "y4", "y5", "y6", "z1", "z2", "z3", "z4", "z5", "z6")

    par.est1 <- array(NA, dim=c(nsim, 45))

    GORICA.weights <- array(NA, dim=c(nsim, 2)) # 2 >> we have H1 and complementH1 now
    colnames(GORICA.weights) <- c("H1","complementH1")

    loglik.weights <- array(NA, dim=c(nsim, 2))
    colnames(loglik.weights) <- c("H1","complementH1")

    penalty.weights <- array(NA, dim=c(nsim, 2))
    colnames(penalty.weights) <- c("H1","complementH1")

    nrposdef <- 0
    round <-1 # set this to count number of iteration
    WarningTeller <- 0
    countNaN <- 0
    countNotPosDef <-0
    tellerOK <- 0
    simteller<- 1

    while(simteller <= nsim){
      #for (simteller in 1:nsim) {

      #ignore warning
      oldw <- getOption("warn")
      options(warn = -1)

      print(paste0("Iteration", simteller))

      for(j in 1:p){
        var1 <- VAR.sim(B=B, n=N, include="none")
        varxy[j,]<-var1[(N-M+1):N,]
      }

      xy <- mvrnorm(n = p, mu = rep(0,3), Sigma = Omega_pop, empirical=FALSE)
      RICLPMdata[, 1:M] <- varxy[, 1:M] + xy[,1]
      RICLPMdata[, (M+1):(2*M)] <- varxy[, (M+1):(2*M)] + xy[,2]
      RICLPMdata[, (M+4):(3*M)] <- varxy[, (M+4):(3*M)] + xy[,3]


      # Fitting RI-CLPM
      riclpmModel<-
        '
      kappa =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5 + 1*x6
      omega =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5 + 1*y6
      zeta  =~ 1*z1 + 1*z2 + 1*z3 + 1*z4 + 1*z5 + 1*z6

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      x5 ~ mu5*1
      x6 ~ mu6*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1
      y5 ~ pi5*1
      y6 ~ pi6*1
      z1 ~ lambda1*1
      z2 ~ lambda2*1
      z3 ~ lambda3*1
      z4 ~ lambda4*1
      z5 ~ lambda5*1
      z6 ~ lambda6*1

      # Random intercept parts
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      zeta  ~~ zeta  #variance
      kappa ~~ omega #covariance
      kappa ~~ zeta  #covariance
      omega ~~ zeta  #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      p5 =~ 1*x5
      p6 =~ 1*x6
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4
      q5 =~ 1*y5
      q6 =~ 1*y6
      r1 =~ 1*z1
      r2 =~ 1*z2
      r3 =~ 1*z3
      r4 =~ 1*z4
      r5 =~ 1*z5
      r6 =~ 1*z6

      #Unconstrain autoregression and cross-lagged effects.
      p6 ~ alpha6*p5 + beta6*q5 + phi6*r5
      p5 ~ alpha5*p4 + beta5*q4 + phi5*r4
      p4 ~ alpha4*p3 + beta4*q3 + phi4*r3
      p3 ~ alpha3*p2 + beta3*q2 + phi3*r2
      p2 ~ alpha2*p1 + beta2*q1 + phi2*r1

      q6 ~ delta6*q5 + gamma6*p5 + psi6*r5
      q5 ~ delta5*q4 + gamma5*p4 + psi5*r4
      q4 ~ delta4*q3 + gamma4*p3 + psi4*r3
      q3 ~ delta3*q2 + gamma3*p2 + psi3*r2
      q2 ~ delta2*q1 + gamma2*p1 + psi2*r1

      r6 ~ tau6*r5 + epsilon6*p5 + chi6*q5
      r5 ~ tau5*r4 + epsilon5*p4 + chi5*q4
      r4 ~ tau4*r3 + epsilon4*p3 + chi4*q3
      r3 ~ tau3*r2 + epsilon3*p2 + chi3*q2
      r2 ~ tau2*r1 + epsilon2*p1 + chi2*q1

      p1 ~~ p1 #variance
      p2 ~~ u2*p2
      p3 ~~ u3*p3
      p4 ~~ u4*p4
      p5 ~~ u5*p5
      p6 ~~ u6*p6
      q1 ~~ q1 #variance
      q2 ~~ v2*q2
      q3 ~~ v3*q3
      q4 ~~ v4*q4
      q5 ~~ v5*q5
      q6 ~~ v6*q6
      r1 ~~ r1 #variance
      r2 ~~ s2*r2
      r3 ~~ s3*r3
      r4 ~~ s4*r4
      r5 ~~ s5*r5
      r6 ~~ s6*r6

      p1 ~~ q1 #p1 and q1 covariance
      p1 ~~ r1 #p1 and r1 covariance
      q1 ~~ r1 #q1 and r1 covariance
      p2 ~~ u2v2*q2 #p2 and q2 covariance should also be constrained to be the same
      p2 ~~ u2s2*r2
      q2 ~~ v2s2*r2
      p3 ~~ u3v3*q3 #p3 and q3 covariance
      p3 ~~ u3s3*r3
      q3 ~~ v3s3*r3
      p4 ~~ u4v4*q4 #p4 and q4 covariance
      p4 ~~ u4s4*r4
      q4 ~~ v4s4*r4
      p5 ~~ u5v5*q5 #p5 and q5 covariance
      p5 ~~ u5s5*r5
      q5 ~~ v5s5*r5
      p6 ~~ u6v6*q6 #p6 and q6 covariance
      p6 ~~ u6s6*r6
      q6 ~~ v6s6*r6'


      riclpmUnconstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                       missing = 'ML',
                                       int.ov.free = F,
                                       int.lv.free = F,
                                       auto.fix.first = F,
                                       auto.fix.single = F,
                                       auto.cov.lv.x = F,
                                       auto.cov.y = F,
                                       auto.var = F)

      warning <- any(is.na(parameterEstimates(riclpmUnconstrainedsim))[c(19:42,61:141),])
      notposdef <- any(eigen(lavInspect(riclpmUnconstrainedsim, "cov.lv"))$values <= 1e-8)

      if(warning == TRUE) {
        countNaN <- countNaN+1
        WarningTeller <- WarningTeller + 1
      } else if(notposdef == TRUE) {
        countNotPosDef <- countNotPosDef+1
        WarningTeller <- WarningTeller + 1
      } else { # if no warning, then do rest of code
        print(paste0("Model does not contain NAs and cov mx of latents is pos def"))

        # subtract standardized estimates
        StdEst <- standardizedsolution(riclpmUnconstrainedsim, type = "std.nox",
                                       se = TRUE, zstat = TRUE, pvalue = TRUE,
                                       ci = TRUE, level = 0.95, cov.std = TRUE,
                                       remove.eq = TRUE, remove.ineq = TRUE,
                                       remove.def = FALSE, partable = NULL,
                                       GLIST = NULL, est = NULL)

        est1 <- StdEst[c(61:105),5]
        names(est1) <- c("alpha6", "beta6", "phi6", "alpha5", "beta5", "phi5", "alpha4", "beta4", "phi4", "alpha3", "beta3", "phi3", "alpha2", "beta2", "phi2",  "delta6", "gamma6", "psi6", "delta5", "gamma5", "psi5", "delta4", "gamma4", "psi4", "delta3", "gamma3", "psi3", "delta2", "gamma2", "psi2", "tau6", "epsilon6", "chi6", "tau5", "epsilon5", "chi5", "tau4", "epsilon4", "chi4", "tau3", "epsilon3", "chi3", "tau2", "epsilon2", "chi2")
        vcov <- lavInspect(riclpmUnconstrainedsim, "vcov.std.nox")[c(25:69), c(25:69)]
        par.est1[simteller,] <- est1
        meanEst1 <- apply(par.est1, 2, mean, na.rm=TRUE)
        names(meanEst1) <- c("alpha6", "beta6", "phi6", "alpha5", "beta5", "phi5", "alpha4", "beta4", "phi4", "alpha3", "beta3", "phi3", "alpha2", "beta2", "phi2",  "delta6", "gamma6", "psi6", "delta5", "gamma5", "psi5", "delta4", "gamma4", "psi4", "delta3", "gamma3", "psi3", "delta2", "gamma2", "psi2", "tau6", "epsilon6", "chi6", "tau5", "epsilon5", "chi5", "tau4", "epsilon4", "chi4", "tau3", "epsilon3", "chi3", "tau2", "epsilon2", "chi2")

        if (all(eigen(vcov)$values > 1e-10)) {

          eigen<-eigen(vcov)$values > 1e-10
          goricaResults1 <- goric(riclpmUnconstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")
          # Store output

          GORICA.weights[simteller,] <- goricaResults1$result[,7]
          loglik.weights[simteller,] <- goricaResults1$result[,5]
          penalty.weights[simteller,] <- goricaResults1$result[,6]

          simteller<- simteller+1
        } else{nrposdef<- nrposdef+1}

        GORICA.power <- sum(GORICA.weights[,1] > GORICA.weights[,2])*(100/nsim)
        #ignore warnings
        on.exit(options(warn = oldw))
        round <- round + 1
      }
    }
    return(list(GORICA.power = GORICA.power, GORICA.weights = GORICA.weights, mean.estimates = meanEst1, loglik.weights = loglik.weights, penalty.weights = penalty.weights))
  }

  ##############################################################################################################################################################################################################################

  if (n == 2) {           # n = 2, bivariate model
    if (model == 1) {   # model = 1, wave-independent parameters model
      if (M == 3) {
        result <- independ3measureBi(nsim, p, B, H1,  M, Omega_pop)
      } else if (M == 4) {
        result <- independ4measureBi(nsim, p, B, H1,  M, Omega_pop)
      } else if (M == 5) {
        result <- independ5measureBi(nsim, p, B, H1,  M, Omega_pop)
      } else if (M == 6) {
        result <- independ6measureBi(nsim, p, B, H1,  M, Omega_pop)
      } else {
        print("The number of measurement occasions should range from 3 to 6")
      }
    } else if (model == 2) {   # model = 2, wave-specific parameters model
      if (M == 3) {
        result <- specific3measureBi(nsim, p, B, H1,  M, Omega_pop)
      } else if (M == 4) {
        result <- specific4measureBi(nsim, p, B, H1,  M, Omega_pop)
      } else if (M == 5) {
        result <- specific5measureBi(nsim, p, B, H1,  M, Omega_pop)
      } else if (M == 6) {
        result <- specific6measureBi(nsim, p, B, H1,  M, Omega_pop)
      } else {
        print("The number of measurement occasions should range from 3 to 6")
      }
    } else {

      print("Invalid model, model == 1: wave-independent parameters model and model == 2: wave-specific parameters model")
    }

  } else if (n == 3) {    # n = 3, trivariate model
    if (model == 1) {   # model = 1, wave-independent parameters model
      if (M == 3) {
        result <- independ3measureTri(nsim, p, B, H1,  M, Omega_pop)
      } else if (M == 4) {
        result <- independ4measureTri(nsim, p, B, H1,  M, Omega_pop)
      } else if (M == 5) {
        result <- independ5measureTri(nsim, p, B, H1,  M, Omega_pop)
      } else if (M == 6) {
        result <- independ6measureTri(nsim, p, B, H1,  M, Omega_pop)
      } else {
        print("The number of measurement occasions should range from 3 to 6")
      }
    } else if (model == 2) {   # model = 2, wave-specific parameters model
      if (M == 3) {
        result <- specific3measureTri(nsim, p, B, H1,  M, Omega_pop)
      } else if (M == 4) {
        result <- specific4measureTri(nsim, p, B, H1,  M, Omega_pop)
      } else if (M == 5) {
        result <- specific5measureTri(nsim, p, B, H1,  M, Omega_pop)
      } else if (M == 6) {
        result <- specific6measureTri(nsim, p, B, H1,  M, Omega_pop)
      } else {
        print("The number of measurement occasions should range from 3 to 6")
      }
    } else {

      print("Invalid model, model == 1: wave-independent parameters model and model == 2: wave-specific parameters model")
    }

  } else {

    print("The number of variables should be 2 or 3, two variables for a bivariate model and three variables for a trivariate model")

  }

  return(result)

} #end of GORICA.power

#' GORICA.test
#'
#' Evaluate hypothesis using the GORICA
#' @param n The number of variables which is represented bi- (n = 2) or tri-variate (n = 3) model
#' @param model Determine wave-independent (model = 1) or wave-specific parameters model (model = 2)
#' @param M The number of measurement occasions (The number of measurement occasions should range from 3 to 6)
#' @param data The dataset
#' @param H1 The hypothesis of interest
#' @return The GORICA results, the estimates and the variances - covariances matrix of the estimates
#' @examples
#' require(Rfssa) # for load_github_data()
#'
#' data <- load_github_data("https://github.com/Chuenjai/GORICApower3/blob/master/data3v4w.RData")
#'
#' RICLPMdata <- as.data.frame(RICLPMdata)  # make sure that the dataset is in the form of data.frame
#'
#' H1 <- "abs(beta4) < abs(gamma4); abs(phi4) < abs(epsilon4); abs(psi4) < abs(chi4); abs(beta3) < abs(gamma3); abs(phi3) < abs(epsilon3); abs(psi3) < abs(chi3); abs(beta2) < abs(gamma2); abs(phi2) < abs(epsilon2); abs(psi2) < abs(chi2)"
#'
#' GORICA.test(n = 3, model = 2, M = 4, data, H1)
#' @import lavaan
#' @import restriktor
#' @export

GORICA.test <- function(n, model, M, data, H1) {

  result <- NULL

  ############# Wave-independent parameters model in a bivariate model ################################################

  independ3measureBi <- function(data, H1) {

    print("The bivariate RI-CLPM for wave-independent parameters model with 3 measurement occasions is generating")

    RICLPMdata <- scale(RICLPMdata) # standardization

    riclpmModel<-
      '
      kappa =~ 1*x1 + 1*x2 + 1*x3
      omega =~ 1*y1 + 1*y2 + 1*y3

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1

      # Random intercept parts
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      kappa ~~ omega #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3

      #constrain autoregression and cross-lagged effects to be the same across both lags.
      p3 ~ alpha*p2 + beta*q2
      p2 ~ alpha*p1 + beta*q1

      q3 ~ delta*q2 + gamma*p2
      q2 ~ delta*q1 + gamma*p1

      p1 ~~ p1 #variance
      p2 ~~ u*p2
      p3 ~~ u*p3
      q1 ~~ q1 #variance
      q2 ~~ v*q2
      q3 ~~ v*q3

      p1 ~~ q1 #p1 and q1 covariance
      p2 ~~ uv*q2 #p2 and q2 covariance should also be constrained to be the same
      p3 ~~ uv*q3 #p2 and q2 covariance'

    riclpmConstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                   missing = 'ML',
                                   int.ov.free = F,
                                   int.lv.free = F,
                                   auto.fix.first = F,
                                   auto.fix.single = F,
                                   auto.cov.lv.x = F,
                                   auto.cov.y = F,
                                   auto.var = F)
    #summary(riclpmConstrainedsim, standardized = T)

    # subtract parameter estimates
    est1<-coef(riclpmConstrainedsim)[c(10,11,14,15)]   ##
    names(est1) <- c("alpha","beta","delta", "gamma")
    vcov<-lavInspect(riclpmConstrainedsim, "vcov")[c(10,14), c(10,14)]  ###

    #goricaResults1 <- goric(riclpmConstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

    goricaResults1 <- goric(riclpmConstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

    return(list(GORICA = goricaResults1, estimates = est1, vcov = vcov))
  }

  independ4measureBi <- function(data, H1) {

    print("The bivariate RI-CLPM for wave-independent parameters model with 4 measurement occasions is generating")

    RICLPMdata <- scale(RICLPMdata) #standardizing

    # Fitting RI-CLPM
    riclpmModel<-
      '
      kappa =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
      omega =~ 1*y1 + 1*y2 + 1*y3 + 1*y4

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1


      # Random intercept part
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      kappa ~~ omega #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4

      #constrain autoregression and cross-lagged effects to be the same across both lags.

      p4 ~ alpha*p3 + beta*q3
      p3 ~ alpha*p2 + beta*q2
      p2 ~ alpha*p1 + beta*q1


      q4 ~ delta*q3 + gamma*p3
      q3 ~ delta*q2 + gamma*p2
      q2 ~ delta*q1 + gamma*p1


      p1 ~~ p1 #variance
      p2 ~~ u*p2
      p3 ~~ u*p3
      p4 ~~ u*p4

      q1 ~~ q1 #variance
      q2 ~~ v*q2
      q3 ~~ v*q3
      q4 ~~ v*q4


      p1 ~~ q1 #p1 and q1 covariance
      p2 ~~ uv*q2 #p2 and q2 covariance should also be constrained to be the same
      p3 ~~ uv*q3 #p2 and q2 covariance
      p4 ~~ uv*q4'


    riclpmConstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                   missing = 'ML',
                                   int.ov.free = F,
                                   int.lv.free = F,
                                   auto.fix.first = F,
                                   auto.fix.single = F,
                                   auto.cov.lv.x = F,
                                   auto.cov.y = F,
                                   auto.var = F)

    # subtract for alpha, beta, delta, gamma
    est1<-coef(riclpmConstrainedsim)[c(12,13, 18, 19)]
    names(est1) <- c("alpha","beta", "delta", "gamma")
    vcov<-lavInspect(riclpmConstrainedsim, "vcov")[c(12,13, 18, 19), c(12,13, 18, 19)]

    goricaResults1 <- goric(riclpmConstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

    return(list(GORICA = goricaResults1, estimates = est1, vcov = vcov))
  }

  independ5measureBi <- function(data, H1) {

    print("The bivariate RI-CLPM for wave-independent parameters model with 5 measurement occasions is generating")

    RICLPMdata <- scale(RICLPMdata) #standardizing

    # Fitting RI-CLPM
    riclpmModel<-
      '
      kappa =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
      omega =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      x5 ~ mu5*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1
      y5 ~ pi5*1


      # Random intercept part
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      kappa ~~ omega #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      p5 =~ 1*x5
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4
      q5 =~ 1*y5

    #constrain autoregression and cross-lagged effects to be the same across both lags.

      p5 ~ alpha*p4 + beta*q4
      p4 ~ alpha*p3 + beta*q3
      p3 ~ alpha*p2 + beta*q2
      p2 ~ alpha*p1 + beta*q1


      q5 ~ delta*q4 + gamma*p4
      q4 ~ delta*q3 + gamma*p3
      q3 ~ delta*q2 + gamma*p2
      q2 ~ delta*q1 + gamma*p1


      p1 ~~ p1 #variance
      p2 ~~ u*p2
      p3 ~~ u*p3
      p4 ~~ u*p4
      p5 ~~ u*p5

      q1 ~~ q1 #variance
      q2 ~~ v*q2
      q3 ~~ v*q3
      q4 ~~ v*q4
      q5 ~~ v*q5


      p1 ~~ q1 #p1 and q1 covariance
      p2 ~~ uv*q2 #p2 and q2 covariance should also be constrained to be the same
      p3 ~~ uv*q3 #p2 and q2 covariance
      p4 ~~ uv*q4
      p5 ~~ uv*q5'


    riclpmConstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                   missing = 'ML',
                                   int.ov.free = F,
                                   int.lv.free = F,
                                   auto.fix.first = F,
                                   auto.fix.single = F,
                                   auto.cov.lv.x = F,
                                   auto.cov.y = F,
                                   auto.var = F)

    # subtract for alpha, beta, delta, gamma
    est1<-coef(riclpmConstrainedsim)[c(14, 15, 22, 23)]
    names(est1) <- c("alpha", "beta", "delta", "gamma")
    vcov<-lavInspect(riclpmConstrainedsim, "vcov")[c(14, 15, 22, 23), c(14, 15, 22, 23)]

    goricaResults1 <- goric(riclpmConstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

    return(list(GORICA = goricaResults1, estimates = est1, vcov = vcov))
  }

  independ6measureBi <- function(data, H1) {

    print("The bivariate RI-CLPM for wave-independent parameters model with 6 measurement occasions is generating")

    RICLPMdata <- scale(RICLPMdata) #standardizing

    # Fitting RI-CLPM
    riclpmModel<-
      '
      kappa =~ 1*x1 + 1*x2 + 1*x3+1*x4 + 1*x5 + 1*x6
      omega =~ 1*y1 + 1*y2 + 1*y3+1*y4 + 1*y5 + 1*y6

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      x5 ~ mu5*1
      x6 ~ mu6*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1
      y5 ~ pi5*1
      y6 ~ pi6*1

      # Random intercept part
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      kappa ~~ omega #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      p5 =~ 1*x5
      p6 =~ 1*x6
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4
      q5 =~ 1*y5
      q6 =~ 1*y6

      #constrain autoregression and cross-lagged effects to be the same across both lags.
      p6 ~ alpha*p5 + beta*q5
      p5 ~ alpha*p4 + beta*q4
      p4 ~ alpha*p3 + beta*q3
      p3 ~ alpha*p2 + beta*q2
      p2 ~ alpha*p1 + beta*q1

      q6 ~ delta*q5 + gamma*p5
      q5 ~ delta*q4 + gamma*p4
      q4 ~ delta*q3 + gamma*p3
      q3 ~ delta*q2 + gamma*p2
      q2 ~ delta*q1 + gamma*p1


      p1 ~~ p1 #variance
      p2 ~~ u*p2
      p3 ~~ u*p3
      p4 ~~ u*p4
      p5 ~~ u*p5
      p6 ~~ u*p6
      q1 ~~ q1 #variance
      q2 ~~ v*q2
      q3 ~~ v*q3
      q4 ~~ v*q4
      q5 ~~ v*q5
      q6 ~~ v*q6

      p1 ~~ q1 #p1 and q1 covariance
      p2 ~~ uv*q2 #p2 and q2 covariance should also be constrained to be the same
      p3 ~~ uv*q3 #p2 and q2 covariance
      p4 ~~ uv*q4
      p5 ~~ uv*q5
      p6 ~~ uv*q6'

    riclpmConstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                   missing = 'ML',
                                   int.ov.free = F,
                                   int.lv.free = F,
                                   auto.fix.first = F,
                                   auto.fix.single = F,
                                   auto.cov.lv.x = F,
                                   auto.cov.y = F,
                                   auto.var = F)

    # subtract for alpha, beta, delta, gamma
    est1<-coef(riclpmConstrainedsim)[c(16, 17, 26, 27)]
    names(est1) <- c("alpha", "beta", "delta", "gamma")
    vcov<-lavInspect(riclpmConstrainedsim, "vcov")[c(16, 17, 26, 27), c(16, 17, 26, 27)]

    goricaResults1 <- goric(riclpmConstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

    return(list(GORICA = goricaResults1, estimates = est1, vcov = vcov))
  }

  ################# Wave-specific parameters model in a bivariate model ################################################

  specific3measureBi <- function(data, H1) {

    print("The bivariate RI-CLPM for wave-specific parameters model with 3 measurement occasions is generating")

    riclpmModel2 <-
      '

      #Intercepts
      kappa =~ 1*x1 + 1*x2 + 1*x3
      omega =~ 1*y1 + 1*y2 + 1*y3

      x1 ~ mu1*1
      x2 ~ mu2*1
      x3 ~ mu3*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1

      # Variances and covariance of the random intercepts
      kappa  ~~ kappa 	# variance
      omega  ~~ omega 	# variance
      kappa  ~~ omega 	# covariance

      # Latent variables for autoregressive and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3

      # Unconstrained autoregressive and cross-lagged effects
      # Specify the name of the standardized estimates
      p3 ~ alpha3*p2 + beta3*q2
      p2 ~ alpha2*p1 + beta2*q1
      q3 ~ delta3*q2 + gamma3*p2
      q2 ~ delta2*q1 + gamma2*p1
      p1 ~~ p1
      p2 ~~ u2*p2
      p3 ~~ u3*p3
      q1 ~~ q1
      q2 ~~ v2*q2
      q3 ~~ v3*q3
      p1 ~~ q1
      p2 ~~ u2v2*q2
      p3 ~~ u3v3*q3 '

    riclpmUnconstrained <-  lavaan(riclpmModel2, data = RICLPMdata,
                                   missing = 'ML',
                                   int.ov.free = F,
                                   int.lv.free = F,
                                   auto.fix.first = F,
                                   auto.fix.single = F,
                                   auto.cov.lv.x = F,
                                   auto.cov.y = F,
                                   auto.var = F)
    summary(riclpmUnconstrained, standardized = T)

    # subtract standardized parameter estimates
    StdEst <- standardizedsolution(riclpmUnconstrained, type = "std.nox",
                                   se = TRUE, zstat = TRUE, pvalue = TRUE,
                                   ci = TRUE, level = 0.95, cov.std = TRUE,
                                   remove.eq = TRUE, remove.ineq = TRUE,
                                   remove.def = FALSE, partable = NULL,
                                   GLIST = NULL, est = NULL)

    est1 <- StdEst[c(22:29),5]
    names(est1) <- c("alpha3","beta3", "alpha2","beta2", "delta3", "gamma3", "delta2", "gamma2")
    vcov <- lavInspect(riclpmUnconstrained, "vcov.std.nox")[c(10:17), c(10:17)]

    goricaResults1 <- goric(riclpmUnconstrained, hypotheses = list(H1), comparison = "complement", type = "gorica")

    return(list(GORICA = goricaResults1, estimates = est1, vcov = vcov))
  }

  specific4measureBi <- function(data, H1) {

    print("The bivariate RI-CLPM for wave-specific parameters model with 4 measurement occasions is generating")

    # Fitting RI-CLPM
    riclpmModel2 <-
      '
      kappa =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
      omega =~ 1*y1 + 1*y2 + 1*y3 + 1*y4

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1


      # Random intercept part
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      kappa ~~ omega #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4

      #Unconstrain autoregression and cross-lagged effects to be the same across both lags.

      p4 ~ alpha4*p3 + beta4*q3
      p3 ~ alpha3*p2 + beta3*q2
      p2 ~ alpha2*p1 + beta2*q1


      q4 ~ delta4*q3 + gamma4*p3
      q3 ~ delta3*q2 + gamma3*p2
      q2 ~ delta2*q1 + gamma2*p1


      p1 ~~ p1 #variance
      p2 ~~ u2*p2
      p3 ~~ u3*p3
      p4 ~~ u4*p4

      q1 ~~ q1 #variance
      q2 ~~ v2*q2
      q3 ~~ v3*q3
      q4 ~~ v4*q4


      p1 ~~ q1 #p1 and q1 covariance
      p2 ~~ u2v2*q2 #p2 and q2 covariance should also be constrained to be the same
      p3 ~~ u3v3*q3 #p3 and q3 covariance
      p4 ~~ u4v4*q4'

    riclpmUnconstrainedsim <- lavaan(riclpmModel2, data = RICLPMdata,
                                     missing = 'ML',
                                     int.ov.free = F,
                                     int.lv.free = F,
                                     auto.fix.first = F,
                                     auto.fix.single = F,
                                     auto.cov.lv.x = F,
                                     auto.cov.y = F,
                                     auto.var = F)

    # subtract for alpha, beta, delta, gamma (standardized estimates)
    StdEst <- standardizedsolution(riclpmUnconstrainedsim, type = "std.nox",
                                   se = TRUE, zstat = TRUE, pvalue = TRUE,
                                   ci = TRUE, level = 0.95, cov.std = TRUE,
                                   remove.eq = TRUE, remove.ineq = TRUE,
                                   remove.def = FALSE, partable = NULL,
                                   GLIST = NULL, est = NULL)

    est1 <- StdEst[c(28:39),5]
    names(est1) <- c("alpha4", "beta4", "alpha3", "beta3", "alpha2", "beta2",  "delta4", "gamma4", "delta3", "gamma3", "delta2", "gamma2")
    vcov<-lavInspect(riclpmUnconstrainedsim, "vcov.std.nox")[c(12:23), c(12:23)]

    goricaResults1 <- goric(riclpmUnconstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

    return(list(GORICA = goricaResults1, estimates = est1, vcov = vcov))
  }

  specific5measureBi <- function(data, H1) {

    print("The bivariate RI-CLPM for wave-specific parameters model with 5 measurement occasions is generating")

    # Fitting RI-CLPM
    riclpmModel2 <-
      '
      kappa =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
      omega =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      x5 ~ mu5*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1
      y5 ~ pi5*1


      # Random intercept part
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      kappa ~~ omega #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      p5 =~ 1*x5
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4
      q5 =~ 1*y5

      #Unconstrain autoregression and cross-lagged effects to be the same across both lags.

      p5 ~ alpha5*p4 + beta5*q4
      p4 ~ alpha4*p3 + beta4*q3
      p3 ~ alpha3*p2 + beta3*q2
      p2 ~ alpha2*p1 + beta2*q1


      q5 ~ delta5*q4 + gamma5*p4
      q4 ~ delta4*q3 + gamma4*p3
      q3 ~ delta3*q2 + gamma3*p2
      q2 ~ delta2*q1 + gamma2*p1


      p1 ~~ p1 #variance
      p2 ~~ u2*p2
      p3 ~~ u3*p3
      p4 ~~ u4*p4
      p5 ~~ u5*p5

      q1 ~~ q1 #variance
      q2 ~~ v2*q2
      q3 ~~ v3*q3
      q4 ~~ v4*q4
      q5 ~~ v5*q5


      p1 ~~ q1 #p1 and q1 covariance
      p2 ~~ u2v2*q2 #p2 and q2 covariance should also be constrained to be the same
      p3 ~~ u3v3*q3 #p2 and q2 covariance
      p4 ~~ u4v4*q4
      p5 ~~ u5v5*q5'


    riclpmUnconstrainedsim <- lavaan(riclpmModel2, data = RICLPMdata,
                                     missing = 'ML',
                                     int.ov.free = F,
                                     int.lv.free = F,
                                     auto.fix.first = F,
                                     auto.fix.single = F,
                                     auto.cov.lv.x = F,
                                     auto.cov.y = F,
                                     auto.var = F)

    # subtract for alpha, beta, delta, gamma (standardized estimates)
    StdEst <- standardizedsolution(riclpmUnconstrainedsim, type = "std.nox",
                                   se = TRUE, zstat = TRUE, pvalue = TRUE,
                                   ci = TRUE, level = 0.95, cov.std = TRUE,
                                   remove.eq = TRUE, remove.ineq = TRUE,
                                   remove.def = FALSE, partable = NULL,
                                   GLIST = NULL, est = NULL)

    est1 <- StdEst[c(34:49),5]
    names(est1) <- c("alpha5", "beta5", "alpha4", "beta4", "alpha3", "beta3", "alpha2", "beta2",  "delta5", "gamma5",  "delta4", "gamma4", "delta3", "gamma3", "delta2", "gamma2")
    vcov <- lavInspect(riclpmUnconstrainedsim, "vcov.std.nox")[c(14:29), c(14:29)]

    goricaResults1 <- goric(riclpmUnconstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

    return(list(GORICA = goricaResults1, estimates = est1, vcov = vcov))
  }

  specific6measureBi <- function(data, H1) {

    print("The bivariate RI-CLPM for wave-specific parameters model with 6 measurement occasions is generating")

    # Fitting RI-CLPM
    riclpmModel2 <-
      '
      kappa =~ 1*x1 + 1*x2 + 1*x3+1*x4 + 1*x5 + 1*x6
      omega =~ 1*y1 + 1*y2 + 1*y3+1*y4 + 1*y5 + 1*y6

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      x5 ~ mu5*1
      x6 ~ mu6*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1
      y5 ~ pi5*1
      y6 ~ pi6*1

      # Random intercept part
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      kappa ~~ omega #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      p5 =~ 1*x5
      p6 =~ 1*x6
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4
      q5 =~ 1*y5
      q6 =~ 1*y6

      #Unconstrain autoregression and cross-lagged effects to be the same across both lags.
      p6 ~ alpha6*p5 + beta6*q5
      p5 ~ alpha5*p4 + beta5*q4
      p4 ~ alpha4*p3 + beta4*q3
      p3 ~ alpha3*p2 + beta3*q2
      p2 ~ alpha2*p1 + beta2*q1

      q6 ~ delta6*q5 + gamma6*p5
      q5 ~ delta5*q4 + gamma5*p4
      q4 ~ delta4*q3 + gamma4*p3
      q3 ~ delta3*q2 + gamma3*p2
      q2 ~ delta2*q1 + gamma2*p1


      p1 ~~ p1 #variance
      p2 ~~ u2*p2
      p3 ~~ u3*p3
      p4 ~~ u4*p4
      p5 ~~ u5*p5
      p6 ~~ u6*p6
      q1 ~~ q1 #variance
      q2 ~~ v2*q2
      q3 ~~ v3*q3
      q4 ~~ v4*q4
      q5 ~~ v5*q5
      q6 ~~ v6*q6

      p1 ~~ q1 #p1 and q1 covariance
      p2 ~~ u2v2*q2 #p2 and q2 covariance should also be constrained to be the same
      p3 ~~ u3v3*q3 #p2 and q2 covariance
      p4 ~~ u4v4*q4
      p5 ~~ u5v5*q5
      p6 ~~ u6v6*q6'

    riclpmUnconstrainedsim <- lavaan(riclpmModel2, data = RICLPMdata,
                                     missing = 'ML',
                                     int.ov.free = F,
                                     int.lv.free = F,
                                     auto.fix.first = F,
                                     auto.fix.single = F,
                                     auto.cov.lv.x = F,
                                     auto.cov.y = F,
                                     auto.var = F)

    # subtract for alpha, beta, delta, gamma (standardized estimates)
    StdEst <- standardizedsolution(riclpmUnconstrainedsim, type = "std.nox",
                                   se = TRUE, zstat = TRUE, pvalue = TRUE,
                                   ci = TRUE, level = 0.95, cov.std = TRUE,
                                   remove.eq = TRUE, remove.ineq = TRUE,
                                   remove.def = FALSE, partable = NULL,
                                   GLIST = NULL, est = NULL)

    est1 <- StdEst[c(40:59),5]
    names(est1) <- c("alpha6", "beta6", "alpha5", "beta5", "alpha4", "beta4", "alpha3", "beta3", "alpha2", "beta2",  "delta6", "gamma6",  "delta5", "gamma5",  "delta4", "gamma4", "delta3", "gamma3", "delta2", "gamma2")
    vcov<-lavInspect(riclpmUnconstrainedsim, "vcov.std.nox")[c(16:35), c(16:35)]

    goricaResults1 <- goric(riclpmUnconstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

    return(list(GORICA = goricaResults1, estimates = est1, vcov = vcov))
  }


  ############# Wave-independent parameters model in a tri-variate model ################################################

  independ3measureTri <- function(data, H1) {

    print("The Trivariate RI-CLPM for wave-independent parameters model with 3 measurement occasions is generating")

    RICLPMdata <- scale(RICLPMdata) #standardizing

    # Fitting RI-CLPM
    riclpmModel<-
      '
      kappa =~ 1*x1 + 1*x2 + 1*x3
      omega =~ 1*y1 + 1*y2 + 1*y3
      zeta  =~ 1*z1 + 1*z2 + 1*z3

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      z1 ~ lambda1*1
      z2 ~ lambda2*1
      z3 ~ lambda3*1

      # Random intercept parts
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      zeta  ~~ zeta  #variance
      kappa ~~ omega #covariance
      kappa ~~ zeta  #covariance
      omega ~~ zeta  #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      r1 =~ 1*z1
      r2 =~ 1*z2
      r3 =~ 1*z3

      #constrain autoregression and cross-lagged effects to be the same across both lags.
      p3 ~ alpha*p2 + beta*q2 + phi*r2
      p2 ~ alpha*p1 + beta*q1 + phi*r1

      q3 ~ delta*q2 + gamma*p2 + psi*r2
      q2 ~ delta*q1 + gamma*p1 + psi*r1

      r3 ~ tau*r2 + epsilon*p2 + chi*q2
      r2 ~ tau*r1 + epsilon*p1 + chi*q1

      p1 ~~ p1 #variance
      p2 ~~ u*p2
      p3 ~~ u*p3
      q1 ~~ q1 #variance
      q2 ~~ v*q2
      q3 ~~ v*q3
      r1 ~~ r1 #variance
      r2 ~~ s*r2
      r3 ~~ s*r3

      p1 ~~ q1 #p1 and q1 covariance
      p1 ~~ r1 #p1 and r1 covariance
      q1 ~~ r1 #q1 and r1 covariance
      p2 ~~ uv*q2 #p2 and q2 covariance should also be constrained to be the same
      p2 ~~ us*r2
      q2 ~~ vs*r2
      p3 ~~ uv*q3 #p2 and q2 covariance
      p3 ~~ us*r3
      q3 ~~ vs*r3'

    riclpmConstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                   missing = 'ML',
                                   int.ov.free = F,
                                   int.lv.free = F,
                                   auto.fix.first = F,
                                   auto.fix.single = F,
                                   auto.cov.lv.x = F,
                                   auto.cov.y = F,
                                   auto.var = F)

    # subtract parameter estimates
    est1<-coef(riclpmConstrainedsim)[c(16:18, 22:24,28:30)]
    names(est1) <- c("alpha", "beta", "phi", "delta", "gamma", "psi", "tau", "epsilon", "chi")
    vcov<-lavInspect(riclpmConstrainedsim, "vcov")[c(16:18, 22:24,28:30), c(16:18, 22:24,28:30)]

    goricaResults1 <- goric(riclpmConstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

    return(list(GORICA = goricaResults1, estimates = est1, vcov = vcov))
  }

  independ4measureTri <- function(data, H1) {

    print("The Trivariate RI-CLPM for wave-independent parameters model with 4 measurement occasions is generating")

    RICLPMdata <- scale(RICLPMdata) #standardizing

    # Fitting RI-CLPM
    riclpmModel<-
      '
      kappa =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
      omega =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
      zeta  =~ 1*z1 + 1*z2 + 1*z3 + 1*z4

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1
      z1 ~ lambda1*1
      z2 ~ lambda2*1
      z3 ~ lambda3*1
      z4 ~ lambda4*1

      # Random intercept parts
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      zeta  ~~ zeta  #variance
      kappa ~~ omega #covariance
      kappa ~~ zeta  #covariance
      omega ~~ zeta  #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4
      r1 =~ 1*z1
      r2 =~ 1*z2
      r3 =~ 1*z3
      r4 =~ 1*z4

      #constrain autoregression and cross-lagged effects to be the same across both lags.
      p4 ~ alpha*p3 + beta*q3 + phi*r3
      p3 ~ alpha*p2 + beta*q2 + phi*r2
      p2 ~ alpha*p1 + beta*q1 + phi*r1

      q4 ~ delta*q3 + gamma*p3 + psi*r3
      q3 ~ delta*q2 + gamma*p2 + psi*r2
      q2 ~ delta*q1 + gamma*p1 + psi*r1

      r4 ~ tau*r3 + epsilon*p3 + chi*q3
      r3 ~ tau*r2 + epsilon*p2 + chi*q2
      r2 ~ tau*r1 + epsilon*p1 + chi*q1

      p1 ~~ p1 #variance
      p2 ~~ u*p2
      p3 ~~ u*p3
      p4 ~~ u*p4
      q1 ~~ q1 #variance
      q2 ~~ v*q2
      q3 ~~ v*q3
      q4 ~~ v*q4
      r1 ~~ r1 #variance
      r2 ~~ s*r2
      r3 ~~ s*r3
      r4 ~~ s*r4

      p1 ~~ q1 #p1 and q1 covariance
      p1 ~~ r1 #p1 and r1 covariance
      q1 ~~ r1 #q1 and r1 covariance
      p2 ~~ uv*q2 #p2 and q2 covariance should also be constrained to be the same
      p2 ~~ us*r2
      q2 ~~ vs*r2
      p3 ~~ uv*q3 #p3 and q3 covariance
      p3 ~~ us*r3
      q3 ~~ vs*r3
      p4 ~~ uv*q4 #p4 and q4 covariance
      p4 ~~ us*r4
      q4 ~~ vs*r4'


    riclpmConstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                   missing = 'ML',
                                   int.ov.free = F,
                                   int.lv.free = F,
                                   auto.fix.first = F,
                                   auto.fix.single = F,
                                   auto.cov.lv.x = F,
                                   auto.cov.y = F,
                                   auto.var = F)

    # subtract for alpha, beta, delta, gamma
    est1<-coef(riclpmConstrainedsim)[c(19:21, 28:30,37:39)]
    names(est1) <- c("alpha", "beta", "phi", "delta", "gamma", "psi", "tau", "epsilon", "chi")
    vcov<-lavInspect(riclpmConstrainedsim, "vcov")[c(19:21, 28:30,37:39), c(19:21, 28:30,37:39)]

    goricaResults1 <- goric(riclpmConstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

    return(list(GORICA = goricaResults1, estimates = est1, vcov = vcov))
  }

  independ5measureTri <- function(data, H1) {

    print("The Trivariate RI-CLPM for wave-independent parameters model with 5 measurement occasions is generating")

    RICLPMdata <- scale(RICLPMdata) #standardizing

    # Fitting RI-CLPM
    riclpmModel<-
      '
      kappa =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
      omega =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
      zeta  =~ 1*z1 + 1*z2 + 1*z3 + 1*z4 + 1*z5

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      x5 ~ mu5*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1
      y5 ~ pi5*1
      z1 ~ lambda1*1
      z2 ~ lambda2*1
      z3 ~ lambda3*1
      z4 ~ lambda4*1
      z5 ~ lambda5*1

      # Random intercept parts
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      zeta  ~~ zeta  #variance
      kappa ~~ omega #covariance
      kappa ~~ zeta  #covariance
      omega ~~ zeta  #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      p5 =~ 1*x5
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4
      q5 =~ 1*y5
      r1 =~ 1*z1
      r2 =~ 1*z2
      r3 =~ 1*z3
      r4 =~ 1*z4
      r5 =~ 1*z5

      #constrain autoregression and cross-lagged effects to be the same across both lags.
      p5 ~ alpha*p4 + beta*q4 + phi*r4
      p4 ~ alpha*p3 + beta*q3 + phi*r3
      p3 ~ alpha*p2 + beta*q2 + phi*r2
      p2 ~ alpha*p1 + beta*q1 + phi*r1

      q5 ~ delta*q4 + gamma*p4 + psi*r4
      q4 ~ delta*q3 + gamma*p3 + psi*r3
      q3 ~ delta*q2 + gamma*p2 + psi*r2
      q2 ~ delta*q1 + gamma*p1 + psi*r1

      r5 ~ tau*r4 + epsilon*p4 + chi*q4
      r4 ~ tau*r3 + epsilon*p3 + chi*q3
      r3 ~ tau*r2 + epsilon*p2 + chi*q2
      r2 ~ tau*r1 + epsilon*p1 + chi*q1

      p1 ~~ p1 #variance
      p2 ~~ u*p2
      p3 ~~ u*p3
      p4 ~~ u*p4
      p5 ~~ u*p5
      q1 ~~ q1 #variance
      q2 ~~ v*q2
      q3 ~~ v*q3
      q4 ~~ v*q4
      q5 ~~ v*q5
      r1 ~~ r1 #variance
      r2 ~~ s*r2
      r3 ~~ s*r3
      r4 ~~ s*r4
      r5 ~~ s*r5

      p1 ~~ q1 #p1 and q1 covariance
      p1 ~~ r1 #p1 and r1 covariance
      q1 ~~ r1 #q1 and r1 covariance
      p2 ~~ uv*q2 #p2 and q2 covariance should also be constrained to be the same
      p2 ~~ us*r2
      q2 ~~ vs*r2
      p3 ~~ uv*q3 #p3 and q3 covariance
      p3 ~~ us*r3
      q3 ~~ vs*r3
      p4 ~~ uv*q4 #p4 and q4 covariance
      p4 ~~ us*r4
      q4 ~~ vs*r4
      p5 ~~ uv*q5 #p5 and q5 covariance
      p5 ~~ us*r5
      q5 ~~ vs*r5'


    riclpmConstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                   missing = 'ML',
                                   int.ov.free = F,
                                   int.lv.free = F,
                                   auto.fix.first = F,
                                   auto.fix.single = F,
                                   auto.cov.lv.x = F,
                                   auto.cov.y = F,
                                   auto.var = F)

    # subtract for alpha, beta, delta, gamma
    est1<-coef(riclpmConstrainedsim)[c(22:24, 34:36,46:48)]
    names(est1) <- c("alpha", "beta", "phi", "delta", "gamma", "psi", "tau", "epsilon", "chi")
    vcov<-lavInspect(riclpmConstrainedsim, "vcov")[c(22:24, 34:36,46:48), c(22:24, 34:36,46:48)]

    goricaResults1 <- goric(riclpmConstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

    return(list(GORICA = goricaResults1, estimates = est1, vcov = vcov))
  }

  independ6measureTri <- function(data, H1) {

    print("The Trivariate RI-CLPM for wave-inindependent parameters model with 6 measurement occasions is generating")

    RICLPMdata <- scale(RICLPMdata) #standardizing

    # Fitting RI-CLPM
    riclpmModel<-
      '
      kappa =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5 + 1*x6
      omega =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5 + 1*y6
      zeta  =~ 1*z1 + 1*z2 + 1*z3 + 1*z4 + 1*z5 + 1*z6

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      x5 ~ mu5*1
      x6 ~ mu6*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1
      y5 ~ pi5*1
      y6 ~ pi6*1
      z1 ~ lambda1*1
      z2 ~ lambda2*1
      z3 ~ lambda3*1
      z4 ~ lambda4*1
      z5 ~ lambda5*1
      z6 ~ lambda6*1

      # Random intercept parts
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      zeta  ~~ zeta  #variance
      kappa ~~ omega #covariance
      kappa ~~ zeta  #covariance
      omega ~~ zeta  #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      p5 =~ 1*x5
      p6 =~ 1*x6
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4
      q5 =~ 1*y5
      q6 =~ 1*y6
      r1 =~ 1*z1
      r2 =~ 1*z2
      r3 =~ 1*z3
      r4 =~ 1*z4
      r5 =~ 1*z5
      r6 =~ 1*z6

      #constrain autoregression and cross-lagged effects to be the same across both lags.
      p6 ~ alpha*p5 + beta*q5 + phi*r5
      p5 ~ alpha*p4 + beta*q4 + phi*r4
      p4 ~ alpha*p3 + beta*q3 + phi*r3
      p3 ~ alpha*p2 + beta*q2 + phi*r2
      p2 ~ alpha*p1 + beta*q1 + phi*r1

      q6 ~ delta*q5 + gamma*p5 + psi*r5
      q5 ~ delta*q4 + gamma*p4 + psi*r4
      q4 ~ delta*q3 + gamma*p3 + psi*r3
      q3 ~ delta*q2 + gamma*p2 + psi*r2
      q2 ~ delta*q1 + gamma*p1 + psi*r1

      r6 ~ tau*r5 + epsilon*p5 + chi*q5
      r5 ~ tau*r4 + epsilon*p4 + chi*q4
      r4 ~ tau*r3 + epsilon*p3 + chi*q3
      r3 ~ tau*r2 + epsilon*p2 + chi*q2
      r2 ~ tau*r1 + epsilon*p1 + chi*q1

      p1 ~~ p1 #variance
      p2 ~~ u*p2
      p3 ~~ u*p3
      p4 ~~ u*p4
      p5 ~~ u*p5
      p6 ~~ u*p6
      q1 ~~ q1 #variance
      q2 ~~ v*q2
      q3 ~~ v*q3
      q4 ~~ v*q4
      q5 ~~ v*q5
      q6 ~~ v*q6
      r1 ~~ r1 #variance
      r2 ~~ s*r2
      r3 ~~ s*r3
      r4 ~~ s*r4
      r5 ~~ s*r5
      r6 ~~ s*r6

      p1 ~~ q1 #p1 and q1 covariance
      p1 ~~ r1 #p1 and r1 covariance
      q1 ~~ r1 #q1 and r1 covariance
      p2 ~~ uv*q2 #p2 and q2 covariance should also be constrained to be the same
      p2 ~~ us*r2
      q2 ~~ vs*r2
      p3 ~~ uv*q3 #p3 and q3 covariance
      p3 ~~ us*r3
      q3 ~~ vs*r3
      p4 ~~ uv*q4 #p4 and q4 covariance
      p4 ~~ us*r4
      q4 ~~ vs*r4
      p5 ~~ uv*q5 #p5 and q5 covariance
      p5 ~~ us*r5
      q5 ~~ vs*r5
      p6 ~~ uv*q6 #p6 and q6 covariance
      p6 ~~ us*r6
      q6 ~~ vs*r6'


    riclpmConstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                   missing = 'ML',
                                   int.ov.free = F,
                                   int.lv.free = F,
                                   auto.fix.first = F,
                                   auto.fix.single = F,
                                   auto.cov.lv.x = F,
                                   auto.cov.y = F,
                                   auto.var = F)

    # subtract for alpha, beta, delta, gamma
    est1<-coef(riclpmConstrainedsim)[c(25:27, 40:42,55:57)]
    names(est1) <- c("alpha", "beta", "phi", "delta", "gamma", "psi", "tau", "epsilon", "chi")
    vcov<-lavInspect(riclpmConstrainedsim, "vcov")[c(25:27, 40:42,55:57), c(25:27, 40:42,55:57)]

    goricaResults1 <- goric(riclpmConstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

    return(list(GORICA = goricaResults1, estimates = est1, vcov = vcov))
  }

  ################### Wave-specific parameters model in a tri-variate model ################################################

  specific3measureTri <- function(data, H1) {

    print("The Trivariate RI-CLPM for wave-specific parameters model with 3 measurement occasions is generating")

    # Fitting RI-CLPM
    riclpmModel<-
      '
      kappa =~ 1*x1 + 1*x2 + 1*x3
      omega =~ 1*y1 + 1*y2 + 1*y3
      zeta  =~ 1*z1 + 1*z2 + 1*z3

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      z1 ~ lambda1*1
      z2 ~ lambda2*1
      z3 ~ lambda3*1

      # Random intercept parts
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      zeta  ~~ zeta  #variance
      kappa ~~ omega #covariance
      kappa ~~ zeta  #covariance
      omega ~~ zeta  #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      r1 =~ 1*z1
      r2 =~ 1*z2
      r3 =~ 1*z3

      #Unconstrain autoregression and cross-lagged effects to be the same across both lags.
      p3 ~ alpha3*p2 + beta3*q2 + phi3*r2
      p2 ~ alpha2*p1 + beta2*q1 + phi2*r1

      q3 ~ delta3*q2 + gamma3*p2 + psi3*r2
      q2 ~ delta2*q1 + gamma2*p1 + psi2*r1

      r3 ~ tau3*r2 + epsilon3*p2 + chi3*q2
      r2 ~ tau2*r1 + epsilon2*p1 + chi2*q1

      p1 ~~ p1 #variance
      p2 ~~ u2*p2
      p3 ~~ u3*p3
      q1 ~~ q1 #variance
      q2 ~~ v2*q2
      q3 ~~ v3*q3
      r1 ~~ r1 #variance
      r2 ~~ s2*r2
      r3 ~~ s3*r3

      p1 ~~ q1 #p1 and q1 covariance
      p1 ~~ r1 #p1 and r1 covariance
      q1 ~~ r1 #q1 and r1 covariance
      p2 ~~ u2v2*q2 #p2 and q2 covariance should also be constrained to be the same
      p2 ~~ u2s2*r2
      q2 ~~ v2s2*r2
      p3 ~~ u3v3*q3 #p2 and q2 covariance
      p3 ~~ u3s3*r3
      q3 ~~ v3s3*r3'

    riclpmUnconstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                     missing = 'ML',
                                     int.ov.free = F,
                                     int.lv.free = F,
                                     auto.fix.first = F,
                                     auto.fix.single = F,
                                     auto.cov.lv.x = F,
                                     auto.cov.y = F,
                                     auto.var = F)

    # subtract standardized estimates
    StdEst <- standardizedsolution(riclpmUnconstrainedsim, type = "std.nox",
                                   se = TRUE, zstat = TRUE, pvalue = TRUE,
                                   ci = TRUE, level = 0.95, cov.std = TRUE,
                                   remove.eq = TRUE, remove.ineq = TRUE,
                                   remove.def = FALSE, partable = NULL,
                                   GLIST = NULL, est = NULL)

    est1 <- StdEst[c(34:51),5]
    names(est1) <- c("alpha3", "beta3", "phi3", "alpha2", "beta2", "phi2", "delta3", "gamma3", "psi3", "delta2", "gamma2", "psi2", "tau3", "epsilon3", "chi3", "tau2", "epsilon2", "chi2")
    vcov <- lavInspect(riclpmUnconstrainedsim, "vcov.std.nox")[c(16:33), c(16:33)]

    goricaResults1 <- goric(riclpmUnconstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

    return(list(GORICA = goricaResults1, estimates = est1, vcov = vcov))
  }

  specific4measureTri <- function(data, H1) {

    print("The Trivariate RI-CLPM for wave-specific parameters model with 4 measurement occasions is generating")

    # Fitting RI-CLPM
    riclpmModel<-
      '
      kappa =~ 1*x1 + 1*x2 + 1*x3 + 1*x4
      omega =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
      zeta  =~ 1*z1 + 1*z2 + 1*z3 + 1*z4

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1
      z1 ~ lambda1*1
      z2 ~ lambda2*1
      z3 ~ lambda3*1
      z4 ~ lambda4*1

      # Random intercept parts
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      zeta  ~~ zeta  #variance
      kappa ~~ omega #covariance
      kappa ~~ zeta  #covariance
      omega ~~ zeta  #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4
      r1 =~ 1*z1
      r2 =~ 1*z2
      r3 =~ 1*z3
      r4 =~ 1*z4

      #Unconstrain autoregression and cross-lagged effects.
      p4 ~ alpha4*p3 + beta4*q3 + phi4*r3
      p3 ~ alpha3*p2 + beta3*q2 + phi3*r2
      p2 ~ alpha2*p1 + beta2*q1 + phi2*r1

      q4 ~ delta4*q3 + gamma4*p3 + psi4*r3
      q3 ~ delta3*q2 + gamma3*p2 + psi3*r2
      q2 ~ delta2*q1 + gamma2*p1 + psi2*r1

      r4 ~ tau4*r3 + epsilon4*p3 + chi4*q3
      r3 ~ tau3*r2 + epsilon3*p2 + chi3*q2
      r2 ~ tau2*r1 + epsilon2*p1 + chi2*q1

      p1 ~~ p1 #variance
      p2 ~~ u2*p2
      p3 ~~ u3*p3
      p4 ~~ u4*p4
      q1 ~~ q1 #variance
      q2 ~~ v2*q2
      q3 ~~ v3*q3
      q4 ~~ v4*q4
      r1 ~~ r1 #variance
      r2 ~~ s2*r2
      r3 ~~ s3*r3
      r4 ~~ s4*r4

      p1 ~~ q1 #p1 and q1 covariance
      p1 ~~ r1 #p1 and r1 covariance
      q1 ~~ r1 #q1 and r1 covariance
      p2 ~~ u2v2*q2 #p2 and q2 covariance should also be constrained to be the same
      p2 ~~ u2s2*r2
      q2 ~~ v2s2*r2
      p3 ~~ u3v3*q3 #p3 and q3 covariance
      p3 ~~ u3s3*r3
      q3 ~~ v3s3*r3
      p4 ~~ u4v4*q4 #p4 and q4 covariance
      p4 ~~ u4s4*r4
      q4 ~~ v4s4*r4'


    riclpmUnconstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                     missing = 'ML',
                                     int.ov.free = F,
                                     int.lv.free = F,
                                     auto.fix.first = F,
                                     auto.fix.single = F,
                                     auto.cov.lv.x = F,
                                     auto.cov.y = F,
                                     auto.var = F)

    # subtract standardized estimates
    StdEst <- standardizedsolution(riclpmUnconstrainedsim, type = "std.nox",
                                   se = TRUE, zstat = TRUE, pvalue = TRUE,
                                   ci = TRUE, level = 0.95, cov.std = TRUE,
                                   remove.eq = TRUE, remove.ineq = TRUE,
                                   remove.def = FALSE, partable = NULL,
                                   GLIST = NULL, est = NULL)

    est1 <- StdEst[c(43:69),5]
    names(est1) <- c("alpha4", "beta4", "phi4", "alpha3", "beta3", "phi3", "alpha2", "beta2", "phi2", "delta4", "gamma4", "psi4", "delta3", "gamma3", "psi3", "delta2", "gamma2", "psi2", "tau4", "epsilon4", "chi4", "tau3", "epsilon3", "chi3", "tau2", "epsilon2", "chi2")
    vcov <- lavInspect(riclpmUnconstrainedsim, "vcov.std.nox")[c(19:45), c(19:45)]

    goricaResults1 <- goric(riclpmUnconstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

    return(list(GORICA = goricaResults1, estimates = est1, vcov = vcov))
  }

  specific5measureTri <- function(data, H1) {

    print("The Trivariate RI-CLPM for wave-specific parameters model with 5 measurement occasions is generating")

    # Fitting RI-CLPM
    riclpmModel<-
      '
      kappa =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5
      omega =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5
      zeta  =~ 1*z1 + 1*z2 + 1*z3 + 1*z4 + 1*z5

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      x5 ~ mu5*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1
      y5 ~ pi5*1
      z1 ~ lambda1*1
      z2 ~ lambda2*1
      z3 ~ lambda3*1
      z4 ~ lambda4*1
      z5 ~ lambda5*1

      # Random intercept parts
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      zeta  ~~ zeta  #variance
      kappa ~~ omega #covariance
      kappa ~~ zeta  #covariance
      omega ~~ zeta  #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      p5 =~ 1*x5
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4
      q5 =~ 1*y5
      r1 =~ 1*z1
      r2 =~ 1*z2
      r3 =~ 1*z3
      r4 =~ 1*z4
      r5 =~ 1*z5

      #Unconstrain autoregression and cross-lagged effects.
      p5 ~ alpha5*p4 + beta5*q4 + phi5*r4
      p4 ~ alpha4*p3 + beta4*q3 + phi4*r3
      p3 ~ alpha3*p2 + beta3*q2 + phi3*r2
      p2 ~ alpha2*p1 + beta2*q1 + phi2*r1

      q5 ~ delta5*q4 + gamma5*p4 + psi5*r4
      q4 ~ delta4*q3 + gamma4*p3 + psi4*r3
      q3 ~ delta3*q2 + gamma3*p2 + psi3*r2
      q2 ~ delta2*q1 + gamma2*p1 + psi2*r1

      r5 ~ tau5*r4 + epsilon5*p4 + chi5*q4
      r4 ~ tau4*r3 + epsilon4*p3 + chi4*q3
      r3 ~ tau3*r2 + epsilon3*p2 + chi3*q2
      r2 ~ tau2*r1 + epsilon2*p1 + chi2*q1

      p1 ~~ p1 #variance
      p2 ~~ u2*p2
      p3 ~~ u3*p3
      p4 ~~ u4*p4
      p5 ~~ u5*p5
      q1 ~~ q1 #variance
      q2 ~~ v2*q2
      q3 ~~ v3*q3
      q4 ~~ v4*q4
      q5 ~~ v5*q5
      r1 ~~ r1 #variance
      r2 ~~ s2*r2
      r3 ~~ s3*r3
      r4 ~~ s4*r4
      r5 ~~ s5*r5

      p1 ~~ q1 #p1 and q1 covariance
      p1 ~~ r1 #p1 and r1 covariance
      q1 ~~ r1 #q1 and r1 covariance
      p2 ~~ u2v2*q2 #p2 and q2 covariance should also be constrained to be the same
      p2 ~~ u2s2*r2
      q2 ~~ v2s2*r2
      p3 ~~ u3v3*q3 #p3 and q3 covariance
      p3 ~~ u3s3*r3
      q3 ~~ v3s3*r3
      p4 ~~ u4v4*q4 #p4 and q4 covariance
      p4 ~~ u4s4*r4
      q4 ~~ v4s4*r4
      p5 ~~ u5v5*q5 #p5 and q5 covariance
      p5 ~~ u5s5*r5
      q5 ~~ v5s5*r5'


    riclpmUnconstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                     missing = 'ML',
                                     int.ov.free = F,
                                     int.lv.free = F,
                                     auto.fix.first = F,
                                     auto.fix.single = F,
                                     auto.cov.lv.x = F,
                                     auto.cov.y = F,
                                     auto.var = F)

    # subtract standardized estimates
    StdEst <- standardizedsolution(riclpmUnconstrainedsim, type = "std.nox",
                                   se = TRUE, zstat = TRUE, pvalue = TRUE,
                                   ci = TRUE, level = 0.95, cov.std = TRUE,
                                   remove.eq = TRUE, remove.ineq = TRUE,
                                   remove.def = FALSE, partable = NULL,
                                   GLIST = NULL, est = NULL)

    est1 <- StdEst[c(52:87),5]
    names(est1) <- c("alpha5", "beta5", "phi5", "alpha4", "beta4", "phi4", "alpha3", "beta3", "phi3", "alpha2", "beta2", "phi2", "delta5", "gamma5", "psi5",  "delta4", "gamma4", "psi4", "delta3", "gamma3", "psi3", "delta2", "gamma2", "psi2", "tau5", "epsilon5", "chi5", "tau4", "epsilon4", "chi4", "tau3", "epsilon3", "chi3", "tau2", "epsilon2", "chi2")
    vcov<-lavInspect(riclpmUnconstrainedsim, "vcov.std.nox")[c(22:57), c(22:57)]

    goricaResults1 <- goric(riclpmUnconstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

    return(list(GORICA = goricaResults1, estimates = est1, vcov = vcov))
  }

  specific6measureTri <- function(data, H1) {

    print("The Trivariate RI-CLPM for wave-specific parameters model with 6 measurement occasions is generating")

    # Fitting RI-CLPM
    riclpmModel<-
      '
      kappa =~ 1*x1 + 1*x2 + 1*x3 + 1*x4 + 1*x5 + 1*x6
      omega =~ 1*y1 + 1*y2 + 1*y3 + 1*y4 + 1*y5 + 1*y6
      zeta  =~ 1*z1 + 1*z2 + 1*z3 + 1*z4 + 1*z5 + 1*z6

      x1 ~ mu1*1 #intercepts
      x2 ~ mu2*1
      x3 ~ mu3*1
      x4 ~ mu4*1
      x5 ~ mu5*1
      x6 ~ mu6*1
      y1 ~ pi1*1
      y2 ~ pi2*1
      y3 ~ pi3*1
      y4 ~ pi4*1
      y5 ~ pi5*1
      y6 ~ pi6*1
      z1 ~ lambda1*1
      z2 ~ lambda2*1
      z3 ~ lambda3*1
      z4 ~ lambda4*1
      z5 ~ lambda5*1
      z6 ~ lambda6*1

      # Random intercept parts
      kappa ~~ kappa #variance
      omega ~~ omega #variance
      zeta  ~~ zeta  #variance
      kappa ~~ omega #covariance
      kappa ~~ zeta  #covariance
      omega ~~ zeta  #covariance

      #latent vars for AR and cross-lagged effects
      p1 =~ 1*x1 #each factor loading set to 1
      p2 =~ 1*x2
      p3 =~ 1*x3
      p4 =~ 1*x4
      p5 =~ 1*x5
      p6 =~ 1*x6
      q1 =~ 1*y1
      q2 =~ 1*y2
      q3 =~ 1*y3
      q4 =~ 1*y4
      q5 =~ 1*y5
      q6 =~ 1*y6
      r1 =~ 1*z1
      r2 =~ 1*z2
      r3 =~ 1*z3
      r4 =~ 1*z4
      r5 =~ 1*z5
      r6 =~ 1*z6

      #Unconstrain autoregression and cross-lagged effects.
      p6 ~ alpha6*p5 + beta6*q5 + phi6*r5
      p5 ~ alpha5*p4 + beta5*q4 + phi5*r4
      p4 ~ alpha4*p3 + beta4*q3 + phi4*r3
      p3 ~ alpha3*p2 + beta3*q2 + phi3*r2
      p2 ~ alpha2*p1 + beta2*q1 + phi2*r1

      q6 ~ delta6*q5 + gamma6*p5 + psi6*r5
      q5 ~ delta5*q4 + gamma5*p4 + psi5*r4
      q4 ~ delta4*q3 + gamma4*p3 + psi4*r3
      q3 ~ delta3*q2 + gamma3*p2 + psi3*r2
      q2 ~ delta2*q1 + gamma2*p1 + psi2*r1

      r6 ~ tau6*r5 + epsilon6*p5 + chi6*q5
      r5 ~ tau5*r4 + epsilon5*p4 + chi5*q4
      r4 ~ tau4*r3 + epsilon4*p3 + chi4*q3
      r3 ~ tau3*r2 + epsilon3*p2 + chi3*q2
      r2 ~ tau2*r1 + epsilon2*p1 + chi2*q1

      p1 ~~ p1 #variance
      p2 ~~ u2*p2
      p3 ~~ u3*p3
      p4 ~~ u4*p4
      p5 ~~ u5*p5
      p6 ~~ u6*p6
      q1 ~~ q1 #variance
      q2 ~~ v2*q2
      q3 ~~ v3*q3
      q4 ~~ v4*q4
      q5 ~~ v5*q5
      q6 ~~ v6*q6
      r1 ~~ r1 #variance
      r2 ~~ s2*r2
      r3 ~~ s3*r3
      r4 ~~ s4*r4
      r5 ~~ s5*r5
      r6 ~~ s6*r6

      p1 ~~ q1 #p1 and q1 covariance
      p1 ~~ r1 #p1 and r1 covariance
      q1 ~~ r1 #q1 and r1 covariance
      p2 ~~ u2v2*q2 #p2 and q2 covariance should also be constrained to be the same
      p2 ~~ u2s2*r2
      q2 ~~ v2s2*r2
      p3 ~~ u3v3*q3 #p3 and q3 covariance
      p3 ~~ u3s3*r3
      q3 ~~ v3s3*r3
      p4 ~~ u4v4*q4 #p4 and q4 covariance
      p4 ~~ u4s4*r4
      q4 ~~ v4s4*r4
      p5 ~~ u5v5*q5 #p5 and q5 covariance
      p5 ~~ u5s5*r5
      q5 ~~ v5s5*r5
      p6 ~~ u6v6*q6 #p6 and q6 covariance
      p6 ~~ u6s6*r6
      q6 ~~ v6s6*r6'


    riclpmUnconstrainedsim <- lavaan(riclpmModel, data = RICLPMdata,
                                     missing = 'ML',
                                     int.ov.free = F,
                                     int.lv.free = F,
                                     auto.fix.first = F,
                                     auto.fix.single = F,
                                     auto.cov.lv.x = F,
                                     auto.cov.y = F,
                                     auto.var = F)

    # subtract standardized estimates
    StdEst <- standardizedsolution(riclpmUnconstrainedsim, type = "std.nox",
                                   se = TRUE, zstat = TRUE, pvalue = TRUE,
                                   ci = TRUE, level = 0.95, cov.std = TRUE,
                                   remove.eq = TRUE, remove.ineq = TRUE,
                                   remove.def = FALSE, partable = NULL,
                                   GLIST = NULL, est = NULL)

    est1 <- StdEst[c(61:105),5]
    names(est1) <- c("alpha6", "beta6", "phi6", "alpha5", "beta5", "phi5", "alpha4", "beta4", "phi4", "alpha3", "beta3", "phi3", "alpha2", "beta2", "phi2",  "delta6", "gamma6", "psi6", "delta5", "gamma5", "psi5", "delta4", "gamma4", "psi4", "delta3", "gamma3", "psi3", "delta2", "gamma2", "psi2", "tau6", "epsilon6", "chi6", "tau5", "epsilon5", "chi5", "tau4", "epsilon4", "chi4", "tau3", "epsilon3", "chi3", "tau2", "epsilon2", "chi2")
    vcov <- lavInspect(riclpmUnconstrainedsim, "vcov.std.nox")[c(25:69), c(25:69)]

    goricaResults1 <- goric(riclpmUnconstrainedsim, hypotheses = list(H1), comparison = "complement", type = "gorica")

    return(list(GORICA = goricaResults1, estimates = est1, vcov = vcov))
  }

  ##############################################################################################################################################################################################################################

  if (n == 2) {           # n = 2, bivariate model
    if (model == 1) {   # model = 1, wave-independent parameters model
      if (M == 3) {
        result <- independ3measureBi(data, H1)
      } else if (M == 4) {
        result <- independ4measureBi(data, H1)
      } else if (M == 5) {
        result <- independ5measureBi(data, H1)
      } else if (M == 6) {
        result <- independ6measureBi(data, H1)
      } else {
        print("The number of measurement occasions should range from 3 to 6")
      }
    } else if (model == 2) {   # model = 2, wave-specific parameters model
      if (M == 3) {
        result <- specific3measureBi(data, H1)
      } else if (M == 4) {
        result <- specific4measureBi(data, H1)
      } else if (M == 5) {
        result <- specific5measureBi(data, H1)
      } else if (M == 6) {
        result <- specific6measureBi(data, H1)
      } else {
        print("The number of measurement occasions should range from 3 to 6")
      }
    } else {

      print("Invalid model, model == 1: wave-independent parameters model and model == 2: wave-specific parameters model")
    }

  } else if (n == 3) {    # n = 3, trivariate model
    if (model == 1) {   # model = 1, wave-independent parameters model
      if (M == 3) {
        result <- independ3measureTri(data, H1)
      } else if (M == 4) {
        result <- independ4measureTri(data, H1)
      } else if (M == 5) {
        result <- independ5measureTri(data, H1)
      } else if (M == 6) {
        result <- independ6measureTri(data, H1)
      } else {
        print("The number of measurement occasions should range from 3 to 6")
      }
    } else if (model == 2) {   # model = 2, wave-specific parameters model
      if (M == 3) {
        result <- specific3measureTri(data, H1)
      } else if (M == 4) {
        result <- specific4measureTri(data, H1)
      } else if (M == 5) {
        result <- specific5measureTri(data, H1)
      } else if (M == 6) {
        result <- specific6measureTri(data, H1)
      } else {
        print("The number of measurement occasions should range from 3 to 6")
      }
    } else {

      print("Invalid model, model == 1: wave-independent parameters model and model == 2: wave-specific parameters model")
    }

  } else {

    print("The number of variables should be 2 or 3, two variables for a bivariate model and three variables for a trivariate model")

  }

  return(result)

} #end of GORICA.test

