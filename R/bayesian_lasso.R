#' Bayesian Lasso Regression
#'
#' Perform Bayesian Lasso regression by 
#' protecting certain variables.
#'
#' @param df A data frame containing the variables for the analysis.
#' @param outcome_var The name of the outcome variable in the data frame.
#' @param protected_vars A vector of variable names that should not be penalized.
#' @param save_output Logical; if TRUE, the function saves the output in a specified location.
#' @param num.reps The number of MCMC repetitions.
#' @param a The shape parameter of the gamma distribution for the prior on lambda squared.
#' @param b The rate parameter of the gamma distribution for the prior on lambda squared.
#' @param burn The number of initial samples to discard (burn-in).
#' @param seed The random seed for reproducibility.
#' @param output_name The name of the output file to save the results.
#'
#' @return A list containing the lambda lasso, beta posterior, and mean beta.
#'
#' @examples
#' n <- 100
#' x1 <- sample(c("A", "B"), n, replace = TRUE)
#' x1[sample(n, 10)] <- NA  # add missing values to the first column
#' x2 <- rnorm(n)
#' x2[sample(n, 5)] <- NA  # add missing values to the second column
#' x3 <- rnorm(n)
#' y <- as.numeric(x1 == "A") + x2 + rnorm(n)
#' mydata <- data.frame(x1, x2, x3, y)
#' model <- bayesian_lasso(df=mydata, outcome_var = "y", protected_vars = c("X1"))
#' @importFrom ggplot2 ggplot aes geom_point geom_linerange scale_y_continuous coord_flip theme_minimal theme element_blank ggtitle
#' @importFrom purrr keep
#' @importFrom bayestestR ci
#' @import dplyr
#' @importFrom stats rexp rgamma runif rnorm median na.omit reorder
#' @importFrom SuppDists rinvGauss
#' @importFrom MCMCpack rinvgamma
#' @export
#'
?usethis::use_vignette()
bayesian_lasso <- function(df, outcome_var, protected_vars = NULL, save_output = FALSE, num.reps = 10000, a = 1, b = 0.1, burn = 501, seed =123,
                           output_name="bayesian_lasso_output") {

  # I REMOVED THE INTERACTIVE PART WHERE THE USER TYPES Y OR N TO PROCEED WITH LISTWISE DELETION                          
  if (any(is.na(df))) {
    # Proceed with listwise deletion without user confirmation.
    n_original <- nrow(df)
    df <- na.omit(df)  # This is a bit cleaner than df[complete.cases(df), ]
    n_new <- nrow(df)
    message("Number of rows after listwise deletion: ", n_new, " (", n_original - n_new, " rows deleted)")
  } else {
    message("No missing values detected in the dataset. Proceeding to the next step...")
  }

  
  message(c("\nJob started at:",date()))   

  n      = nrow(df)
  Y = df[,outcome_var]
  message("Number of non-numeric variables (", ncol(df)-sum(sapply(df, is.numeric)),") will be removed from the predictors.\n")
  df <- df[,sapply(df, is.numeric)] # REMOVE ALL CATEGORICAL VARIABLES
  
  n.prot <- length(protected_vars)
  oX <- as.matrix(df %>% dplyr::select(-{{outcome_var}})) # create predictor matrix, without the outcome.
  
  p      = ncol(oX)
  J      = matrix (1, nrow=n, ncol=1) # for intercept
  X      = (oX - J%*%colMeans(oX))/(J%*%apply(oX,2,sd)) #STANDARDIZED X
  
  XX = t(X)%*%X
  
  Y.til  = Y - mean(Y)
  
  # PRIORS 
  lambda.sq  <- rgamma(1,shape=a, rate=b) 
  sig.sq     <- runif(1,0.1,10)
  if ((p - n.prot) <= 0) {
    stop("You're protecting ALL predictors. Which essentially means Bayesian linear regression. Please leave at least one NUMERIC variable out of protection.")}
  tau.sq.nprot <- rexp(p-n.prot, rate=lambda.sq/2)
  tau.sq.prot <- rgamma(n.prot,1.0)
  tau.sq <- c(tau.sq.nprot,tau.sq.prot)
  mean.be    <- rep(0,p)
  cov.be     <- sig.sq*diag(tau.sq)
  beta.l     <- rmvnorm(1,mean=mean.be,sigma=cov.be)
  
  # FOR POSTERIOR
  sigsq.post <- lambda.post <- NULL 
  tau.sq.post.prot <- rbind( tau.sq.prot,matrix(rep(NA,num.reps*n.prot),ncol=n.prot))
  tau.sq.post.nprot <- rbind(tau.sq.nprot,matrix(rep(NA,num.reps*(p-n.prot)),ncol=(p-n.prot)))
  tau.sq.post <- rbind( tau.sq,matrix(rep(NA,num.reps*p),ncol=p))
  
  beta.p     <- rbind( beta.l,matrix(rep(NA,num.reps*p),ncol=p) )
  
  lambda.lasso <- c()
  
  # POSTERIOR
  set.seed(seed)
  if (n.prot==0) {
    # MCMC LOOPING
    for (M in 1:num.reps)  {
      #Full Conditional Posteriors  
      # beta
      dtau.inv   <- diag(1/tau.sq)
      cov.be     <- sig.sq * solve(XX+dtau.inv)
      mean.be    <- 1/sig.sq * cov.be%*%t(X)%*%(Y.til)
      beta.p[M+1,] <- rmvnorm(1,mean=mean.be,sigma=cov.be)

      #tau.sq
      gam <- c()
      for (j in 1:p){
        repeat{
          gam[j]  <- rinvGauss(1, nu=sqrt(lambda.sq * sig.sq/beta.p[M+1,j]^2), lambda=lambda.sq)
          if (gam[j] > 0) break    	
        }
        tau.sq[j] <- 1/gam[j]
      }
      tau.sq.post[M+1,] <- tau.sq 	
      
      # sig.sq
      sh.sig     <- (n-1+p)/2
      sc.sig     <- 1/2*t(Y.til-X%*%beta.p[M+1,])%*%(Y.til-X%*%beta.p[M+1,])+ 1/2*t(beta.p[M+1,])%*%diag(1/tau.sq)%*%beta.p[M+1,]
      sig.sq     <- rinvgamma(1, shape=sh.sig, scale=sc.sig)
      sigsq.post <- c(sigsq.post, sig.sq)
      
      # lambda
      sh.lam      <- p + a 
      sc.lam      <- 1/2*sum(tau.sq) + b 
      lambda.sq   <- rgamma(1, shape=sh.lam, rate=sc.lam)
      lambda.post <- c(lambda.post, lambda.sq)
    }
  } else {
    # MCMC LOOPING IF SOME VARIABLES ARE TO BE PROTECTED
    set.seed(seed)
    
    # MCMC LOOPING
    for (M in 1:num.reps)  {
      #Full Conditional Posteriors  
      # beta
      dtau.inv   <- diag(1/tau.sq)
      cov.be     <- sig.sq * solve(XX+dtau.inv)
      mean.be    <- 1/sig.sq * cov.be%*%t(X)%*%(Y.til) 
      beta.p[M+1,] <- rmvnorm(1,mean=mean.be,sigma=cov.be)
      
      if(n.prot==p){
        gam.prot <- c()
        for (j in 1:n.prot){ 
          repeat{
            gam.prot[j]  <- rgamma(n = 1,shape = sqrt(sig.sq/beta.p[M+1,j]^2)) 
            
            if (gam.prot[j] > 0) break
          }
          tau.sq.prot[j] <- 1/gam.prot[j]
        }
        tau.sq.post.prot[M+1,] <- tau.sq.prot
        
      }else{
        gam <- c() 
        for (j in 1:(p-n.prot)){
          repeat{
            gam[j]  <- rinvGauss(1, nu=sqrt(lambda.sq * sig.sq/beta.p[M+1,j]^2), lambda=lambda.sq) 
            if (gam[j] > 0) break 
          }
          tau.sq.nprot[j] <- 1/gam[j]
        }
        tau.sq.post.nprot[M+1,] <- tau.sq.nprot 	
        
        
        gam.prot <- c() 
        for (j in 1:n.prot){
          repeat{
            gam.prot[j]  <- rgamma(n = 1,shape = sqrt(sig.sq/beta.p[M+1,j]^2)) 
            if (gam.prot[j] > 0) break 
          }
          tau.sq.prot[j] <- 1/gam.prot[j]
        }
        tau.sq.post.prot[M+1,] <- tau.sq.prot
      }
      
      ## MERGE TAU sq s
      if (n.prot==p){
        tau.sq <- c(tau.sq.nprot,tau.sq.prot )
        tau.sq.post <- tau.sq.post.nprot
      }else{
        tau.sq <- c(tau.sq.nprot,tau.sq.prot )
        tau.sq.post <- cbind(tau.sq.post.nprot,tau.sq.post.prot)
      }
      
      # sig.sq
      sh.sig     <- (n-1+(p))/2 
      sc.sig     <- 1/2*t(Y.til-X%*%beta.p[M+1,])%*%(Y.til-X%*%beta.p[M+1,])+ 1/2*t(beta.p[M+1,])%*%diag(1/tau.sq)%*%beta.p[M+1,]
      sig.sq     <- rinvgamma(1, shape=sh.sig, scale=sc.sig)
      sigsq.post <- c(sigsq.post, sig.sq)
      
      
      
      # lambda
      sh.lam      <- (p-n.prot) + a
      sc.lam      <- 1/2*sum(tau.sq) + b 
      lambda.sq   <- rgamma(1, shape=sh.lam, rate=sc.lam)
      lambda.post <- c(lambda.post, lambda.sq)
    }
    
  }
  
  #######################################################
  # POSTERIOR MEDIAN OF LAMBDA; MCMC AT POST.LAM
  ######################################################
  if(exists("lambda.post")) {
    lam.sq.post <- lambda.post[burn:num.reps]
    lam.post <- sqrt(lam.sq.post)
    lambda.la <- median(lam.post)
    lambda.lasso <- c(lambda.lasso, lambda.la)
  }
  
  if(exists("t.beta")) {
    ms1.l <- t(t.beta - mean_beta) %*% XX.t %*% (t.beta - mean_beta)
    ms2.l <- t(t.beta - mean_beta) %*% (t.beta - mean_beta)
  }
  
  # POSTERIOR ESTIMATES
  beta.post <- beta.p[burn:num.reps,]
  mean_beta  <- apply(beta.post, 2, mean)
  
  if (save_output && !file.exists("output")) {
    dir.create("output")
    message ("Output directory created")
  }
  
  if(save_output) {
    if(exists("lambda.post")){
      save(lambda.lasso, beta.post, mean_beta, file = paste0("output/",output_name,".Rdata"))
      message ("Results are saved to the output folder")
    } else {
      save(beta.post, mean_beta, file = paste0("output/",output_name,".Rdata"))
      message ("Results are saved to the output folder")
    }
    
  }
  message(c("Job finished at:",date())) 
  # Return a list of outputs
  if(exists("lambda.post")){
    return(list(lambda.lasso = lambda.lasso, beta_post = beta.post, mean_beta = mean_beta))
  } else {
    return(list(beta_post = beta.post, mean_beta = mean_beta))
  }
  
}
