# Chapter 2 of Statistical Rethinking 

# 2.1 
sample <- c("W","L","W","W", "W", "L", "W","L","W")
W <- sum(sample=="W") #number of W observed 
L <- sum(sample=="L") # number of L observed 
p <- c(0,0.25,0.5,0.75, 1) # proportions W
ways <- sapply(p, function(q) (q *4)^W *((1-q)*4)^L)
prob <- ways/sum(ways)
cbind(p, ways, prob)


## Test Before You Est(imate)
# (1) Code a generative simulation 
# 2.3 function to toss a globe covered p by water N times 
sim_globe <- function(p=0.7, N=9) {
  sample(c("W","L"), size=N, prob=c(p, 1-p), replace=TRUE)
}

# 2.4
sim_globe()
replicate(sim_globe(p=0.5,N=9), n=10)

# 2.5-2.6 Test the simulation on extreme settings 
sim_globe(p=1, N=11)
sum(sim_globe(p=0.5, N=1e4) == "W") / 1e4  #the proportion of water should be very close to 0.5 

# If you test nothing, you miss everything 

# (2) Code the estimator
# Ways for p to produce W,L = (4p)^W * (4-4p)^L
# Function to compute posterior distribution 
compute_posterior <- function(the_sample, poss=c(0,0.25,0.5,0.75,1)) {
  W <- sum(the_sample=="W") # number of W observed
  L <- sum(sample=="L") # number of L observed 
  ways <- sapply(poss, function(q) (q *4)^W *((1-q)*4)^L)
  post <- ways/sum(ways)
  bars <- sapply(post, function(q) make_bar(q))
  data.frame(poss, ways, post=round(post,3), bars)
}

#2.9 
compute_posterior(sim_globe())
