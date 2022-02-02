data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1, upper=T> Tsubj[N]; //State variable: round
  real<lower=0> choice[N, T]; // Effort in minutes
  real<lower=0> outcome[N, T];  // no lower and upper bounds
}
transformed data {
  vector[4] initV;  // initial values
  initV = rep_vector(0.01, 4);
}
parameters {
// Declare all parameters as vectors for vectorizing
  // Hyper(group)-parameters
  //vector[2] mu_theta_mu;
  //vector[2] mu_theta_sigma;
  //vector[2] mu_w;
  real mu_cost;
  real<lower=0, upper = 1> mu_alpha_w;
  real<lower=0, upper = 1> mu_alpha_theta;
  
  real<lower=0> a_lambda;
  real<lower=0> b_lambda;
  
  real<lower=0> a_eta;
  real<lower=0> b_eta;
  
  //vector<lower=0>[2] tau_theta_mu;
  //vector<lower=0>[2] tau_theta_sigma;
  //vector<lower=0>[2] tau_w;
  real<lower=0> tau_cost;
  real<lower=0> tau_alpha_w;
  real<lower=0> tau_alpha_theta;

  // Subject-level raw parameters (for transformation from hyper to subj parameter)
  //vector[2] theta_mu[N];    // mean policy parameters
  //vector[2] theta_sigma[N]; // sd policy parameters
  //vector[2] w[N];           // Value paremeters
  vector<lower = 0>[N] cost;        // cost
  vector<lower = 0, upper = 1>[N] alpha_w;     // step size w
  vector<lower = 0, upper = 1>[N] alpha_theta; // step size theta
  vector<lower = 0, upper = 1>[N] lambda;        // mixing parameter
  vector<lower = 0>[N] eta;                      // variance of 0-normal dist
}
model {
  // Hyperparameters
  //mu_theta_mu  ~ normal(0, 5); // weakly informative priors
  //mu_theta_sigma  ~ normal(0, 5);
  //mu_w  ~ normal(0, 5);
  mu_cost  ~ normal(0, 5);
  mu_alpha_w  ~ normal(0, 5);
  mu_alpha_theta  ~ normal(0, 5);
  
  a_lambda ~ cauchy(0, 2.5);
  b_lambda ~ cauchy(0, 2.5);
  
  a_eta ~ cauchy(0, 2.5);
  b_eta ~ cauchy(0, 2.5);
  
  //tau_theta_mu ~ cauchy(0, 2.5);  // weakly informative priors, see section 6.9 in STAN user guide
  //tau_theta_sigma ~ cauchy(0, 2.5);
  //tau_w ~ cauchy(0, 2.5);
  tau_cost ~ cauchy(0, 2.5);
  tau_alpha_w ~ cauchy(0, 2.5);
  tau_alpha_theta ~ cauchy(0, 2.5);

  
  for(j in 1:N){
   //theta_mu[j]    ~ normal(mu_theta_mu,tau_theta_mu); //fill the matrix of group-level parameters
   //theta_sigma[j] ~ normal(mu_theta_sigma,tau_theta_sigma);
   //w[j]           ~ normal(mu_w,tau_w);
   cost[j]        ~ normal(mu_cost,tau_cost);
   alpha_w[j]     ~ normal(mu_alpha_w,tau_alpha_w);
   alpha_theta[j] ~ normal(mu_alpha_theta,tau_alpha_theta);
   lambda[j]      ~ beta(a_lambda, b_lambda);
   eta[j]         ~ gamma(a_eta, b_eta);
  }

  // subject loop and trial loop
  for (i in 1:N) {
    real TD;      // prediction error
    real mu;
    real sigma;
    vector[4] w;
    vector[4] theta_mu;
    vector[4] theta_sigma;
    
    vector[4] state;
    vector[4] state_prime;
    state = initV;
    state[1] = 2;
    state[2] = outcome[i, 1];
    state[3] = (state[1])^2;
    state[4] = (state[2])^2;
    w = initV;
    theta_mu = initV;
    theta_sigma = initV;
    // print("N: ", i);
    for (t in 3:(Tsubj[i]-1)) {
      //Find mu and sigma
      mu = inv_logit(dot_product(theta_mu, state))*10;
      sigma = inv_logit(dot_product(theta_sigma, state))*10+0.0001;
      // print("state: ", state);
      // print("mu: ", mu);
      // print("sigma: ", sigma);
      // print("dot product mu: ", dot_product(theta_mu, state));
      // print("dot product sigma: ", dot_product(theta_sigma, state));
      // compute action probabilities
      // Only normal distribution
      //choice[i, t] ~ normal(mu, sigma);
      // Zero-inflated normal
      // target += log1m(lambda[i]) + normal_lpdf(choice[i, t] | mu, sigma);
      // if (choice[i, t] == 0)
      //   target += log(lambda[i]);
      // Mixture of two normals
      target += log_mix(lambda[i], normal_lpdf(choice[i, t] | 0, eta[i]), normal_lpdf(choice[i, t] | mu, sigma));


      state_prime[1] = state[1] + 1;
      state_prime[2] = (state[2]*state[1] + outcome[i, t])/state_prime[1];
      state_prime[3] = (state_prime[1])^2;
      state_prime[4] = (state_prime[2])^2;

      // temporal difference
      // R + gamma*v(S',w) - v(S,w)
      // assume gamma = 1
      TD = (outcome[i, t] - cost[i]*(choice[i, t])) + dot_product(w, state_prime) - dot_product(w, state);
      // print("TD: ", TD);
      // Update w:
      w += alpha_w[i] * TD * state;
      // Update theta
      // print("alpha theta: ",alpha_theta[i]);
      // print("alpha w: ",alpha_w[i]);
      // print("Theta mu: ", theta_mu);
      // print("Theta sigma: ", theta_sigma);
      // print("w: ", w);
      // print("cost: ", cost[i]);
      // print(" ");
      theta_mu += alpha_theta[i] * TD * 1/(sigma)^2 * (choice[i, t] - mu) * state;
      theta_sigma += alpha_theta[i] * TD * ((choice[i, t] - mu)^2/(sigma)^2 - 1) * state;
      
      state = state_prime;
    }
    //Find mu and sigma
    mu = inv_logit(dot_product(theta_mu, state))*10;
    sigma = inv_logit(dot_product(theta_sigma, state))*10+0.0001;
    
    // compute action probabilities
    target += log_mix(lambda[i], normal_lpdf(choice[i, Tsubj[i]] | 0, eta[i]), normal_lpdf(choice[i, Tsubj[i]] | mu, sigma));
    // temporal difference
    // R + gamma*v(S',w) - v(S,w)
    // assume gamma = 1
    TD = (outcome[i, Tsubj[i]] - cost[i]*(choice[i, Tsubj[i]])) - dot_product(w, state);
    
    // Update w:
    w += alpha_w[i] * TD * state;
    // Update theta
    theta_mu    += alpha_theta[i] * TD * 1/(sigma)^2 * (choice[i, Tsubj[i]] - mu) * state;
    theta_sigma += alpha_theta[i] * TD * ((choice[i, Tsubj[i]] - mu)^2/(sigma)^2 - 1) * state;
  }
}