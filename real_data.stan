functions {
  real huber_gradient(real[] likelihood, real mom, int k, int n, real a){
    real gradient = 0;
    real n_sqrt = sqrt(n);
    for(j in 1 : k){
      if(fabs((likelihood[j] - mom) * n_sqrt) < a){
        gradient += - n_sqrt * (likelihood[j] - mom);
      }else if((n_sqrt * (likelihood[j] - mom)) > a){
        gradient += - a;
      }else{
        gradient += a;
      }
    }
    return gradient;
  }
  real mom_estimator(int k, int n, vector x, real sigma, real a){
    real log_likelihood[k];
    real log_likelihood_sort[k];
    real mom;
    real gradient;
    for(j in 1 : k){
      log_likelihood[j] = 0;
      for (i in ((j - 1) * n + 1) : (j * n)){
        log_likelihood[j] += (normal_lpdf(x[i] | 0, sigma) / n); 
      }
    }
    log_likelihood_sort = sort_asc(log_likelihood);
    mom = log_likelihood_sort[k / 2 + 1];
    gradient = 1;
    while(fabs(gradient) > 0.01){
      gradient = huber_gradient(log_likelihood, mom, k, n, a);
      mom += - 0.001 *  gradient;
    }
    mom = k * n * mom;
    return mom;
  }
}
data {
  real a;
  int k;
  int n;
  int dim;
  matrix[n * k, dim] x;
  vector[n * k] y;
}
parameters {
  vector[dim] theta;
  real sigma;
}
model {
  for(j in 1 : dim){
    target += normal_lpdf(theta[j] | 0, 10);
  }
  target += uniform_lpdf(sigma | 0, 1);
  target += mom_estimator(k, n, y - x * theta, sigma, a); // log-likelihood
}
