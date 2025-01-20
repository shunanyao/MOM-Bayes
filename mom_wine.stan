functions {
  real median(array[] real x){
	  int k = size(x);
	  real med;
	  array[k] real x_asc = sort_asc(x);
	  if(k % 2 == 1){
  		med = x_asc[(k + 1) / 2];
		}else{
	  	med = (x_asc[k / 2] + x_asc[k / 2 + 1]) / 2.0;
		}
		return med;
	}
  real huber_gradient(array[] real likelihood, real mom, int k, int n, real mad, real a){
    real gradient = 0;
    real n_sqrt = sqrt(n);
    for(j in 1 : k){
      if(abs((likelihood[j] - mom) * n_sqrt / mad) < a){
        gradient += - n_sqrt * (likelihood[j] - mom) / mad * n_sqrt / mad;
      }else if((n_sqrt * (likelihood[j] - mom) / mad) > a){
        gradient += - a * n_sqrt / mad;
      }else{
        gradient += a * n_sqrt / mad;
      }
    }
    return gradient;
  }
  real mom_estimator(int k, int n, vector x, vector initial_x, real sigma, real initial_sigma, real a){
    array[k] real log_likelihood;
    array[k] real log_likelihood_sort;
    array[k] real log_likelihood_abs;
		array[k] real log_likelihood_dev;
    real mom;
    real new_mom;
    real new_gradient;
    real mad;
    real gradient;
    real increment;
    for(j in 1 : k){
      log_likelihood[j] = 0;
      for (i in ((j - 1) * n + 1) : (j * n)){
        log_likelihood[j] += (normal_lpdf(x[i] | 0, sigma) - normal_lpdf(initial_x[i] | 0, initial_sigma))/ n; 
      }
    }
    mom = median(log_likelihood);
    for(j in 1 : k){
		  log_likelihood_dev[j] = log_likelihood[j] - mom;
		}
		log_likelihood_abs = abs(log_likelihood_dev);
    mad = median(log_likelihood_abs) / 0.6745;
    gradient = huber_gradient(log_likelihood, mom, k, n, mad, a);
    while(abs(gradient) > 0.005){
      increment =  gradient;
      new_mom = mom - increment;
      new_gradient = huber_gradient(log_likelihood, new_mom, k, n, mad, a);
      while(new_gradient * gradient < 0){
        increment = 0.5 * increment;
        new_mom = mom - increment;
        new_gradient = huber_gradient(log_likelihood, new_mom, k, n, mad, a);
      }
      mom = new_mom;
      gradient = huber_gradient(log_likelihood, mom, k, n, mad, a);
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
  // vector[n * k] x;
  vector[n * k] y;
  vector[dim] initial_theta;
  real initial_sigma;
}
parameters {
  vector[dim] theta;
  real sigma;
}
model {
  for(j in 1 : dim){
    target += normal_lpdf(theta[j] | 0, 10);
  }
  // target += gamma_lpdf(abs(sigma) | 2, sqrt(2));
  target += uniform_lpdf(sigma | 0, 1);
  target += mom_estimator(k, n, y - x * theta, y - x * initial_theta, sigma, initial_sigma, a); // log-likelihood
  // target += normal_lpdf(y - x * theta | 0, abs(sigma));
}
