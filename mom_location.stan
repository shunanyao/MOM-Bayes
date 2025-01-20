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
	real mom_estimator(int k, int n, array[] real x, real theta, real initial_theta, real a){
		int begin_this;
		int end_this;
		array[k] real log_likelihood;
		array[k] real log_likelihood_abs;
		array[k] real log_likelihood_dev;
		real mom;
		real mad;
		real gradient;
		real increment;
		real new_mom;
		real new_gradient;
		for(j in 1 : k){
			log_likelihood[j] = 0;
			begin_this = (j - 1) * n + 1;
			end_this = j * n;
			for (i in begin_this : end_this){
				log_likelihood[j] += (normal_lpdf(x[i] | theta, 1)) / n;
			}
		}
		mom = median(log_likelihood);
// 		for(j in 1 : k){
// 		  log_likelihood_dev[j] = log_likelihood[j] - mom;
// 		}
// 		log_likelihood_abs = abs(log_likelihood_dev);
//     mad = median(log_likelihood_abs) / 0.6745;
    mad = 1;
    gradient = huber_gradient(log_likelihood, mom, k, n, mad, a);
    while(abs(gradient) > 0.005){
      increment =  gradient;
      new_mom = mom - increment;
      new_gradient = huber_gradient(log_likelihood, new_mom, k, n, mad, a);
      while(new_gradient * gradient <= 0){
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
	real huber_gradient(array[] real likelihood, real mom, int k, int n, real mad, real a){
    real gradient = 0;
    real n_sqrt = sqrt(n);
    real increment;
    for(j in 1 : k){
      if(abs((likelihood[j] - mom) * n_sqrt / mad) < a){
        increment =  - n_sqrt * (likelihood[j] - mom) / mad * n_sqrt / mad;
      }else if((n_sqrt * (likelihood[j] - mom) / mad) > a){
        increment = - a * n_sqrt / mad;
      }else{
        increment =  a * n_sqrt / mad;
      }
      gradient += increment;
    }
    return gradient;
  }
}

data {
	real a;
	int k;
	int n;
	array[n*k] real x;
	real mu_prior;
	real sigma_prior;
	real initial_theta;
}
parameters {
	real theta;
}
model {
	target += normal_lpdf(theta | mu_prior, sigma_prior);
	target += mom_estimator(k, n, x, theta, initial_theta, a); // log-likelihood
	// target += normal_lpdf(x | theta, 1) - normal_lpdf(x | initial_theta, 1);
}
generated quantities{
  real likelihood;
  real likelihood_correct;
  likelihood = mom_estimator(k, n, x, theta, initial_theta, a);
  likelihood_correct = mom_estimator(k, n, x, -30, initial_theta, a);
  // likelihood = normal_lpdf(x | theta, 1) - normal_lpdf(x | initial_theta, 1);
}