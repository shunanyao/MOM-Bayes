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
	real mom_estimator(int k, int n, vector x, vector rt, real p_task, real initial_p_task,
                     real alpha, real initial_alpha, real beta, real initial_beta,
                     real gamma, real initial_gamma, real sigma, real initial_sigma,
                     real sigma2, real initial_sigma2, real a){
		int begin_this;
		int end_this;
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
			begin_this = (j - 1) * n + 1;
			end_this = j * n;
			for (i in begin_this : end_this){
				log_likelihood[j] += (log_likelihhod_mixture(x[i], rt[i], p_task, alpha, beta, sigma, sigma2, gamma) - 
				                      log_likelihhod_mixture(x[i], rt[i], initial_p_task, initial_alpha, initial_beta, initial_sigma, initial_sigma2, initial_gamma)) / n;
			}
		}
		mom = median(log_likelihood);
		log_likelihood_sort = sort_asc(log_likelihood);
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
	real huber_gradient(array[] real likelihood, real mom, int k, int n, real mad, real a){
    real gradient = 0;
    real n_sqrt = sqrt(n);
    for(j in 1 : k){
      if(abs((likelihood[j] - mom) * n_sqrt / mad) < a){
        gradient += - n_sqrt * (likelihood[j] - mom) / mad;
      }else if((n_sqrt * (likelihood[j] - mom) / mad) > a){
        gradient += - a ;
      }else{
        gradient += a;
      }
    }
    return gradient;
  }
	real log_likelihhod_mixture(real x, real rt, real p_task, real alpha, real beta, real sigma, real sigma2, real gamma){
	  real log_likelihood;
	  log_likelihood = log_sum_exp(log(p_task) + lognormal_lpdf(rt | alpha + x * beta, sigma),
	                               log1m(p_task) + lognormal_lpdf(rt | gamma, sigma2));
	  return log_likelihood;
	}
}
data {
  int k;
  int n;
  vector[k*n] x;
  vector[k*n] rt;
  real a;
  real sigma;
  real sigma2;
  real initial_alpha;
  real initial_beta;
  real initial_sigma;
  real initial_gamma;
  real initial_sigma2;
  real initial_p_task;
}
parameters {
  real alpha;
  real beta;
  // real<lower = 0> sigma;
  real<upper = alpha> gamma;
  // real<lower = 0> sigma2;
  real<lower = 0, upper = 1> p_task;
}
model {
  target += normal_lpdf(alpha | 6, 1);
  target += normal_lpdf(beta | 0, .3);
  // target += normal_lpdf(sigma | .5, .2)
  //   - normal_lccdf(0 | .5, .2);
  target += normal_lpdf(gamma | 6, 1) -
    normal_lcdf(alpha | 6, 1);
  // target += normal_lpdf(sigma2 | .5, .2)
  //   - normal_lccdf(0 | .5, .2);
  target += beta_lpdf(p_task | 8, 2);
  // target += mom_estimator(k, n, x, rt, p_task, initial_p_task,
  //                         alpha, initial_alpha, beta, initial_beta,
  //                         gamma, initial_gamma, sigma, initial_sigma, sigma2, initial_sigma2, a);
  for(j in 1:k*n)
    target +=
      log_sum_exp(log(p_task) +
                  lognormal_lpdf(rt[j] | alpha + x[j] * beta, sigma),
                  log1m(p_task) +
                  lognormal_lpdf(rt[j] | gamma, sigma2)) ;
}