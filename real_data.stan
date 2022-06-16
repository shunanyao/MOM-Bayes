// functions {
  // 	real huber_loss(real[] likelihood, real mom, int k, int n, real a){
    // 		real loss = 0;
    // 		real n_sqrt = sqrt(n);
    // 		for(j in 1 : k){
      // 			if(fabs((likelihood[j] - mom) * n_sqrt) < a){
        // 				loss += n_sqrt * (likelihood[j] - mom);
        // 			}else if((n_sqrt * (likelihood[j] - mom)) > a){
          // 				loss += a;
          // 			}else{
            // 				loss += -a;
            // 			}
      // 		}
    // 		return loss;
    // 	}
  // 	real binary_search(real[] likelihood, int k, int n, real a){
    // 		real likelihood_min = likelihood[1];
    // 		real likelihood_max = likelihood[k];
    // 		real mom = (likelihood_min + likelihood_max) / 2;
    // 		real loss_med = huber_loss(likelihood, mom, k, n, a);
    // 		while(fabs(loss_med) > 0.001){
      // 			if(loss_med > 0.001){
        // 				likelihood_min = mom;
        // 				mom = (likelihood_min + likelihood_max) / 2;
        // 				loss_med = huber_loss(likelihood, mom, k, n, a);
        // 			}else{
          // 				likelihood_max = mom;
          // 				mom = (likelihood_min + likelihood_max) / 2;
          // 				loss_med = huber_loss(likelihood, mom, k, n, a);
          // 			}
      // 		}
    // 		return mom;
    // 	}
  // 	real mom_estimator(int k, int n, vector x, real sigma, real a){
    // 		real log_likelihood[k];
    // 		real log_likelihood_sort[k];
    // 		real mom;
    // 		for(j in 1 : k){
      // 			log_likelihood[j] = 0;
      // 			for (i in ((j - 1) * n + 1) : (j * n)){
        // 				log_likelihood[j] += (normal_lpdf(x[i] | 0, sigma) / n); 
        // 			}
      // 		}
    // 		log_likelihood_sort = sort_asc(log_likelihood);
    // 		mom = binary_search(log_likelihood_sort, k, n, a);
    // 		mom = k * n * mom;
    // 		return mom;
    // 	}
  // 	/*real mom_estimator(int k, int n, int dim, real[,] x_data, vector theta, real a){
    // 		real log_likelihood[k];
    // 		real log_likelihood_sort[k];
    // 		real mom;
    // 		real x[n * k];
    // 		real this_x;
    // 		for(j in 1 : (n * k)){
      // //			this_x = x_data[j, dim] - theta[1];
      //       this_x = x_data[j, dim];
      // 			for(i in 1 : (dim - 1)){
        // 				this_x += (- theta[i] * x_data[j, i - 1]); 
        // 			}
      // 			x[j] = this_x;
      // 		}
    // 		for(j in 1 : k){
      // 			log_likelihood[j] = 0;
      // 			for (i in ((j - 1) * n + 1) : (j * n)){
        // 				log_likelihood[j] += (normal_lpdf(x[i] | 0, 0.8017645) / n); 
        // 			}
      // 		}
    // 		log_likelihood_sort = sort_asc(log_likelihood);
    // 		mom = binary_search(log_likelihood_sort, k, n, a);
    // 		mom = k * n * mom;
    // 		return mom;
    // 	}*/
    // }
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
  // vector[n * k] x;
  vector[n * k] y;
  // vector[dim] mu_prior;
  // real l;
  // real u;
  // matrix<lower = 0>[dim,dim] sigma_prior;
}
parameters {
  vector[dim] theta;
  // real intercept;
  // real theta;
  real sigma;
  // real theta;
}
model {
  // target += multi_normal_lpdf(theta | mu_prior, sigma_prior);       // prior log-density
  for(j in 1 : dim){
    target += normal_lpdf(theta[j] | 0, 10);
  }
  // target += uniform_lpdf(intercept | l, u);
  target += uniform_lpdf(sigma | 0, 1);
  // target += normal_lpdf(theta | 0, 1);
  target += mom_estimator(k, n, y - x * theta, sigma, a); // log-likelihood
  // target += mom_estimator(k, n, y - theta, sigma, a);
}
