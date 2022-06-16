functions {
	real huber_loss(real[] likelihood, real mom, int k, int n, real a){
		real loss = 0;
		real n_sqrt = sqrt(n);
		for(j in 1 : k){
			if(abs((likelihood[j] - mom) * n_sqrt) < a){
				loss += n_sqrt * (likelihood[j] - mom);
			}else if((n_sqrt * (likelihood[j] - mom)) > a){
				loss += a;
			}else{
				loss += -a;
			}
		}
		return loss;
	}
	real binary_search(real[] likelihood, int k, int n, real a){
		real loss = 0;
		real likelihood_min = likelihood[1];
		real likelihood_max = likelihood[k];
		real mom = (likelihood_min + likelihood_max) / 2;
		real loss_med = huber_loss(likelihood, mom, k, n, a);
		while(abs(loss_med) > 0.001){
			if(loss_med > 0.001){
				likelihood_min = mom;
				mom = (likelihood_min + likelihood_max) / 2;
				loss_med = huber_loss(likelihood, mom, k, n, a);
			}else{
				likelihood_max = mom;
				mom = (likelihood_min + likelihood_max) / 2;
				loss_med = huber_loss(likelihood, mom, k, n, a);
			}
		}
		return mom;
	}
	real mom_estimator(int k, int n, real[] x, real theta, real a){
		real log_likelihood[k];
		real log_likelihood_sort[k];
		real mom;
		for(j in 1 : k){
			log_likelihood[j] = 0;
			for (i in ((j - 1) * n + 1) : (j * n)){
				log_likelihood[j] += (normal_lpdf(x[i] | theta, 1) / n); 
			}
		}
		log_likelihood_sort = sort_asc(log_likelihood);
		mom = binary_search(log_likelihood_sort, k, n, a);
		mom = k * n * mom;
		return mom;
	}
}
data {
	real a;
	int k;
	int n;
	real x[n * k];
	real mu_prior;
	real sigma_prior;
}
parameters {
	real theta;
}
model {
	target += normal_lpdf(theta | mu_prior, sigma_prior * sigma_prior);       // prior log-density
	target += mom_estimator(k, n, x, theta, a); // log-likelihood
}
