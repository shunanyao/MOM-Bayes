functions {
	real mom_estimator(int k, int n, real[] x, real theta, real a){
		int begin_this;
		int end_this;
		real log_likelihood[k];
		real log_likelihood_sort[k];
		real mom;
		for(j in 1 : k){
			log_likelihood[j] = 0;
			begin_this = (j - 1) * n + 1;
			end_this = j * n;
			for (i in begin_this : end_this){
				log_likelihood[j] += (normal_lpdf(x[i] | theta, 1) / n); 
			}
		}
		log_likelihood_sort = sort_asc(log_likelihood);
		//mom = Huber_loss_solution(log_likelihood_sort, k, 10);
		if(k % 2 == 1){
			mom = log_likelihood_sort[(k + 1) / 2];
		}else{
			mom = (log_likelihood_sort[k / 2] + log_likelihood_sort[k / 2 + 1]) / 2.0;
		}
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
