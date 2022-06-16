functions{
  real l_hood(vector x, real sigma, int n, int k){
    real likelihood = 0;
    for(j in 1 : (n * k)){
      likelihood += normal_lpdf(x[j] | 0 ,sigma);
    }
    return likelihood;
  }
}
data {
  int n;
  int k;
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
  // target += uniform_lpdf(intercept | l, u);
  target += uniform_lpdf(sigma | 0, 1);
  target += l_hood(y - x * theta, sigma, n, k); // log-likelihood
}
