data {
  int <lower=1> N;
  vector[N] t;
  vector[N] q;
  vector[N] Q;
  real <lower=0> maxrate;
  real <lower=0> cumtodate;
  real <lower=0> qf;
  real <lower=0> t_prior;
  real <lower=0> Q_prior;
}
parameters {
  //real <lower=0> qi;
  //real <lower=0.4, upper=3.0> a;
  //real <lower=0.9, upper=1.3> m;
  real <lower=0.0, upper=10.0> a;
  real <lower=0.0, upper=3.0> m;
  //real <lower=0.0> a;
  //real <lower=0.0> m;
  //real qinf;
  //real <lower=0> s2;
  real <lower=0> s3;
}
transformed parameters {
  //vector[N] Qhat;
  vector[N] T_hat;
  vector[N] t_hat;
  //vector[N] tam;
  for (i in 1:N) {
    //tam[i] = (t[i]^(-m))*exp((a/(1.0-m))*((t[i]^(1.0-m))-1.0));
    //if (m==1) {
    //  tam[i] = t[i]^(a-1.0);
    //}
    //T_hat[i] = log(q[i]/Q[i]);
    //if(i==1) {
    //  T_hat[i] = log10((Q[i]-0)/Q[i]);
    //}
    //if(i>1) {
    //  T_hat[i] = log10((Q[i]-Q[i-1])/Q[i]);
    //}
    T_hat[i] = log(q[i]/Q[i]);
    //t_hat[i] = a*(t[i]^(-m));
    //Qhat[i] = qi*tam[i]/(a*(t[i]^(-m)));
    t_hat[i] = (-m)*log(t[i]) + log(a);
    //t_hat[i] = (-m)*log10(i-0.5) + log10(a);
  }
}
model {
  //vector[N] qhat;
  //qi ~ uniform(0,2*maxrate);
  a ~ uniform(0.0,4.0);
  m ~ uniform(0.0,2.0);
  //qinf ~ uniform(-maxrate/2,maxrate/2);
  //s2 ~ uniform(0,1e5);
  s3 ~ uniform(0,1e5);
  //qhat = qi*tam + qinf;
  //q ~ normal(qhat,sqrt(s2));
  //T_hat ~ normal(log(a)+(-m)*log(t),sqrt(s3));
  T_hat ~ normal(t_hat,sqrt(s3));
  //Q ~ normal(Qhat, sqrt(s3));
  //q ~ normal(qi*tam + qinf,sqrt(s2));
}
generated quantities {
  //real EUR;
  //vector[600] t_star;
  //vector[600] tam_star;
  //vector[600] q_star;
  //vector[600] Q_star;
  //EUR = cumtodate;
  //for(i in 1:600) {
  //  t_star[i] = 30.4375*i;
  //  tam_star[i] = (t_star[i]^(-m))*exp((a/(1.0-m))*((t_star[i]^(1.0-m))-1.0));
  //  //if (m==1) {
  //  //  tam_star[i] = t_star[i]^(a-1.0);
  //  //}
  //  q_star[i] = qi*tam_star[i] + qinf;
  //  //Q_star[i] = (qi/a)*exp((a/(1.0-m))*(t_star[i]^(1.0-m)-1.0));
  //  //Q_star[i] = (qi*tam_star[i]/(a*t_star[i]^(-m)));
  //  if (q_star[i] > qf) {
  //    //EUR = fmax(Q_star[i], Q_prior);
  //    //EUR = Q_star[i];
  //    EUR = (q_star[i]/a)*t_star[i]^m;
  //  }
  //}
}
