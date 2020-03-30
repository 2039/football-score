// https://discourse.mc-stan.org/t/hierarchical-bayesian-poisson-regression-model/10165/5

data {
    int<lower=2> N; // Number of teams
    int<lower=0> home_scores[N, N];
    int<lower=0> away_scores[N, N];
}

transformed data {
    // Constant
    vector[2] Mu = [0, 0]'; // invalid syntax
}

parameters {
    //matrix[2, N] AB;
    vector[N] alpha;
    vector[N] beta;

    real<lower=0> gamma;
    real<lower=0> mu;

    cov_matrix[2] Sigma;
}

transformed parameters {
    matrix[N, N] llambda_home;
    matrix[N, N] llambda_away;

    for (i in 1:N) { for (j in 1:N) {
        if (i==j) continue;

        //llambda_home[i, j] = AB[1, i] - AB[2, j] + gamma + mu;
        //llambda_away[i, j] = AB[1, j] - AB[2, i] - gamma + mu;
        llambda_home[i, j] = alpha[i] - beta[j] + gamma + mu;
        llambda_away[i, j] = alpha[j] - beta[i] - gamma + mu;
    }}
}

model {
    for (i in 1:N) { for (j in 1:N) {
        if (i==j) continue;

        target += poisson_log_lpmf( home_scores[i, j] | llambda_home[i, j] );
        target += poisson_log_lpmf( away_scores[i, j] | llambda_away[i, j] );
    }}

    for (i in 1:N) {
        // target += multi_normal_lpdf( col(AB, i) | [0, 0], Sigma );
        target += multi_normal_lpdf( [alpha[i], beta[i]] | Mu, Sigma );
    }
}
