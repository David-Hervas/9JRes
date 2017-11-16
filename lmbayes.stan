
    data {
    int N; //the number of observations
    int K; //the number of columns in the model matrix
    real y[N]; //the response
    matrix[N,K] X; //the model matrix
    }
    parameters {
    vector[K] beta; //the regression parameters
    real sigma; //the standard deviation
    }
    transformed parameters {
    vector[N] linpred;
    linpred = X*beta;
    }
    model {  
    beta[1] ~ uniform(-500, 500); 
    
    for(i in 2:K)
    beta[i] ~ uniform(-50, 50);//prior for the slopes following Gelman 2008
    
    y ~ normal(linpred,sigma);
    }
    