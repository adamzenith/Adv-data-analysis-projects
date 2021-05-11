#include <TMB.hpp>                                // Links in the TMB libraries

template<class Type>
  Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(clo);                                 // Data vector transmitted from R
  DATA_VECTOR(sex);                                 // Data vector transmitted from R
  DATA_VECTOR(subjId);                              // Data vector transmitted from R
  DATA_VECTOR(day);
  DATA_VECTOR(subDay);
  
  PARAMETER_VECTOR(Id);                             // Random effects
  PARAMETER_VECTOR(sexID);
  PARAMETER_VECTOR(u);
  PARAMETER_VECTOR(v);
  PARAMETER_VECTOR(gamma);
  // Parameters
  PARAMETER_VECTOR(mu);                          // Parameter value transmitted from R
  PARAMETER(sigma);                              // Parameter value transmitted from R
  PARAMETER(sigmau);
  PARAMETER(sigmav);
  PARAMETER(sigmaG);
  PARAMETER(alpham);
  
  
  int alphaf = 1; 
  int nopers = Id.size();
  //int no.pers = subjId.size();
  int nosubDay = 136;
  
  Type f = 0;                                      // Declare the "objective function" (neg. log. likelihood)
  
  //Type gamma = dnorm(Id, 0, sigmaG, true);
   
  //Randoms effects 
  for(int i =0; i < nopers; i++){
    if(sexID[i] == 1 ){
      Type alpha = alphaf;
    }
    else{
      Type alpha = alpham;
    }
  
    f -= dnorm(u[i], 0, sigmau*alpha*exp(-gamma[i]), true);
    
    for(int j =0; j < nosubDay; j++){
      f -= dnorm(v[j], 0, sigmav*alpha*exp(-gamma[i]), true);
    }
  }
  
  //Posterior distribution (conditional expected)
  for(int i =0; i < clo.size(); i++){
    if(sex[i] == "female" ){
      Type alpha = alphaf;
    }
    else{
      Type alpha = alpham;
    }
  
    f -= dnorm(clo[i], mu[i] + u[i] + v[i], sigma*alpha*exp(-gamma[i]), true); //
  }

  //Randoms effects  
  //for(int i =0; i < nopers; i++){
  //  if(sexID[i] == 1 ){
  //    Type alpha = alphaf;
  //  }
  //  else{
  //    Type alpha = alpham;
      //  }
  
  //  f -= dnorm(subjId[i], 0, sigmau*alpha*exp(-gamma[i]), true);
  //  
  //  for(int j =0; j < nosubDay; j++){
  //    f -= dnorm(subDay[j], 0, sigmav*alpha*exp(-gamma[i]), true);
      //  }
      // }
// 
  //Posterior distribution (conditional expected)
  //for(int i =0; i < clo.size(); i++){
  //  if(sex[i] == "female" ){
  //    Type alpha = alphaf;
  //  }
  //  else{
      //    Type alpha = alpham;
      //  }
    
    //  f -= dnorm(clo[i], mu[i], sigma*alpha*exp(-gamma[i]), true);
    //}
  
  return f;
  }
