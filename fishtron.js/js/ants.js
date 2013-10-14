
var defaultOpts = {
  rho : 0.5 ,

};


function updateFeromon( oldTau , deltaTaus , opts ){
  return (1-opts.rho)*oldTau + sum(deltaTaus); // newTau
}


