library("rjags")

run_model <- function(df, mod_string, params, burn=2e4, iters=5e3, seed=1){
  set.seed(1)
  df_jags = as.list(df)
  model = jags.model(textConnection(mod_string), data=df_jags, n.chains=3)
  update(model, burn)
  
  mod1_sim = coda.samples(model=model, variable.names=params, n.iter=iters)
  return (list(mod=model, sim=mod1_sim))
}

run_mcmc <- function(mod_sim){
  return (as.mcmc(do.call(rbind, mod_sim)))
}

run_diagnostics <- function(mod_dict){
  plot(mod_dict$sim, ask=FALSE)
  
  gelman <- gelman.diag(mod_dict$sim)
  print (gelman)
  autocorr <- autocorr.diag(mod_dict$sim)
  print (autocorr)
  autocorr.plot(mod_dict$sim)
  effective_size <- effectiveSize(mod_dict$sim)
  print (effective_size)
  dic <- dic.samples(mod_dict$mod, n.iter=1e3)
  return (list(gelman=gelman, autocorr=autocorr, effective_size=effective_size, dic=dic))
}

run_all <- function(df, string, params){
  mod_dict = run_model(df, string, params)
  mod_dict = modifyList(mod_dict, run_diagnostics(mod_dict))
  mod_dict$csim = run_mcmc(mod_dict$sim)
  return (mod_dict)
}