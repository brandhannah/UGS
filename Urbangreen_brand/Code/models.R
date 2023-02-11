# Estimate a conditional logit model

## Model 1: Basic Model [name: modelbasic]

apollo_initialise()

modelOutput_settings = list(printPVal=T)

### Set core controls
apollo_control = list(
  modelName  ="Simulated Data",
  modelDescr ="Simple MNL model",
  indivID    ="id"
)


apollo_beta=c(b_alt1 =0,
              b_alt2 =0,
              b_Erreichbarkeit =0,
              b_Miete =0,
              b_Naturnähe =0 )

### keine Parameter fix halten
apollo_fixed = c()

### validieren
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities (later integrated in mnl_settings below)
  V = list()
  V[['alt1']] = b_alt1 + b_Erreichbarkeit*Erreichbarkeit1 + b_Miete*Miete1 + b_Naturnähe*Naturnähe1
  V[['alt2']] = b_alt2 + b_Erreichbarkeit*Erreichbarkeit2 + b_Miete*Miete2 + b_Naturnähe*Naturnähe2
  V[['alt3']] = b_Erreichbarkeit*Erreichbarkeit3 + b_Miete*Miete3 + b_Naturnähe*Naturnähe3
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3) ,
    avail         = 1, # all alternatives are available in every choice
    choiceVar     = choice,
    V             = V  # tell function to use list vector defined above
    
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws - nur bei Mixed Logit!
  ### P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

modelbasic = apollo_estimate(apollo_beta, apollo_fixed,
                             apollo_probabilities, apollo_inputs, 
                             estimate_settings=list(hessianRoutine="maxLik"))

## Printing

library(kableExtra)
kable(apollo_modelOutput(modelbasic, modelOutput_settings = list(printPVal=T)), digits = 3) %>% kable_styling()

#################



## Model 2: Model with interaction terms "Income Present and Healthcondition" (Median Centered) [name: modelinter1]

apollo_initialise()

modelOutput_settings = list(printPVal=T)

### Set core controls
apollo_control = list(
  modelName  ="Urban Green Spaces",
  modelDescr ="UBS Logit model",
  indivID    ="id"
)


apollo_beta=c(b_alt1 =0,
              b_alt2 =0,
              b_Erreichbarkeit =0,
              b_Miete =0,
              b_Naturnähe =0,
              b_HealthMC = 0,
              b_IncomeMC = 0  )

### keine Parameter fix halten
apollo_fixed = c()

### validieren
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities (later integrated in mnl_settings below)
  V = list()
  V[['alt1']] = b_alt1 + b_Erreichbarkeit*Erreichbarkeit1 + b_Miete*Miete1 + b_Naturnähe*Naturnähe1 + b_HealthMC*HealthMC*Erreichbarkeit1 + b_IncomeMC*IncomeMC*Erreichbarkeit1
  V[['alt2']] = b_alt2 + b_Erreichbarkeit*Erreichbarkeit2 + b_Miete*Miete2 + b_Naturnähe*Naturnähe2 + b_HealthMC*HealthMC*Erreichbarkeit2 + b_IncomeMC*IncomeMC*Erreichbarkeit2
  V[['alt3']] = b_Erreichbarkeit*Erreichbarkeit3 + b_Miete*Miete3 + b_Naturnähe*Naturnähe3 + b_HealthMC*HealthMC*Erreichbarkeit3 + b_IncomeMC*IncomeMC*Erreichbarkeit3
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3) ,
    avail         = 1, # all alternatives are available in every choice
    choiceVar     = choice,
    V             = V  # tell function to use list vector defined above
    
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws - nur bei Mixed Logit!
  ### P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

modelinter1 = apollo_estimate(apollo_beta, apollo_fixed,
                               apollo_probabilities, apollo_inputs, 
                               estimate_settings=list(hessianRoutine="analytic"))


## Printing
library(kableExtra)
kable(apollo_modelOutput(modelinter1, modelOutput_settings = list(printPVal=T)), digits = 3) %>% kable_styling()

########

## Model 3: Model with Interactions "Garden, Kleingarten and GreenBackyard" [name: modelinter2]

apollo_initialise()

modelOutput_settings = list(printPVal=T)

### Set core controls
apollo_control = list(
  modelName  ="Simulated Data",
  modelDescr ="Simple MNL model",
  indivID    ="id"
)


apollo_beta=c(b_alt1 =0,
              b_alt2 =0,
              b_Erreichbarkeit =0,
              b_Miete =0,
              b_Naturnähe =0,
              b_Garden =0,
              b_Kleingarten =0,
              b_Balcony = 0 )

### keine Parameter fix halten
apollo_fixed = c()

### validieren
apollo_inputs = apollo_validateInputs()

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities (later integrated in mnl_settings below)
  V = list()
  V[['alt1']] = b_alt1 + b_Erreichbarkeit*Erreichbarkeit1 + b_Miete*Miete1 + b_Naturnähe*Naturnähe1 + b_Garden*Garden*Naturnähe1 + b_Kleingarten*Kleingarten*Naturnähe1 + b_Balcony*Balcony*Naturnähe1 
  V[['alt2']] = b_alt2 + b_Erreichbarkeit*Erreichbarkeit2 + b_Miete*Miete2 + b_Naturnähe*Naturnähe2 + b_Garden*Garden*Naturnähe2 +  b_Kleingarten*Kleingarten*Naturnähe2 + b_Balcony*Balcony*Naturnähe2
  V[['alt3']] = b_Erreichbarkeit*Erreichbarkeit3 + b_Miete*Miete3 + b_Naturnähe*Naturnähe3 + b_Garden*Garden*Naturnähe3 +  b_Kleingarten*Kleingarten*Naturnähe3 + b_Balcony*Balcony*Naturnähe3
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3) ,
    avail         = 1, # all alternatives are available in every choice
    choiceVar     = choice,
    V             = V  # tell function to use list vector defined above
    
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws - nur bei Mixed Logit!
  ### P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

modelinter2 = apollo_estimate(apollo_beta, apollo_fixed,
                             apollo_probabilities, apollo_inputs, 
                             estimate_settings=list(hessianRoutine="maxLik"))

library(kableExtra)
kable(apollo_modelOutput(modelinter2, modelOutput_settings = list(printPVal=T)), digits = 3) %>% kable_styling()