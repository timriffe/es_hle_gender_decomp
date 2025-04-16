
# a wrapper to do the decomposition, here with 
# initial conditions hard-coded to 100% healthy.
# this can be relaxed
do_dec <- function(data, init, expectancy ="h", interval = 2){
  tr <- 
    data |> 
    pivot_wider(names_from = sex, values_from = prob) |> 
    mutate(p = (female + male) / 2,
           delta = female - male) 
  trd <- tr |> 
    select(age, transition, p)
  s2t(data = trd,
      expectancy = "h",
      interval = interval) |> 
    mutate(age = age + 50)
    left_join(tr, sen, by = join_by(age, transition)) |> 
    filter(!is.na(effect)) |> 
    mutate(dec = delta * effect)
}


get_dxs <- function(tr, init = c(H=1,U=0)){
  tr <- tr |> 
    pivot_wider(names_from = transition, 
                values_from = prob)
  age <- tr$age
  HH  <- tr$HH
  HU  <- tr$HU
  UU  <- tr$UU
  UH  <- tr$UH
  
  lh   <- rep(0,length(age))
  lu   <- rep(0,length(age))
  lh[1] <- init["H"]
  lu[1] <- init["U"]
  for (i in 2:length(age)){
    lh[i] <- lh[i-1] * HH[i-1] + lu[i-1] * HU[i-1]
    lu[i] <- lh[i-1] * HU[i-1] + lu[i-1] * UU[i-1]
  }
  
  tr |> 
    mutate(lh = lh,
           lu = lu,
           dxh = lh * HD,
           dxu = lu * UD) |> 
    select(age, dxh, dxu)
}
