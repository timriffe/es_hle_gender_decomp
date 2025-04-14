
# a wrapper to do the decomposition, here with 
# initial conditions hard-coded to 100% healthy.
# this can be relaxed
do_dec <- function(data, init, expectancy, interval){
  tr <- 
    data |> 
    pivot_wider(names_from = sex, values_from = prob) |> 
    mutate(p = (female + male) / 2,
           delta = female - male) 
  s2t(data = tr,
        init = c(H=1,U=0), 
        expectancy = "h", 
        interval = 2) |> 
    mutate(age = age + 50)
    left_join(tr, sen, by = join_by(age, transition)) |> 
    filter(!is.na(effect)) |> 
    mutate(dec = delta * effect)
}
