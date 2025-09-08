
library(data.table)
library(tidyverse)
library(collapse) # for fsubset(), fast subsetting
library(tictoc)
library(tidyfast)

# From Results section:
# We compute the initial conditions at age 50 (ℓ_i(0), where 0 here means age 50)
# assuming that the probabilities we have for age 50 were constant in younger ages.
init_constant <- function(x){
  u <- matrix(x[c("HH","UH","HU","UU")] %>% unlist(),2,byrow=TRUE)
  v <- eigen(u)$vectors[,1]
  init <- v / sum(v)
  names(init) <- c("H","U")
  init
}



pi_block <- function(p, state_from, state_to, age, interval = 1) {
  
  state_fromi  <- state_from[1]
  state_toi    <- state_to[1]
  age          <- c(age, max(age) + interval)
  P            <- diag(p)
  P            <- cbind(rbind(0, P), 0)
  from_names   <- paste(state_fromi, age, sep = "_")
  to_names     <- paste(state_toi,   age, sep = "_")
  dimnames(P)  <- list(to_names, from_names)
  return(P)
}

pi_block_outer <- function(chunk, interval = 1) {
  
  pi_block(chunk[["p"]] |>
             as.double(),
           chunk[["from"]],
           chunk[["to"]],
           chunk[["age"]],
           interval = interval) |>
    as_tibble()
}

Ptibble2U <- function(Ptibble, interval = 1) {
  
  age <- Ptibble[["age"]] |>
    unique() |>
    sort()
  
  age <- c(age, age[length(age)] + interval)
  pre <- Ptibble |>
    dt_pivot_longer(-age, 
                    names_to  = "fromto", 
                    values_to = "p") |>
    fmutate(from = substr(fromto, 0, 1),
            to   = substr(fromto, 2, 2)) |>
    fselect(-fromto) |>
    fsubset(to != "D")
  
  states <- pre$from |>
    unique()
  
  pre |>
    group_by(from, to) |>
    nest() |>
    fmutate(data = map(data, ~ .x |>
                         pi_block_outer(interval = interval))) |> 
    dt_pivot_wider(names_from  = from, 
                   values_from = data) |> 
    unnest(cols = all_of(states),
           names_sep = "") |> 
    ungroup() |> 
    fmutate(to = paste(rep(states, each = length(age)), 
                       rep(age,    length(states)), # each
                       sep = "_")) |> 
    column_to_rownames("to") |> 
    as.matrix()
}

# 24-07-2024, updated to add interval arg
Ptibble2N <- function(Ptibble, discount = FALSE, interval = 1) {
  
  U <- Ptibble2U(Ptibble)
  I <- diag(rep(1, nrow(U)))
  N <- solve(I - U) * interval
  if (discount) {
    # untested
    N <- N - (I * interval) / 2
  }
  return(N)
}

# -----------------------
library(tidyverse)
library(expm)

# creates a submatrix for a given transition,
# meant to be composed into U
pi2u <- function(pivec, from ="H",to = "H",start_age = 50,interval = 1) {
  out           <- cbind(rbind(0, diag(pivec)), 0)
  n             <- length(pivec)
  ages          <- ((0:n) * interval) + start_age
  from_names    <- paste(from, ages, sep = "::")
  to_names      <- paste(to, ages, sep = "::")
  dimnames(out) <- list(to_names, from_names)
  out
}


# composes U from a set of submatrices
u2U_closed <- function(HH, HU, UH, UU){
  out <- rbind(
    cbind(HH, UH),
    cbind(HU, UU))
  
  out <- cbind(rbind(out, 1 - colSums(out)),0)
  colnames(out)[ncol(out)] <- "D::Inf"
  rownames(out)[nrow(out)] <- "D::Inf"
  out[nrow(out),ncol(out)] <- 1
  out
}

Ptibble2U_closed <- function(Ptibble, interval = 1, start_age = 50){
  n <- nrow(Ptibble) + 1
  HH <- Ptibble %>% 
    pull(HH) %>% 
    pi2u("H","H", start_age = start_age, interval = interval) %>% 
    '['(-1,-n) # hard coded for this example. 
  # Could be dealt with in pi2u() more generally
  HU <- Ptibble %>% 
    pull(HU) %>% 
    pi2u("H","U", start_age = start_age, interval = interval) %>% 
    '['(-1,-n)
  UU <- Ptibble %>% 
    pull(UU) %>% 
    pi2u("U","U", start_age = start_age, interval = interval) %>% 
    '['(-1,-n)
  UH <- Ptibble %>% 
    pull(UH) %>% 
    pi2u("U","H", start_age = start_age, interval = interval) %>% 
    '['(-1,-n)
  
  U <- u2U_closed(HH, HU, UH, UU)
  U
}

# avoid needing to make submatrices manually,
# takes care of upper left corner minor detail that
# caused the singularity error
Ptibble2U_closed <- function(Ptibble, interval = 1, start_age = 50){
  n <- nrow(Ptibble) + 1
  HH <- Ptibble %>% 
    pull(HH) %>% 
    pi2u("H","H", start_age = start_age, interval = interval) %>% 
    '['(-1,-n) # hard coded for this example. 
  # Could be dealt with in pi2u() more generally
  HU <- Ptibble %>% 
    pull(HU) %>% 
    pi2u("H","U", start_age = start_age, interval = interval) %>% 
    '['(-1,-n)
  UU <- Ptibble %>% 
    pull(UU) %>% 
    pi2u("U","U", start_age = start_age, interval = interval) %>% 
    '['(-1,-n)
  UH <- Ptibble %>% 
    pull(UH) %>% 
    pi2u("U","H", start_age = start_age, interval = interval) %>% 
    '['(-1,-n)
  
  U <- u2U_closed(HH, HU, UH, UU)
  U
}

# U is the standard transient matrix with probabilities, 
# in demgoraphers' orientation
U2Q <- function(U, interval_current = 1){
  Q <-
    U |>
    t() |> 
    expm::logm() |>
    zapsmall() %>%
    # discount for interval!
    '/'(interval_current)
  dimnames(Q) <- U |> t() |> dimnames()
  Q
}
logm_small <- function(U, age=50, interval_current = 2){ 
  from <- paste(c("H","U","D"),c(age,age,"Inf"),sep="::")
  to   <- paste(c("H","U","D"),c(age+interval_current,age+interval_current,"Inf"),sep="::")
  U_small <- U[to,from] |> t()
  Q    <- logm(U_small,"Eigen")
  dimnames(Q) <- dimnames(U_small)
  Q
}


U2Q_hackish <- function(U, ages=seq(50,110,by=2), interval_current = 2){
  Q <- t(U) * 0
  for (a in ages){
    Qi <- logm_small(U, age = a, interval_current = interval_current)
    Q[rownames(Qi),colnames(Qi)] <- Qi
  }
  Q <- Q / interval_current
  Q
}

# extracts the rates and sticks them in a reasonable tibble
Q2Rtibble <- function(Q, interval_current = 2){
  Q |>
    as.data.frame() |>
    rownames_to_column(var = "from") |>
    pivot_longer(-1, names_to = "to", values_to = "R") |> 
    separate(to, 
             into = c("state_to", "age_to"), 
             sep = "::", 
             convert = TRUE) %>% 
    separate(from, 
             into = c("state_from", "age_from"), 
             sep = "::",
             convert = TRUE) |> 
    filter(age_to == (age_from + interval_current) |
             is.infinite(age_to),
           !is.infinite(age_from)) |>
    select(-age_to) |>
    mutate(transition = paste0(state_from, state_to), 
           .keep = "unused") |>
    pivot_wider(names_from = transition,
                values_from = R) |> 
    rename(age = age_from)
}

graduate_Rtibble <- function(Rtibble, interval_current = 2, interval_desired = 1, method = "spline"){
  age_in  <- Rtibble$age
  age_out <- seq(age_in[1], max(age_in) + interval_current , by = interval_desired)
  age_fit <- age_in + interval_current / 2
  HD1 <- splinefun(age_fit,Rtibble$HD)(age_out)
  UD1 <- splinefun(age_fit,Rtibble$UD)(age_out)
  UH1 <- splinefun(age_fit,Rtibble$UH)(age_out)
  HU1 <- splinefun(age_fit,Rtibble$HU)(age_out)
  tibble(age_out, HD = HD1, HU = HU1, UD = UD1, UH = UH1) |> 
    mutate(HH = -(HD+HU),
           UU = -(UD+UH))
}


Rtibble2Q <- function(Rtibble, interval = 1, start_age = 50){
  n <- nrow(Rtibble) + 1
  HH <- Rtibble |> 
    pull("HH") |>
    pi2u(from = "H", 
         to = "H", 
         start_age = start_age, 
         interval = interval) %>%
    '['(-1,-n)
  
  HU <- Rtibble |> 
    pull(HU) |> 
    pi2u("H","U", 
         start_age = start_age, 
         interval = interval) %>%
    '['(-1,-n)
  
  UU <- Rtibble |> 
    pull(UU) |> 
    pi2u("U","U", 
         start_age = start_age, 
         interval = interval) %>%
    '['(-1,-n)
  
  UH <- Rtibble |>
    pull(UH) |> 
    pi2u("U","H", 
         start_age = start_age, 
         interval = interval) %>%
    '['(-1,-n)
  
  Q <- rbind(
    cbind(HH, UH),
    cbind(HU, UU))
  
  Q <- cbind(rbind(Q, -colSums(Q)),0)
  colnames(Q)[ncol(Q)] <- "D::Inf"
  rownames(Q)[nrow(Q)] <- "D::Inf"
  Q
}

# Q is the standard transient matrix, in Markov orientation.
Q2U <- function(Q, interval_desired = 1){
  U <- 
    Q %>%
    '*'(interval_desired) |> 
    expm::expm()  
  dimnames(U) <- dimnames(Q)
  U |> t()
}
extract_pi <- function(U, from = "H", to = "H"){
  
  # identify the part of the matrix of the from and to of interest
  from_cols   <- grepl(colnames(U), pattern = from)
  to_cols     <-  grepl(rownames(U), pattern = to) 
  U_block     <- U[to_cols, from_cols]
  # take the subdiagonal of the matrix 
  # without the last col (last transition it is always to the abs. one )
  diag(U_block[-1,-ncol(U)])
  
}

# extract the non-null probabilities from U of each possible transition
U2Ptibble <- function(U, interval_current=1){
  
  U |>
    as.data.frame() |>
    rownames_to_column(var = "to") |>
    pivot_longer(-1, names_to = "from", values_to = "R") |> 
    separate(to, 
             into = c("state_to", "age_to"), 
             sep = "::", 
             convert = TRUE) %>% 
    separate(from, 
             into = c("state_from", "age_from"), 
             sep = "::",
             convert = TRUE) |> 
    filter(age_to == (age_from + interval_current) |
             is.infinite(age_to),
           !is.infinite(age_from)) |>
    select(-age_to) |>
    mutate(transition = paste0(state_from, state_to), 
           .keep = "unused") |>
    pivot_wider(names_from = transition,
                values_from = R) |> 
    rename(age = age_from)
}

complete_partial_Ptibble <- function(partial_Ptibble){
  partial_Ptibble <- as.data.frame(partial_Ptibble)
  
  all_from_to <- c("HH","HU","HD","UH","UU","UD")
  Missing <- setdiff(all_from_to, names(partial_Ptibble))
  
  if (length(Missing) == 0){
    return(partial_Ptibble)
  }
  partial_Ptibble[Missing] <- NA
  
  n <- nrow(partial_Ptibble)
  rownames(partial_Ptibble) <- 0:(n-1)
  
  partial_Ptibble %>% 
    as_tibble(rownames = NA) %>% 
    rownames_to_column("age") %>% 
    pivot_longer(-age,names_to = "from_to", values_to = "p") %>% 
    mutate(from = substr(from_to, 1, 1)) %>% 
    group_by(from, age) %>% 
    mutate(p = if_else(is.na(p), 1 - sum(p, na.rm = TRUE), p)) %>% 
    ungroup() %>% 
    select(-from) %>% 
    pivot_wider(names_from = from_to, values_from = p) %>% 
    column_to_rownames(var="age")
}





