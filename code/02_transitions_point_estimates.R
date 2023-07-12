rm(list = ls())
libraries <- list("data.table", "VGAM", "tidyverse")
lapply(libraries, require, character = T)

# Estimation of transition probabilities between health, disability and death states
# using Silc data 2012-15, survival correction matching survival probs with italian
# life tables 2014.
#------------------------------------------------------------------------------
all_yrs <- sprintf("%02d", 7:19)

# fixed age ranges
mineta <- 50
maxeta <- 79
age_init <- seq(mineta - 10, mineta + 10, 1)

# this is a directory contain prepped microdata
# not part of the repository, it needs to have
# been created and populated already
dir_in <- file.path("data/dat_trformat")

dir_out <- file.path("data/data_out")

for (yr in all_yrs) {
  tr_file <- paste0("SILC_panel_", yr, ".rds")
  
  dat <- readRDS(file.path(dir_in, tr_file))
  
  dat <- setDT(dat[!is.na(RX020) & !is.na(PB150), ])
  
  
  tr_format <-
    setDT(subset(
      dat,
      select = c(
        "PB030",
        "RX010",
        "FROM",
        "TO",
        "PB150",
        "PB010",
        "DB040",
        "edu",
        "area3",
        "edu_mid",
        "edu_high",
        "edu_low",
        "Centre",
        "South",
        "North"
      )
    ))
  names(tr_format) <-
    c(
      "pid",
      "age",
      "from",
      "to",
      "gender",
      "year",
      "area",
      "edu",
      "area3",
      "edu_mid",
      "edu_high",
      "edu_low",
      "Centre",
      "South",
      "North"
    )
  
  tr_format[, from2 := from, ]
  tr_format[, to2 := to, ]
  
  tr_format[, from := factor(
    from2,
    levels = c(0, 1, 2),
    labels = c("Healthy", "Disabled", "Dead")
  ), ]
  tr_format[, to := factor(to2,
                           levels = c(0, 1, 2),
                           labels = c("Healthy", "Disabled", "Dead")), ]
  
  tr_format <- unique(droplevels(tr_format[complete.cases(to), , ]))
  
  setkey(tr_format, "pid")
  tr_id <- tr_format[J(unique(pid)), mult = "first"]
  
  
  f1 <-
    formula(to ~  s(age) + from  + (edu_high + edu_low) + Centre + South)
  
  # Models
  
  fit.m <- vgam(
    f1
    ,
    family = multinomial(refLevel = 1),
    data = droplevels(tr_format[tr_format$gender == 1, ]),
    trace = T,
    control = vgam.control(maxit = 50)
  )
  
  
  fit.f <- vgam(
    f1
    ,
    family = multinomial(refLevel = 1),
    data = droplevels(tr_format[tr_format$gender == 2, ]),
    trace = T,
    control = vgam.control(maxit = 50)
  )
  
  #exp(coefvgam(fit.m,matrix=T))
  
  setkey(tr_format, "pid")
  tr_id <- droplevels(tr_format[J(unique(pid)), mult = "first"])
  
  # models for origin states
  # We estimate smooth prevalences by the same covariates
  # these are used just for purposes of reweighting the transitions
  # according to an externally valid lifetable. To do so, we need to
  # weight original estiamted survivorship according to actual prevalence
  # rather than the stationary prevalence.
  
  f2 <-
    formula(from ~  s(age) + (edu_high + edu_low) + Centre + South)
  
  # Models for corrections
  
  fit.m_weights <- vgam(
    f2
    ,
    family = multinomial(refLevel = 1),
    data = droplevels(tr_format[tr_format$gender == 1, ]),
    trace = T,
    control = vgam.control(maxit = 50)
  )
  
  
  fit.f_weights <- vgam(
    f2
    ,
    family = multinomial(refLevel = 1),
    data = droplevels(tr_format[tr_format$gender == 2, ]),
    trace = T,
    control = vgam.control(maxit = 50)
  )
  
  #---------------------------state space variables----------------------
  age <- paste(mineta:maxeta)
  hstatus <- c("Healthy", "Disabled")
  variables <- list(age = age, hstatus = hstatus)
  
  # Generate state space
  tstates <- levels(interaction(variables, sep = "::"))
  astates <- c("Dead")
  states <- c(tstates, astates)
  
  #------- probabilitites ---------------------------------------------------------------------------------------------------------
  # Data frame for prediction
  # Dummy data
  
  tmp_probs <- data.frame(state = tstates)
  setDT(tmp_probs)[, c("from", "age") := tstrsplit(
    state,
    "::",
    fixed = TRUE,
    keep = c(2, 1),
    type.convert = TRUE
  ), ]
  tmp_probs[, from := as.factor(from), ]
  
  
  # ITA avg  ====================================================================================
  
  probs.m <- probs.f <- tmp_probs
  
  means_ita <-
    tr_format[, lapply(.SD, function(x)
      mean(x, na.rm = T)), .SDcols = c("South", "Centre", "edu_low", "edu_high"), by =
        "gender"]
  
  probs.m <- cbind(probs.m, means_ita[gender == 1, ])
  probs.f <- cbind(probs.f, means_ita[gender == 2, ])
  # probs.m <- crossing(probs.m,means_ita[gender==1,])
  # probs.f <- crossing(probs.f,means_ita[2,])
  #
  probs.m_ita <- cbind(probs.m, predict(fit.m, probs.m, "response"))
  probs.f_ita <- cbind(probs.f, predict(fit.f, probs.f, "response"))
  
  means.m <- cbind(age = 50:79, means_ita[gender == 1, ])
  means.f <- cbind(age = 50:79, means_ita[gender == 2, ])
  
  ### Life table correction
  # for each age get the Health distribution
  
  weights_m <-
    cbind(means.m, predict(fit.m_weights, means.m, "response"))[, c("age", "Healthy", "Disabled")]
  weights_f <-
    cbind(means.f, predict(fit.f_weights, means.f, "response"))[, c("age", "Healthy", "Disabled")]
  
  names(weights_m) <-
    names(weights_f) <- c("age", "w_Healthy", "w_Disabled")
  
  # life tables
  myr <- sprintf("%02d", as.numeric(yr) - 1)
  lt_female <-
    file.path("data/life_tables", paste0("Italia_", myr, "_Women.csv"))
  
  # Before using life tables a few edits are needed. Eliminate the first row from 
  # the downloaded version from ISTAT; if not one has to add skip=1 in the 
  # corresponding read.csv
  
  lt_male  <-
    file.path("data/life_tables", paste0("Italia_", myr, "_Men.csv"))
  col_nm <- c("x", "age", "lx", "dx", "qx", "Lx", "px", "ex")
  
  female <- read.csv(lt_female, header = T, col.names = col_nm)
  male <- read.csv(lt_male, header = T, col.names = col_nm)
  
  # merge survival probs from life tables to health distribution
  
  weights_f <- merge(weights_f, female[, c("age", "qx")], by = "age")
  weights_m <- merge(weights_m,  male[, c("age", "qx")], by = "age")
  
  # merge the health distribution to trans probabilities
  
  sr_probs.f <- merge(probs.f_ita, weights_f, by = "age")
  sr_probs.m <- merge(probs.m_ita, weights_m, by = "age")
  
  # first we get the survival probabilities by origin state
  
  sr_probs.f[, surv := (Healthy + Disabled), by = c("from", "age")]
  sr_probs.f[, states_surv := ifelse(from == "Healthy", surv * w_Healthy, surv *
                                       w_Disabled)]
  sr_probs.f[, from_surv := sum(states_surv), by = "age"]
  sr_probs.f[, scaling_surv := from_surv / (1 - (qx / 1000)), ]
  sr_probs.f[, Healthy := Healthy / scaling_surv, ]
  sr_probs.f[, Disabled := Disabled / scaling_surv, ]
  
  # scale again if the probs exceed 1
  sr_probs.f[, surv2 := (Healthy + Disabled), by = c("from", "age")]
  sr_probs.f[, scaling_surv2 := 1, ]
  sr_probs.f[surv2 > 1, scaling_surv2 := 1 / surv2, ]
  sr_probs.f[, Healthy := Healthy / scaling_surv2, ]
  sr_probs.f[, Disabled := Disabled / scaling_surv2, ]
  
  sr_probs.m[, surv := (Healthy + Disabled), by = c("from", "age")]
  sr_probs.m[, states_surv := ifelse(from == "Healthy", surv * w_Healthy, surv *
                                       w_Disabled)]
  sr_probs.m[, from_surv := sum(states_surv), by = "age"]
  sr_probs.m[, scaling_surv := from_surv / (1 - (qx / 1000)), ]
  sr_probs.m[, Healthy := Healthy / scaling_surv, ]
  sr_probs.m[, Disabled := Disabled / scaling_surv, ]
  
  # scale again if the probs exceed 1
  sr_probs.m[, surv2 := (Healthy + Disabled), by = c("from", "age")]
  sr_probs.m[, scaling_surv2 := 1, ]
  sr_probs.m[surv2 > 1, scaling_surv2 := 1 / surv2, ]
  sr_probs.m[, Healthy := Healthy / scaling_surv2, ]
  sr_probs.m[, Disabled := Disabled / scaling_surv2, ]
  
  #===========================================================================
  transitions.m <- expand.grid(from = tstates, to = tstates)
  
  
  transitions <- function(dati_tmp) {
    data_tp <-
      melt(
        setDT(dati_tmp),
        id.vars = c("state", "age"),
        measure.vars = hstatus,
        variable.name = "state_to",
        value.name = "probs"
      )
    data_tp[, to := paste(age + 1, state_to, sep = "::"), ]
    setnames(data_tp, "state", "from")
    data_tp <-
      merge(transitions.m,
            data_tp[, c("from", "to", "probs"), with = F],
            by = c("from", "to"),
            all.x = T)
    data_tp <- setDT(na.omit(data_tp))
  }
  
  tp_m_ita <- transitions(sr_probs.m)[, geo_edu := "ita"][]
  tp_f_ita <- transitions(sr_probs.f)[, geo_edu := "ita"][]
  
  
  Umat_m <-
    dcast(
      setDT(tp_m_ita[, c("from", "to", "probs")][]),
      to ~ from,
      value.var = "probs",
      drop = FALSE,
      fill = 0
    )
  Umat_f <-
    dcast(
      setDT(tp_f_ita[, c("from", "to", "probs")][]),
      to ~ from,
      value.var = "probs",
      drop = FALSE,
      fill = 0
    )
  
  Umat_m <- as.matrix(Umat_m, rownames = "to")
  Umat_f <- as.matrix(Umat_f, rownames = "to")
  
  Fmat <- function(Umat) {
    solve(diag(1, nrow(Umat)) - Umat)
  }
  Expect <- function(Umat, eta_state) {
    N      <- Fmat(Umat)
    expect <- sum(N[, eta_state]) - 0.5
    expect
  }
  
  
  
  NM <- Fmat(Umat_m)
  NF <- Fmat(Umat_f)
  
  # Determine initial conditions based on prevalence estimates
  # over a wide age range centered on our lower age bound.
  weights_f <-
    prop.table(xtabs(~ from, data = subset(tr_id, age %in% age_init &
                                             gender == 2)))
  weights_f <- as.numeric(weights_f)
  
  weights_m <-
    prop.table(xtabs(~ from, data = subset(tr_id, age %in% age_init &
                                             gender == 1)))
  weights_m <- as.numeric(weights_m)
  
  LE_M  <-
    Expect(Umat_m, "50::Healthy") * weights_m[1] + Expect(Umat_m, "50::Disabled") *
    weights_m[2]
  LE_F  <-
    Expect(Umat_f, "50::Healthy") * weights_f[1] + Expect(Umat_f, "50::Disabled") *
    weights_f[2]
  
  # Note, in the decompositions, instead of making the initial conditions an additional
  # parameter to decompose, we instead derive these values from the transitions
  # estimated for age 50, a trivial discrepancy becuase initial conditions have
  # little leverage on the expectancies.
  
  DLE_M <-
    sum(c(sum(NM[rownames(NM) %like% "Disabled", "50::Healthy"]), sum(NM[rownames(NM) %like% "Disabled", "50::Disabled"]) -
            0.5) * weights_m)
  HLE_M <-
    sum(c(sum(NM[rownames(NM) %like% "Healthy", "50::Healthy"]) - .5, sum(NM[rownames(NM) %like% "Healthy", "50::Disabled"])) *
          weights_m)
  
  DLE_F <-
    sum(c(sum(NF[rownames(NF) %like% "Disabled", "50::Healthy"]), sum(NF[rownames(NF) %like% "Disabled", "50::Disabled"]) -
            0.5) * weights_f)
  HLE_F <-
    sum(c(sum(NF[rownames(NF) %like% "Healthy", "50::Healthy"]) - .5, sum(NF[rownames(NF) %like% "Healthy", "50::Disabled"])) *
          weights_f)
  
  
  risultati <- c(LE_M, HLE_M, DLE_M, LE_F, HLE_F, DLE_F)
  
  
  names(risultati) <-
    c("LE_M", "HLE_M", "DLE_M", "LE_F", "HLE_F", "DLE_F")
  write.csv(risultati, file = file.path(dir_out, paste0("expectancies_", yr, ".csv")))
  
  matrices <- list(female = Umat_f, male = Umat_m)
  #
  save(matrices, file = file.path(dir_out, paste0("Umats_", yr, ".rda")))
}
