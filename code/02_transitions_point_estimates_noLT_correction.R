source("code/00_functions.R")

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
  
  tp_m_ita <- transitions(probs.m_ita)[, geo_edu := "ita"][]
  tp_f_ita <- transitions(probs.f_ita)[, geo_edu := "ita"][]
  
  
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
  write.csv(risultati, file = file.path(dir_out, paste0("expectancies_no_LT_", yr, ".csv")))
  
  matrices <- list(female = Umat_f, male = Umat_m)
  #
  save(matrices, file = file.path(dir_out, paste0("Umats_no_LT_", yr, ".rda")))
}
