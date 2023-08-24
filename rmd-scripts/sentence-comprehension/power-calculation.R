# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
 # # # POWER CALCULATION # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# First, run a model ------------------------------------------------------
dat.temp <- dat.pilot[, c('participants', 'itemNo', 'cloze', 'speed', 'noun_acc')] #this is a df from a pilot study 
dat.temp$noun_acc <- as.integer(dat.temp$noun_acc)

temp.m1 <- glmer(noun_acc ~ 1 +(cloze + speed + cloze:speed) +
                     (1 | participants) + (1 | itemNo), 
                 data=dat.temp, family = "binomial", 
                 control = glmerControl(calc.derivs = FALSE, optimizer="bobyqa", optCtrl = list(maxfun=1e6)),
                 nAGQ = 0, na.action = na.exclude)

# Add expected effect size ------------------------------------------------
lme4::fixef(temp.m1)['cloze2-1:speed2-1'] # This is the "effect size" or beta estimate of the interaction in the current pilot study.
fixef(temp.m1)['cloze2-1:speed2-1'] <- -1.5 # This is the effect size that I expect in the main experiment. "-1.5" comes from similar experiments conducted earlier.


# Extend along participants -----------------------------------------------
N_tar_grid    <- seq(192, 240, by = 16)
# N_tar_grid_simr <- N_tar_grid * 1 

fit_ext_simr  <- simr::extend(temp.m1, along = "participants", n = max(N_tar_grid)) # df extended along participants; increased sample size

getData(fit_ext_simr) %>% group_by(participants, noun_acc) %>% count() %>%
    pivot_wider(names_from = noun_acc, values_from=n) %>%
    mutate(tot=`0`+`1`, acc=`1`*100/tot) %>% View() # This shows that most of the participants have near ceiling performance, just like `dat.temp`

# Create simulation curve -------------------------------------------------
B_boot <- 1000 # number of boot repetitions within one experiment, one setup
simr::powerSim(fit_ext_simr, nsim = 1, progress = TRUE,
               test = fixed('cloze2-1:speed2-1', 'z'))

pc_out_max <- simr::powerCurve(fit_ext_simr,
                                along = 'participants', nsim = B_boot, breaks = N_tar_grid, progress = TRUE,
                                test = fixed('cloze2-1:speed2-1', 'z'));

pc_out_max

# This shows that to detect the expected effect size or beta estimate of -1.5 from the model we've specified, at least 80 participants' data are needed.

 
