source("helper_functions.R")
dat2 <- parse_asc_file("data/sub_1.asc")
dat <- trace_process(dat2)

png(file = "phases1.png", width = 500, height = 400)
ggplot(dat[trial == 21], aes(x = time, y = pupil_s)) + 
  geom_line() +
  theme(text = element_text(size = 20)) +
  labs(title = "pp1, trial 21")
dev.off()

png(file = "phases2.png", width = 500, height = 400)
ggplot(dat[trial == 21], aes(x = time, y = pupil_s, color = phase)) +
  geom_line() +
  theme(text = element_text(size = 20)) +
  labs(title = "pp1, trial 21")
dev.off()

png(file = "blink1.png", width = 500, height = 400)
ggplot(dat[trial == 21 & time %between% c(6500, 7500)], aes(x = time)) + 
  geom_line(aes(y = pupil_s), color = 'black', size = 1.5) +
  theme(text = element_text(size = 20)) +
  labs(title = "pp1, trial 21")
dev.off()

png(file = "blink2.png", width = 500, height = 400)
ggplot(dat[trial == 21 & time %between% c(6500, 7500)], aes(x = time)) + 
  geom_line(aes(y = pupil_s), color = 'black', size = 1.5) +
  geom_line(aes(y = pupil_b), color = "red", size = 2) + 
  theme(text = element_text(size = 20)) +
  labs(title = "pp1, trial 21")
dev.off()

png(file = "blink3.png", width = 500, height = 400)
ggplot(dat[trial == 21 & time %between% c(100, 7500)], aes(x = time)) + 
  geom_line(aes(y = pupil_s), color = 'black', size = 1.5) +
  geom_line(aes(y = pupil_b), color = "red", size = 2) + 
  geom_line(aes(y = pupil_e), color = "pink", size = 2.5) + 
  theme(text = element_text(size = 20)) +
  labs(title = "pp1, trial 21")
dev.off()

png(file = "blink4.png", width = 500, height = 400)
ggplot(dat[trial == 21 & time %between% c(6000, 7500)], aes(x = time)) +
  geom_line(aes(y = pupil_s), color = 'black', size = 1.5) +
  geom_line(aes(y = pupil_b), color = "red", size = 2) + 
  geom_line(aes(y = pupil_e), color = "pink", size = 2.5) + 
  geom_line(aes(y = pupil_i), color = "green", size = 1) + 
  theme(text = element_text(size = 20)) +
  labs(title = "pp1, trial 21")
dev.off()

dat[, ":="(pupil = pupil_i,
           pupil_i = NULL,
           pupil_b = NULL,
           pupil_e = NULL,
           pupil_s = NULL)]
# dat <- dat[!is.na(pupil)]

dat <- merge(dat, dat[phase == "response", .(resptime = max(time)), by = .(pp, trial)], by = c("pp", "trial"))
dat <- merge(dat, dat[phase == "problem", .(problemtime = min(time)), by = .(pp, trial)], by = c("pp", "trial"))
png(file = "timelock1.png", width = 500, height = 400)
ggplot() + 
  geom_line(data = dat[onsetTime == min(onsetTime)], aes(x = time, y = pupil, color = phase)) +
  geom_line(data = dat[onsetTime == max(onsetTime)], aes(x = time, y = pupil, color = phase)) +
  labs(title = "pp1, trial 2 and 20") + 
  theme(text = element_text(size = 20)) 
dev.off()

dat[, time_onset := time - onsetTime, by = .(pp, trial)] # Timelock the data to the onset of the mask

png(file = "timelock2.png", width = 500, height = 400)
ggplot() + 
  geom_line(data = dat[onsetTime == min(onsetTime)], aes(x = time_onset, y = pupil, color = phase)) +
  geom_line(data = dat[onsetTime == max(onsetTime)], aes(x = time_onset, y = pupil, color = phase)) +
  labs(title = "pp2, trial 2 and 20") + 
  theme(text = element_text(size = 20)) 
dev.off()

dat[, time := time - problemtime, by = .(pp, trial)] # Timelock the data to the onset of the mask
dat <- normalize(dat, dat$time >= -50 & dat$time <= 0,by = c("pp","trial"))
dat[, Baseline := NULL]

png(file = "normalize.png", width = 500, height = 400)
ggplot() + 
  geom_line(data = dat[onsetTime == min(onsetTime)], aes(x = time, y = pupil, color = phase)) +
  geom_line(data = dat[onsetTime == max(onsetTime)], aes(x = time, y = pupil, color = phase)) +
  labs(title = "pp2, trial 48 and 51") + 
  theme(text = element_text(size = 20)) 
dev.off()
dat[, onsetTime := NULL]


png(file = "downsample1.png", width = 500, height = 400)
ggplot() +
  geom_line(data = dat[trial == 1 & time %between% c(0, 400)], aes(x = time, y = pupil), size = 1) +
  geom_point(data = dat[trial == 1 & time %between% c(0, 400)], aes(x = time, y = pupil), size = 2) +
  labs(title = "pp2, trial 1") + 
  theme(text = element_text(size = 20)) 
dev.off()

dat2 <- downsample(dat, Hz = 100, by = c("subject_nr", "trial", "phase"))

png(file = "downsample2.png", width = 500, height = 400)
ggplot() +
  geom_line(data = dat[trial == 1 & time %between% c(0, 400)], aes(x = time, y = pupil), size = 1) +
  geom_point(data = dat[trial == 1 & time %between% c(0, 400)], aes(x = time, y = pupil), size = 2) +
  geom_line(data = dat2[trial == 1 & time %between% c(0, 400)], aes(x = time, y = pupil), size = 1, color = "red") +
  geom_point(data = dat2[trial == 1 & time %between% c(0, 400)], aes(x = time, y = pupil), size = 2, color = "red") +
  labs(title = "pp2, trial 1") + 
  theme(text = element_text(size = 20)) 
dev.off()

dat <- dat2
dat2 <- NULL

behav_dat <- fread("../data2020/subject-2.csv")
behav_dat <- behav_dat[,list(subject_nr,
                             trial = count_trial_sequence + 1,
                             block_type,
                             target_id,
                             fp = ifelse(block_type == "short", fp_short,
                                         ifelse(block_type == "long", fp_long,
                                                fp_mixed)),
                             fix_dur = time_mask_pre_msg - time_fix_dur,
                             response,
                             correct,
                             opacity)]
dat <- merge(dat, behav_dat, by = c("trial", "subject_nr"))

png(file = "mean1.png", width = 500, height = 400)
ggplot() +
  geom_line(data = dat[time %between% c(-50, 1500)], aes(x = time, y = pupil, group = as.factor(trial)), size = .5, alpha = .5) +
  labs(title = "pp2, all 96 trials") + 
  theme(text = element_text(size = 20)) 
dev.off()

plotdat <- dat[time %between% c(-50, 1500), .(pupil = mean(pupil)), by = time]
png(file = "mean2.png", width = 500, height = 400)
ggplot() +
  geom_line(data = dat[time %between% c(-50, 1500)], aes(x = time, y = pupil, group = as.factor(trial)), size = .5, alpha = .2) +
  geom_line(data = plotdat, aes(x = time, y = pupil), size = 2) +
  labs(title = "pp2, all 96 trials") + 
  theme(text = element_text(size = 20)) 
dev.off()