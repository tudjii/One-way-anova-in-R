install.packages("tidyr")
library(tidyr)
library(dplyr)
library(haven)


data =read_sav("Smoker:Nonsmoker.sav")
sigara_icen <- data %>% 
  filter(Sigaraİçiyormu == 1)
sigara_icen

sigarayoksunsozel <- sigara_icen$YoksunÖlçümSözel

t.test(sigara_yoksun_sozel, sigara_yoksun_yazılı)

t.test(sigara_icen$YoksunÖlçümSözel, sigara_icen$İlkÖlçümSözel, alternative = "less")
t.test(sigara_icen$YoksunÖlçümYazılı, sigara_icen$İlkÖlçümYazılı, alternative = "less")

sigara_icmeyen <- data %>% 
  filter(Sigaraİçiyormu == 0)
t.test(sigara_icmeyen$YoksunÖlçümSözel, sigara_icmeyen$İlkÖlçümSözel, alternative = "less")
t.test(sigara_icmeyen$YoksunÖlçümYazılı, sigara_icmeyen$İlkÖlçümYazılı, alternative = "less")
yeni_data <- data.frame(sigara_icen$YoksunÖlçümSözel, sigara_icen$YoksunÖlçümYazılı, sigara_icen$İlkÖlçümSözel, sigara_icen$İlkÖlçümYazılı
               ,sigara_icmeyen$YoksunÖlçümSözel, sigara_icmeyen$YoksunÖlçümYazılı, sigara_icmeyen$İlkÖlçümSözel, sigara_icmeyen$İlkÖlçümYazılı)


means <- lapply(yeni_data, mean)
sds <- lapply(yeni_data, sd)

sozeller_mean <- c(means[1],means[3],means[5])
sozeller_sd <- c(sds[1],sds[3],sds[5])
sozeller_mean <- t(as.data.frame(sozeller_mean))
sozeller_sd <- t(as.data.frame(sozeller_sd))
sozel_mean_sd <- cbind(sozeller_mean,sozeller_sd)


sozel_scores <- data_frame(sigara_icen$YoksunÖlçümSözel, sigara_icen$İlkÖlçümSözel, sigara_icmeyen$YoksunÖlçümSözel)

status <- rep(c("sigara","yoksigara"),each = 21)
pre <- c(sigara_icen$YoksunÖlçümSözel, sigara_icmeyen$YoksunÖlçümSözel)
post <- c(sigara_icen$İlkÖlçümSözel, sigara_icmeyen$İlkÖlçümSözel)
anovadata <- data_frame(id= factor(1:42), status, pre, post)

dflong = gather(anovadata, key=time, value=score, pre:post) %>% arrange(id)


anovaModelRep = aov(score ~ status*time + Error(id), dflong)
summary(anovaModelRep)
