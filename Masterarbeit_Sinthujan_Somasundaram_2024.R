#####1. Vorbereitung#####
## Packages installieren:
install.packages("psych")
install.packages("tidyverse")
install.packages("readxl")
install.packages("ggplot2")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("lavaan")
install.packages("DescTools")
install.packages("car")
install.packages("reshape2")
install.packages("cocor")
install.packages("ltm")
install.packages("rstatix")
install.packages("VIM")
install.packages("ggpubr")

## Packages laden:
library(psych)
library(tidyverse)
library(readxl) #Daten einlesen
library(ggplot2) #Plots allgemein
library(rpart) #CART
library(rpart.plot) #Plot für CART
library(lavaan) #Für CFA
library(DescTools) #Für QQ-Plot
library(car) #Für den Levens Test für Varianzhomogenität und für Bootstrapping
library(reshape2)
library(cocor) #Für den statistischen Vergleich zweier Korrelationskoeffizienten
library(ltm) #Für die Punktbiseriale-Korrelation zu erhalten
library(rstatix)
library(VIM) ## Darstellung der fehlenden Werte
library(ggpubr) ## Zusammenführen der Barplots

## Welche Versionen sind die Packages?
packageVersion("psych")
packageVersion("tidyverse")
packageVersion("readxl")
packageVersion("ggplot2")
packageVersion("rpart")
packageVersion("rpart.plot")
packageVersion("lavaan")
packageVersion("DescTools")
packageVersion("car")
packageVersion("reshape2")
packageVersion("cocor")
packageVersion("ltm")
packageVersion("rstatix")
packageVersion("VIM")
packageVersion("ggpubr") 

## Den Datensatz einlesen:
d_full <- read_csv("Daten/Datensatz")
tibble(d_full)

## Die für diese Arbeit benötigten Variablen werden ausgewählt:
d <- d_full %>% select(!c("durill", "race_lat", "livsit2"))

#####2.1 Methode: Datenvalidierung ######
## UCLA-Gesamtscore muss ≥20 und ≤80 sein.
d$uclages <- ifelse(d$uclalst >= 20 | d$uclalst <= 80, 1, 0)
d$uclages <- factor(d$uclages,levels=c(0,1),labels=c("ausserhalb 20-80","innerhalb 20-80"))
table(d$uclages)
## alle sind zwischen 20 und 80.

## Einkommen sollten Stufen von 1-9 haben:
table(d$income_f)
table(d$income_p)
## Auch das passt.

## Soziale Positionen sollten Stufen von 1-5 haben:
table(d$socposc)
table(d$socposh)
table(d$socposl)
## Auch hier gibt es keine ungültigen Werte.

## Diagnose sollte den Wert 0 oder 1 haben:
table(d$Diagnosis)
## Stimmt.

## Der Familienstand hat die Werte 101 oder 102:
table(d$marital2)
## Auch diese Variable hat keine ungültigen Werte.

## Geschlecht hat Werte 0 oder 1:
table(d$gender)
## Stimmt.

## Das Brief Symptom Inventory Anxiety Scale Total Score kann zwischen 0-24 liegen.
table(d$bsiatot)
## Auch das passt.

## Das Connor Dacidson Resilience Scale muss den Wert zwischen 0-40 sein.
table(d$cdrs10_t)
## Sie beginnt hier bei 3 und geht bis 40. Deshalb ist das auch in Ordnung.

## Das Calgary Depression Scale Total Score kann zwischen 0-27 sein.
table(d$cdrstot)
## Hier sind Werte zwischen 0-12, deshalb stimmt es.

## Das CESD Happiness Scale muss Werte zwischen 0-12 sein.
table(d$cesdhs)
## Auch hier stimmt es mit den Skalen.

## Das Lifetime Orientation Test-Revised Total Score kann Werte zwischen 6-30 haben:
table(d$lotrt)
## Auch das passt. Sie beginnt hier bei 10.

## Gibt es ungültige Werte in der Variable Antipsychotikum?
table(d$apdddeq)
## Es gibt verschiedene Werte mit Dezimalstellen. Da es nach der WHO definierte tägliche Dosis ist, sollte das in Ordnung sein.

## Das Scale Assessment of Positive Symptoms Total Score beinhaltet Werte zwischen 0-170
table(d$sapstot)
## Eine Person hat hier einen Wert von -23. Dies ist hier ungültig.

## Wie sieht es aus mit dem Assessment of Negative Symptoms Total Score. Hier muss es Werte zwischen 0-120 haben.
table(d$sanstot)
## Auch hier hat eine Person einen ungültigen Wert von -27.

## Welche Personen haben bei diesen Variablen einen ungültigen Wert?
d$casenum[d$sanstot < 0]
d$casenum[d$sapstot < 0]
## Die ungültigen Werte kommen von der Fallnummer 40.

## Diese zwei ungültigen Werte werden als fehlende Werte behandelt:
d$sapstot[d$sapstot<0] <- NA
d$sanstot[d$sanstot<0] <- NA

## Bei der Executive Functioning Composite Score wurde der Z-Wert berechnet.
range(d$EXCOMP2)
## Dieser geht von -2.03 bis 1.77.

## Die SF-36 Mental und Physical Component Scale Total Score sollten zwischen 0-100 sein.
summary(d$phycomp)
summary(d$mencomp)
## Physical Component Scale geht von 21.64-67.24 und Mental Component Scale geht von 18.19-68.12

## Das Perceived Stress Scale Total Score soll die Werte zwischen 0-40.
table(d$psstot)
## Auch das passt. Sie geht von 0-37.

## Die Ausbildung wird in Jahre angegeben.
table(d$educ)
## Diese geht von 0-20 Jahren.

## Das Alter bei Krankheitsbeginn wird auch in Jahren angegeben.
table(d$aos)
## Zwischen 6-57 Jahren.

#####2.2 Methode: Deskriptive Statistik ######

## Wie viele Personen gehören zur Schizophreniegruppe und wie viele zur gesunden Kontrollgruppe?
table(d$Diagnosis)
## 106 gehören zur gesunden Kontrollgruppe und 116 Personen gehören zur Schizophreniegruppe.

## Wie viele Männer und Frauen sind in der Stichpronbe?
## und wie viele Männer und wie viele Frauen haben Schizophrenie, bzw. keine Schizophrenie?
table(d$gender)
table(d$Diagnosis, d$gender)
## Insgesamt hat es 113 Frauen und 109 Männer. Dabei haben 59 Frauen und 47 Männer keine Diagnose erhalten,
## und 54 Frauen und 62 Männer haben die Diagnose Schizophrenie oder schizoaffektive Störung erhalten.

## Welches Alter haben die Personen?
describe(d$agevisit)
describeBy(d$agevisit, d$Diagnosis)
## Das Alter der Teilnehmenden ist zwischen 27.13 Jahren und 68.78 Jahren (M = 51.11, SD = 10.89)
## Für die Kontrollgruppe ist der Alter zwischen 27.13 und 68.78(M = 51.49, SD = 11.4)
## Für die Schizophreniegruppe ist der Alter zwischen 28.05 und 68.13 Jahren (M = 50.77, SD = 10.44)

#####2.3 Methode: Fehlende Werte######

## Wie viele Fehlende Werte gibt es bei den UCLA-3 Items?
UCLA <- d %>% select(uclals01:uclals20r)
colSums(is.na(UCLA))
## Es gibt keine NA's. Dies wurde so auch in der Studie von Eglit et al. (2018) beschrieben.

## Wie sieht es aus für die anderen Variablen?
sum(is.na(d))
## Insgesamt 399 NAs gibt es.

## In welchen Variablen sind fehlenden Werte vorhanden und wie viele gibt es in diesen Variablen?
colSums(is.na(d))
## Bei der Variable aos hat es 108 fehlende Werte. Dabei gehören 106 der fehlenden Werten zu den Personen, die keine Schizophrenie haben.
## Welche 2 Personen der Gruppe Schizophrenie haben keine Angaben dazu gemacht?
d_sch <- d %>% filter(Diagnosis == 1)
d_sch$casenum[is.na(d_sch$aos)]
## Die Personen mit den casenummern 9 und 201 haben keine Angaben dazu gemacht.

## Wie viele Personen von welcher Gruppe haben fehlende Werte in den verschiedenen Variablen?
d_nc <- d %>% filter(Diagnosis == 0)
colSums(is.na(d_sch))
colSums(is.na(d_nc))

## Visualisierung von fehlenden Werten getrennt für die Gruppen:
d_miss_sch <- d_sch[, 1:25]
d_miss_nc <- d_nc[, 1:25]

aggr(d_miss_sch, col = c("lightskyblue3", "pink1"), numbers = TRUE, sortVars = TRUE, labels = names(d_miss_sch), cex.axis = 1,
     gap = 1, ylab = c("Fehlende Werte", "Muster"))
aggr(d_miss_nc, col = c("lightskyblue3", "pink1"), numbers = TRUE, sortVars = TRUE, labels = names(d_miss_nc), cex.axis = 1,
     gap = 1, ylab = c("Fehlende Werte", "Muster"))

#####2.4 Methode: Voraussetzungen der Faktorenanalyse######

## Ausreisser:

## Die Namen der Items auf "Fragen" ändern:
d_sch_name <- d_sch %>% rename("Frage.1r" = "uclals01r", "Frage.2" = "uclals02", "Frage.3" = "uclals03", "Frage.4" = "uclals04",
                               "Frage.5r" = "uclals05r", "Frage.6r" = "uclals06r", "Frage.7" = "uclals07", "Frage.8" = "uclals08",
                               "Frage.9r" = "uclals09r", "Frage.10r" = "uclals10r", "Frage.11" = "uclals11", "Frage.12" = "uclals12",
                               "Frage.13" = "uclals13", "Frage.14" = "uclals14", "Frage.15r" = "uclals15r", "Frage.16r" = "uclals16r",
                               "Frage.17" = "uclals17", "Frage.18" = "uclals18", "Frage.19r" = "uclals19r", "Frage.20r" = "uclals20r")
d_nc_name <- d_nc %>% rename("Frage.1r" = "uclals01r", "Frage.2" = "uclals02", "Frage.3" = "uclals03", "Frage.4" = "uclals04",
                             "Frage.5r" = "uclals05r", "Frage.6r" = "uclals06r", "Frage.7" = "uclals07", "Frage.8" = "uclals08",
                             "Frage.9r" = "uclals09r", "Frage.10r" = "uclals10r", "Frage.11" = "uclals11", "Frage.12" = "uclals12",
                             "Frage.13" = "uclals13", "Frage.14" = "uclals14", "Frage.15r" = "uclals15r", "Frage.16r" = "uclals16r",
                             "Frage.17" = "uclals17", "Frage.18" = "uclals18", "Frage.19r" = "uclals19r", "Frage.20r" = "uclals20r")

## Den Datensatz beider Gruppen in Long-Format ändern:
d_sch_ohnetot <- d_sch_name %>% select(26:54) # Alle UCLA-3 Items werden ausgewählt
d_sch_long <- melt(d_sch_ohnetot, measure.vars = 1:29) # In Long-Format ändern
d_nc_ohnetot <- d_nc_name %>% select(26:54) # Alle UCLA-3 Items werden ausgewählt
d_nc_long <- melt(d_nc_ohnetot, measure.vars = 1:29) # In Long-Format ändern



## Bei d_sch und d_nc alle positiv formulierten Items sollen nur die inverse skalierte Items vorhanden sein:
d_sch_long_r <- d_sch_long %>% filter(variable != "uclals01")
d_sch_long_r <- d_sch_long_r %>% filter(variable != "uclals05")
d_sch_long_r <- d_sch_long_r %>% filter(variable != "uclals06")
d_sch_long_r <- d_sch_long_r %>% filter(variable != "uclals09")
d_sch_long_r <- d_sch_long_r %>% filter(variable != "uclals10")
d_sch_long_r <- d_sch_long_r %>% filter(variable != "uclals15")
d_sch_long_r <- d_sch_long_r %>% filter(variable != "uclals16")
d_sch_long_r <- d_sch_long_r %>% filter(variable != "uclals19")
d_sch_long_r <- d_sch_long_r %>% filter(variable != "uclals20")

d_nc_long_r <- d_nc_long %>% filter(variable != "uclals01")
d_nc_long_r <- d_nc_long_r %>% filter(variable != "uclals05")
d_nc_long_r <- d_nc_long_r %>% filter(variable != "uclals06")
d_nc_long_r <- d_nc_long_r %>% filter(variable != "uclals09")
d_nc_long_r <- d_nc_long_r %>% filter(variable != "uclals10")
d_nc_long_r <- d_nc_long_r %>% filter(variable != "uclals15")
d_nc_long_r <- d_nc_long_r %>% filter(variable != "uclals16")
d_nc_long_r <- d_nc_long_r %>% filter(variable != "uclals19")
d_nc_long_r <- d_nc_long_r %>% filter(variable != "uclals20")

## Plot für die Schizophreniegruppe:
plot_ausreisser_sch2 <- ggplot(d_sch_long_r, aes(x = value, fill = factor(value)))
plot_ausreisser_sch2 + geom_bar() +
  labs(y = "Häufigkeit", x = "Angegebene Antwort", fill = "Angegebene Antwort") +
  facet_wrap(~variable)
## Wenige Items mit untypischen Werten.

## Plot für Kontrollgruppe:
plot_ausreisser_nc2 <- ggplot(d_nc_long_r, aes(x = value, fill = factor(value)))
plot_ausreisser_nc2 + geom_bar() +
  labs(y = "Häufigkeit", x = "Angegebene Antwort", fill = "Angegebene Antwort") +
  facet_wrap(~variable)
## Hier haben fast alle Items untypische Werte.

#####2.5 Methode: Voraussetzung Pearson Produkt-Moment Korrelation######

## Normalverteilungannahme mithilfe von Shapiro-Wilk-Test:

## Alter:
shapiro.test(d_sch$agevisit)
## Shapiro-Wilk Test zeigt signifikante Abweichung von der Normalverteilung für die Schizophreniegruppe.

shapiro.test(d_nc$agevisit)
## Auch bei der gesunden Gruppe ist der Shapiro-Wilk Test signifikant. 

## Alter bei Krankheitsbeginn:
shapiro.test(d_sch$aos)
## Auch dieser Test zeigt deutlich signifikante Abweichung von der Normalverteilung.

## Ausbildung:
shapiro.test(d_sch$educ)
shapiro.test(d_nc$educ)
## In beiden Gruppen weichen die Variablen signifikant von der Normalverteilung ab.

## Antipsychotikum:
shapiro.test(d_sch$apdddeq)
## Auch hier ist der Shapiro-Wilk Test signifikant.

## Positive Symptome:
shapiro.test(d_sch$sapstot)
## Weicht signifikant von der Normalverteilung ab.

## Negative Symptome:
shapiro.test(d_sch$sanstot)
## Auch hier gibt es signifikante Abweichung von der Normalverteilung.

## Depression:
shapiro.test(d_sch$cdrstot)
shapiro.test(d_nc$cdrstot)
## Weichen beide ab von der Normalverteilung.

## Angst:
shapiro.test(d_sch$bsiatot)
shapiro.test(d_nc$bsiatot)
## Beide signifikant. 

## Exekutive Funktion:
shapiro.test(d_sch$EXCOMP2)
shapiro.test(d_nc$EXCOMP2)
## Zeigen beide eine Normalverteilung!

## Physische Gesundheit:
shapiro.test(d_sch$phycomp)
shapiro.test(d_nc$phycomp)
## Bei der Schizophreniegruppe gibt es eine Normalverteilung. Bei der NC Gruppe hingegen nicht.

## Mentale Gesundheit:
shapiro.test(d_sch$mencomp)
shapiro.test(d_nc$mencomp)
## Auch hier, bei Schizophreniegruppe ist sie normalverteilt, bei der NC Gruppe aber nicht.

## Glücklichkeit
shapiro.test(d_sch$cesdhs)
shapiro.test(d_nc$cesdhs)
## Beide sind signifikant.

## Resilienz:
shapiro.test(d_sch$cdrs10_t)
shapiro.test(d_nc$cdrs10_t)
## Beide signifikant. 

## Optimismus:
shapiro.test(d_sch$lotrt)
shapiro.test(d_nc$lotrt)
## Bei der Schizophreniegruppe gibt es keine signifikanten Abweichung der Normalverteilung. 
## Bei der Kontrollgruppe gibt es jedoch eine signifikante Abwweichung.

## Erlebter Stress:
shapiro.test(d_sch$psstot)
shapiro.test(d_nc$psstot)
## Bei der Schizophreniegruppe gibt es keine signifikanten Abweichung der Normalverteilung.
## Bei der Kontrollgruppe gibt es hingegen eine signifikante Abweichung der Normalverteilung.

## Lebenszufriedenheit:
shapiro.test(d_sch$swlstot)
shapiro.test(d_nc$swlstot)
## Schizophreniegruppe ist normalverteilt, die NC Gruppe nicht.

## Einsamkeit:
shapiro.test(d_sch$uclalst)
shapiro.test(d_nc$uclalst)
## Schizophreniegruppe ist Normalverteilt. Bei der NC Gruppe war der Shapiro-Wilk Test signifikant.

## Überprüfung linearer Zusammenhang zwischen UCLA-3 Gesamtscore und den kontinuierlichen Variablen:
## Dafür wird immer ein Streudiagramm erstellt, um den Zusammenhang zu prüfen.

## Alter:
ggplot(d_sch, aes(x = uclalst, y = agevisit)) +
  geom_smooth(method = "lm") +
  geom_point()
ggplot(d_nc, aes(x = uclalst, y = agevisit)) +
  geom_smooth(method = "lm") +
  geom_point()
## Bei beiden ist es sehr verstreut. Jedoch kein Anzeichen auf eine nicht-lineare Beziehung.

## Alter bei Krankheitsbeginn:
ggplot(d_sch, aes(x = uclalst, y = aos)) +
  geom_smooth(method = "lm") +
  geom_point()
## Auch hier ist es stark versteut. Jedoch auch hier kein Anzeichen auf nicht-lineare Beziehung.

## Ausbildung:
ggplot(d_sch, aes(x = uclalst, y = educ)) +
  geom_smooth(method = "lm") +
  geom_point()

ggplot(d_sch, aes(x = uclalst, y = educ)) +
  geom_smooth(method = "lm") +
  geom_point()
## Eher schwacher Zusammenhang, aber kein Anzeichen einer nicht-linearen Beziehung.

## Antipsychotikum:
ggplot(d_sch, aes(x = uclalst, y = apdddeq)) +
  geom_smooth(method = "lm") +
  geom_point()
ggplot(d_nc, aes(x = uclalst, y = apdddeq)) +
  geom_smooth(method = "lm") +
  geom_point()
## Hier ist es auch stark verstreut in der Schizophreniegruppe,
## aber kein Anzeichen auf eine nicht-lineare Beziehung. Bei der Kontrollgruppe sind alle Werte auf 0.

## Positive Symptome:
ggplot(d_sch, aes(x = uclalst, y = sapstot)) +
  geom_smooth(method = "lm") +
  geom_point()
## Bei Schizophreniegruppe eher linearer positiver Zusammenhang.

## Negative Symptome:
ggplot(d_sch, aes(x = uclalst, y = sanstot)) +
  geom_smooth(method = "lm") +
  geom_point()
## Bei Schizophreniegruppe schein der Zusammenhang eher linear zu sein.

## Depression:
ggplot(d_sch, aes(x = uclalst, y = cdrstot)) +
  geom_smooth(method = "lm") +
  geom_point()
ggplot(d_nc, aes(x = uclalst, y = cdrstot)) +
  geom_smooth(method = "lm") +
  geom_point()
## Eher positiver Zusammenhang, kein Anzeichen einer nicht-linearen Beziehung.

## Angst:
ggplot(d_sch, aes(x = uclalst, y = bsiatot)) +
  geom_smooth(method = "lm") +
  geom_point()
ggplot(d_nc, aes(x = uclalst, y = bsiatot)) +
  geom_smooth(method = "lm") +
  geom_point()
## Die Streuung bei der Schizophreniegruppe ist gross. Insgesamt kein Anzeichen einer nicht-linearen Beziehung.

## Exekutiven Funtkionen:
ggplot(d_sch, aes(x = uclalst, y = EXCOMP2)) +
  geom_smooth(method = "lm") +
  geom_point()
ggplot(d_nc, aes(x = uclalst, y = EXCOMP2)) +
  geom_smooth(method = "lm") +
  geom_point()
## Kein Anzeichen von einem nicht-linearen Zusammenhang.

## Physische Gesundheit:
ggplot(d_sch, aes(x = uclalst, y = phycomp)) +
  geom_smooth(method = "lm") +
  geom_point()
ggplot(d_nc, aes(x = uclalst, y = phycomp)) +
  geom_smooth(method = "lm") +
  geom_point()
## Tendenz einer negativen Zusammenhang bei gesunder Gruppe. Bei der Schizophreniegruppe ist der Zusammenhang auch eher negativ.
## Kein Anzeichen von einem nicht-linearen Zusammenhang.

## Mentale Gesundheit:
ggplot(d_sch, aes(x = uclalst, y = mencomp)) +
  geom_smooth(method = "lm") +
  geom_point()
ggplot(d_nc, aes(x = uclalst, y = mencomp)) +
  geom_smooth(method = "lm") +
  geom_point()
## Kein Anzeichen für einen nicht-linearen Zusammenhang.

## Glücklichkeit:
ggplot(d_sch, aes(x = uclalst, y = cesdhs)) +
  geom_smooth(method = "lm") +
  geom_point()
ggplot(d_nc, aes(x = uclalst, y = cesdhs)) +
  geom_smooth(method = "lm") +
  geom_point()
## Auch hier hat es eine starke Streuung, jedoch kein Anzeichen einer nicht-linearen Zusammenhang.

## Resilienz:
ggplot(d_sch, aes(x = uclalst, y = cdrs10_t)) +
  geom_smooth(method = "lm") +
  geom_point()
ggplot(d_nc, aes(x = uclalst, y = cdrs10_t)) +
  geom_smooth(method = "lm") +
  geom_point()
## Kein Anzeichen für einen nicht-linearen Zusammenhang.

## Optimismus:
ggplot(d_sch, aes(x = uclalst, y = lotrt)) +
  geom_smooth(method = "lm") +
  geom_point()
ggplot(d_nc, aes(x = uclalst, y = lotrt)) +
  geom_smooth(method = "lm") +
  geom_point()
## Kein Anzeichen für einen nicht-linearen Zusammenhang.

## Erlebter Stress:
ggplot(d_sch, aes(x = uclalst, y = psstot)) +
  geom_smooth(method = "lm") +
  geom_point()
ggplot(d_nc, aes(x = uclalst, y = psstot)) +
  geom_smooth(method = "lm") +
  geom_point()
## Kein Anzeichen für einen nicht-linearen Zusammenhang.

## Lebenszufriedenheit:
ggplot(d_sch, aes(x = uclalst, y = swlstot)) +
  geom_smooth(method = "lm") +
  geom_point()
ggplot(d_nc, aes(x = uclalst, y = swlstot)) +
  geom_smooth(method = "lm") +
  geom_point()
## Kein Anzeichen für einen nicht-linearen Zusammenhang.

## Alle Plots zusammenfügen:
## Für Schizophreniegruppe:
d_sch_scatter <- d_sch %>% select(c("casenum","agevisit", "aos", "educ", "apdddeq", "sapstot", "sanstot", "cdrstot", "bsiatot",
                                    "EXCOMP2", "phycomp", "mencomp", "cesdhs", "cdrs10_t", "lotrt", "psstot", "swlstot", "uclalst"))

## Die Namen der Variablen ändern für den Plot:
d_sch_scatter <- d_sch_scatter %>% rename("Alter" = "agevisit", "Alter bei Krankheitsbeginn" = "aos", "Ausbildung" = "educ", "Antipsychotikum" = "apdddeq",
                                          "Positivsymptome" = "sapstot", "Negativsymptome" = "sanstot", "Depression" = "cdrstot", "Angst" = "bsiatot",
                                          "Exekutive Funktionen" = "EXCOMP2", "Physische Gesundheit" = "phycomp", "Mentale Gesundheit" = "mencomp",
                                          "Glücklichkeit" = "cesdhs", "Resilienz" = "cdrs10_t", "Optimismus" = "lotrt", "Erlebter Stress" = "psstot",
                                          "Lebenszufriedenheit" = "swlstot", "Einsamkeit" = "uclalst")
## Plot für die Schizophreniegruppe:
sch_long <- d_sch_scatter %>%
  pivot_longer(cols = c("Alter", "Alter bei Krankheitsbeginn", "Ausbildung", "Antipsychotikum", "Positivsymptome", "Negativsymptome", "Depression",
                        "Angst", "Exekutive Funktionen", "Physische Gesundheit", "Mentale Gesundheit", "Glücklichkeit", "Resilienz", "Optimismus",
                        "Erlebter Stress", "Lebenszufriedenheit"), 
               names_to = "Variable", 
               values_to = "Wert")

ggplot(sch_long, aes(x = Einsamkeit, y = Wert)) +
  geom_smooth(method = "lm") +
  geom_point() +
  facet_wrap(~ Variable, scales = "free_y")

## Für gesunde Gruppe:
d_nc_scatter <- d_nc %>% select(c("casenum","agevisit", "educ", "apdddeq", "cdrstot", "bsiatot",
                                  "EXCOMP2", "phycomp", "mencomp", "cesdhs", "cdrs10_t", "lotrt", "psstot", "swlstot", "uclalst"))

## Auch hier werden die Variablennamen geändert:
d_nc_scatter <- d_nc_scatter %>% rename("Alter" = "agevisit", "Ausbildung" = "educ", "Antipsychotikum" = "apdddeq",
                                        "Depression" = "cdrstot", "Angst" = "bsiatot", "Exekutive Funktionen" = "EXCOMP2", 
                                        "Physische Gesundheit" = "phycomp", "Mentale Gesundheit" = "mencomp",
                                        "Glücklichkeit" = "cesdhs", "Resilienz" = "cdrs10_t", "Optimismus" = "lotrt", "Erlebter Stress" = "psstot",
                                        "Lebenszufriedenheit" = "swlstot", "Einsamkeit" = "uclalst")

nc_long <- d_nc_scatter %>%
  pivot_longer(cols = c("Alter", "Ausbildung", "Antipsychotikum", "Depression", "Angst",
                        "Exekutive Funktionen", "Physische Gesundheit", "Mentale Gesundheit", "Glücklichkeit", "Resilienz",
                        "Optimismus", "Erlebter Stress", "Lebenszufriedenheit"), 
               names_to = "Variable", 
               values_to = "Wert")

ggplot(nc_long, aes(x = Einsamkeit, y = Wert)) +
  geom_smooth(method = "lm") +
  geom_point() +
  facet_wrap(~ Variable, scales = "free_y")

## Überprüfung der extremen Ausreisserwerte, für Schizophreniegruppe:
## Dafür werden zuerst Boxplots erstellt.
sch_outlier_long <- d_sch_scatter %>%
  pivot_longer(cols = c("Alter", "Alter bei Krankheitsbeginn", "Ausbildung", "Antipsychotikum", "Positivsymptome", "Negativsymptome", "Depression",
                        "Angst", "Exekutive Funktionen", "Physische Gesundheit", "Mentale Gesundheit", "Glücklichkeit", "Resilienz", "Optimismus",
                        "Erlebter Stress", "Lebenszufriedenheit", "Einsamkeit"), 
               names_to = "Variable", 
               values_to = "Wert")
ggplot(sch_outlier_long, aes(y = Wert, fill = Variable)) +
  facet_wrap(~ Variable, scales = "free_y") +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.size = 2, outlier.shape = 1)+
  theme(strip.text = element_text(size = 9))
## Insgesamt 9 Variablen mit Ausreisser. Wie viele Davon sind extreme Ausreisserwert?

## Alter bei Krankheitsbeginn:
IQR_s_aos <- IQR(d_sch$aos, na.rm = TRUE)
(outlier_u_s_aos <- quantile(d_sch$aos, probs = 0.25, na.rm = TRUE) - (3 * IQR_s_aos))
(outlier_o_s_aos <- quantile(d_sch$aos, probs = 0.75, na.rm = TRUE) + (3 * IQR_s_aos))

(outlier_s_aos <- d_sch %>% filter(aos < outlier_u_s_aos | aos > outlier_o_s_aos))
## 3 Personen haben Extremwerte: Fallnummer 58, 73, 78

## Antipsychotikum:
IQR_s_apdddeq <- IQR(d_sch$apdddeq, na.rm = TRUE)
(outlier_u_s_apdddeq <- quantile(d_sch$apdddeq, probs = 0.25, na.rm = TRUE) - (3 * IQR_s_apdddeq))
(outlier_o_s_apdddeq <- quantile(d_sch$apdddeq, probs = 0.75, na.rm = TRUE) + (3 * IQR_s_apdddeq))

(outlier_s_apdddeq <- d_sch %>% filter(apdddeq < outlier_u_s_apdddeq | apdddeq > outlier_o_s_apdddeq))
## Keine Extremwerte.

## Depression:
IQR_s_cdrstot <- IQR(d_sch$cdrstot, na.rm = TRUE)
(outlier_u_s_cdrstot <- quantile(d_sch$cdrstot, probs = 0.25, na.rm = TRUE) - (3 * IQR_s_cdrstot))
(outlier_o_s_cdrstot <- quantile(d_sch$cdrstot, probs = 0.75, na.rm = TRUE) + (3 * IQR_s_cdrstot))

(outlier_s_cdrstot <- d_sch %>% filter(cdrstot < outlier_u_s_cdrstot | cdrstot > outlier_o_s_cdrstot))
## Keine Extremwerte.

## Ausbildung:
IQR_s_educ <- IQR(d_sch$educ, na.rm = TRUE)
(outlier_u_s_educ <- quantile(d_sch$educ, probs = 0.25, na.rm = TRUE) - (3 * IQR_s_educ))
(outlier_o_s_educ<- quantile(d_sch$educ, probs = 0.75, na.rm = TRUE) + (3 * IQR_s_educ))

(outlier_s_educ <- d_sch %>% filter(educ < outlier_u_s_educ | educ > outlier_o_s_educ))
## 3 Extremwerte (VP 5, 48 und 101)

## Exekutive Funktionen:
IQR_s_excomp<- IQR(d_sch$EXCOMP2, na.rm = TRUE)
(outlier_u_s_excomp <- quantile(d_sch$EXCOMP2, probs = 0.25, na.rm = TRUE) - (3 * IQR_s_excomp))
(outlier_o_s_excomp <- quantile(d_sch$EXCOMP2, probs = 0.75, na.rm = TRUE) + (3 * IQR_s_excomp))

(outlier_s_excomp <- d_sch %>% filter(EXCOMP2 < outlier_u_s_excomp | EXCOMP2 > outlier_o_s_excomp))
## Keine Extremwerte.

## Optimismus:
IQR_s_lotrt<- IQR(d_sch$lotrt, na.rm = TRUE)
(outlier_u_s_lotrt <- quantile(d_sch$lotrt, probs = 0.25, na.rm = TRUE) - (3 * IQR_s_lotrt))
(outlier_o_s_lotrt <- quantile(d_sch$lotrt, probs = 0.75, na.rm = TRUE) + (3 * IQR_s_lotrt))

(outlier_s_lotrt <- d_sch %>% filter(lotrt < outlier_u_s_lotrt | lotrt > outlier_o_s_lotrt))
## Keine Extremwerte.

## Erlebter Stress:
IQR_s_psstot<- IQR(d_sch$psstot, na.rm = TRUE)
(outlier_u_s_psstot <- quantile(d_sch$psstot, probs = 0.25, na.rm = TRUE) - (3 * IQR_s_psstot))
(outlier_o_s_psstot <- quantile(d_sch$psstot, probs = 0.75, na.rm = TRUE) + (3 * IQR_s_psstot))

(outlier_s_psstot <- d_sch %>% filter(psstot < outlier_u_s_psstot | psstot > outlier_o_s_psstot))
## Keine Extremwerte.

## Negative Symptome:
IQR_s_sanstot<- IQR(d_sch$sanstot, na.rm = TRUE)
(outlier_u_s_sanstot <- quantile(d_sch$sanstot, probs = 0.25, na.rm = TRUE) - (3 * IQR_s_sanstot))
(outlier_o_s_sanstot <- quantile(d_sch$sanstot, probs = 0.75, na.rm = TRUE) + (3 * IQR_s_sanstot))

(outlier_s_sanstot <- d_sch %>% filter(sanstot < outlier_u_s_sanstot | sanstot > outlier_o_s_sanstot))
## Keine Extremwerte.

## UCLA-3 Gesamtscore (Einsamkeit:
IQR_s_uclalst<- IQR(d_sch$uclalst, na.rm = TRUE)
(outlier_u_s_uclalst <- quantile(d_sch$uclalst, probs = 0.25, na.rm = TRUE) - (3 * IQR_s_uclalst))
(outlier_o_s_uclalst <- quantile(d_sch$uclalst, probs = 0.75, na.rm = TRUE) + (3 * IQR_s_uclalst))

(outlier_s_uclalst <- d_sch %>% filter(uclalst < outlier_u_s_uclalst | uclalst > outlier_o_s_uclalst))
## Keine Extremwerte.

## Wie sieht es aus mit der gesunden Gruppe?

## Dafür wieder Boxplot:
nc_outlier_long <- d_nc_scatter %>%
  pivot_longer(cols = c("Alter", "Ausbildung", "Antipsychotikum", "Depression", "Angst",
                        "Exekutive Funktionen", "Physische Gesundheit", "Mentale Gesundheit", "Glücklichkeit", "Resilienz",
                        "Optimismus", "Erlebter Stress", "Lebenszufriedenheit", "Einsamkeit"), 
               names_to = "Variable", 
               values_to = "Wert")

ggplot(nc_outlier_long, aes(y = Wert, fill = Variable)) +
  facet_wrap(~ Variable, scales = "free_y") +
  stat_boxplot(geom = "errorbar", width = 0.5) +
  geom_boxplot(outlier.size = 2, outlier.shape = 1)+
  theme(strip.text = element_text(size = 9))

## Insgesamt 5 Variablen mit Ausreisser. Welche sind Extrem?

## Angst:
IQR_nc_bsiatot<- IQR(d_nc$bsiatot, na.rm = TRUE)
(outlier_u_nc_bsiatot <- quantile(d_nc$bsiatot, probs = 0.25, na.rm = TRUE) - (3 * IQR_nc_bsiatot))
(outlier_o_nc_bsiatot <- quantile(d_nc$bsiatot, probs = 0.75, na.rm = TRUE) + (3 * IQR_nc_bsiatot))

(outlier_nc_bsiatot <- d_nc %>% filter(bsiatot < outlier_u_nc_bsiatot | bsiatot > outlier_o_nc_bsiatot))
## Eine Person (VP 31) mit Extremwert.

## Depression:
IQR_nc_cdrstot <- IQR(d_nc$cdrstot, na.rm = TRUE)
(outlier_u_nc_cdrstot <- quantile(d_nc$cdrstot, probs = 0.25, na.rm = TRUE) - (3 * IQR_nc_cdrstot))
(outlier_o_nc_cdrstot <- quantile(d_nc$cdrstot, probs = 0.75, na.rm = TRUE) + (3 * IQR_nc_cdrstot))

(outlier_nc_cdrstot <- d_nc %>% filter(cdrstot < outlier_u_nc_cdrstot | cdrstot > outlier_o_nc_cdrstot))
## 4 Extremwerte (VP 23, 31, 107 und 116).

## Physische Gesundheit:
IQR_nc_phycomp <- IQR(d_nc$phycomp, na.rm = TRUE)
(outlier_u_nc_phycomp <- quantile(d_nc$phycomp, probs = 0.25, na.rm = TRUE) - (3 * IQR_nc_phycomp))
(outlier_o_nc_phycomp <- quantile(d_nc$phycomp, probs = 0.75, na.rm = TRUE) + (3 * IQR_nc_phycomp))

(outlier_nc_phycomp <- d_nc %>% filter(phycomp < outlier_u_nc_phycomp | phycomp > outlier_o_nc_phycomp))
## 2 Extremwerte (VP 23 und 95).

## UCLA-3 Gesamtscore:
IQR_nc_uclalst <- IQR(d_nc$uclalst, na.rm = TRUE)
(outlier_u_nc_uclalst <- quantile(d_nc$uclalst, probs = 0.25, na.rm = TRUE) - (3 * IQR_nc_uclalst))
(outlier_o_nc_uclalst <- quantile(d_nc$uclalst, probs = 0.75, na.rm = TRUE) + (3 * IQR_nc_uclalst))

(outlier_nc_uclalst <- d_nc %>% filter(uclalst < outlier_u_nc_uclalst | uclalst > outlier_o_nc_uclalst))
## Keine Extremwerte.

#####2.6 Methode: Voraussetzungen punktbiseriale Korrelation#####
## Wie viele Ausreisserwerte gibt es in der UCLA-3 Gesamtscore?
## Bei der Schizophreniegruppe:
IQR_s_uclalst<- IQR(d_sch$uclalst, na.rm = TRUE)
(outlier_u_s_uclalst_2 <- quantile(d_sch$uclalst, probs = 0.25, na.rm = TRUE) - (1.5 * IQR_s_uclalst))
(outlier_o_s_uclalst_2 <- quantile(d_sch$uclalst, probs = 0.75, na.rm = TRUE) + (1.5 * IQR_s_uclalst))
(outlier_s_uclalst_2 <- d_sch %>% filter(uclalst < outlier_u_s_uclalst_2 | uclalst > outlier_o_s_uclalst_2))

IQR_nc_uclalst <- IQR(d_nc$uclalst, na.rm = TRUE)
(outlier_u_nc_uclalst_2 <- quantile(d_nc$uclalst, probs = 0.25, na.rm = TRUE) - (1.5 * IQR_nc_uclalst))
(outlier_o_nc_uclalst_2 <- quantile(d_nc$uclalst, probs = 0.75, na.rm = TRUE) + (1.5 * IQR_nc_uclalst))
(outlier_nc_uclalst_2 <- d_nc %>% filter(uclalst < outlier_u_nc_uclalst_2 | uclalst > outlier_o_nc_uclalst_2))
## Es gibt insgesamt 9 Ausreisser in der Schizophreniegruppe und 1 Ausreisser in der gesunden Gruppe.

## Die Normalverteilung wurde schon im Abschnitt 2.5 (Zeile 330 - 333) durchgeführt.

## Levenes Test für Varianzhomogenität für die Schizophreniegruppe:

## Geschlecht:
leveneTest(d_sch$uclalst, d_sch$gender)
## Das Ergebnis ist nicht signifikant und somit kann die Varianzhomogenität angenommen werden.

## Familienstand:
leveneTest(d_sch$uclalst, d_sch$marital2)
## Das Ergebnis ist nicht signifikant und somit kann die Varianzhomogenität angenommen werden.

## Levenes Test für Varianzhomogenität für die gesunde Gruppe:

## Geschlecht:
leveneTest(d_nc$uclalst, d_nc$gender)
## Das Ergebnis ist nicht signifikant und somit kann die Varianzhomogenität angenommen werden.

## Familienstand:
leveneTest(d_nc$uclalst, d_nc$marital2)
## Auch dieses Ergebnis ist nicht signifikant und die Varianzhomogenität kann angenommen werden.

#####3.1 Resultat: Reproduktion Bifaktormodell#####

## Das Bifaktor Modell:
model1 <- '
    # Messmodell
    P=~  uclals01r+uclals05r+uclals06r+uclals09r+uclals10r+uclals15r+uclals16r+uclals19r+uclals20r 
    N=~  uclals02+uclals03+uclals04+uclals07+uclals08+uclals11+uclals12+uclals13+uclals14+uclals17+uclals18
    L=~  uclals01r+uclals02+uclals03+uclals04+uclals05r+uclals06r+uclals07+uclals08+uclals09r+
                  uclals10r+uclals11+uclals12+uclals13+uclals14+uclals15r+uclals16r+uclals17+uclals18+
                  uclals19r+uclals20r 
    
    #Restriktionen
    L ~~ 0 * P
    L ~~ 0 * N
    P ~~ 0 * N
'
## Fakorenanalyse für die Schizophreniegruppe:
model1.1.fit <- sem(model1, estimator = "WLSMV", data = d_sch)
summary(model1.1.fit, fit.measures = TRUE)
## Hier stimmen alle Fit-Indices, nur SRMR stimmt nicht überein.

## Faktorenanalyse für die Kontrollgruppe:
model1.2.fit <- sem(model1, estimator = "WLSMV", data = d_nc)
summary(model1.2.fit,  fit.measures = TRUE)
## So erhalte ich keine Fit-Indices.

model1.3.fit <- sem(model1, estimator = "WLSMV", ordered = TRUE, data = d_nc)
summary(model1.3.fit,  fit.measures = TRUE)
## Bei diesem Bifaktor Modell erhalte ich nur die Freiheitsgrade gleich wie im Paper, die Fit-Indices unterscheiden sich.


## Wie sind die Faktorenladungen für die Gruppe Schizophrenie?
ladung_sch0 <- inspect(model1.1.fit, "std")$lambda
print(ladung_sch0)
## Bei allen Faktoren gibt es Werte für die jeweiligen Items.

## Wie sind die Faktorenladungen für die gesunde Gruppe?
ladung_nc0 <- inspect(model1.3.fit, "std")$lambda
print(ladung_nc0)
## Bei dem Faktor "Positiv" sind die Ladungen NAs.

## Wie unterscheiden sich die Faktorenladungen von Eglit et al. (2018) mit den reproduzierten Ergebnissen?
## Barplots werden zur veranschaulichung benutzt.

## Dafür zuerst die Ladungen von Eglit et al. (2018) zusammenfügen:
ladung_nc_eglit <- data.frame(
  Item = c("uclals01r", "uclals05r", "uclals06r", "uclals09r", "uclals10r", "uclals15r", "uclals16r", "uclals19r", "uclals20r",
           "uclals02", "uclals03",  "uclals04", "uclals07", "uclals08", "uclals11", "uclals12", "uclals13", "uclals14", "uclals17", "uclals18"),
  P = c(0.50, 0.34, 0.33, 0.39, 0.36, 0.33, 0.34, 0.32, 0.33, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  N = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0.20, 0.16, 0.16, 0.16, 0.11, 0.22, 0.22, 0.14, 0.21, 0.36, 0.23),
  L = c(0.42, 0.61, 0.57, 0.36, 0.70, 0.57, 0.63, 0.64, 0.72, 0.67, 0.81, 0.69, 0.85, 0.61, 0.71, 0.60, 0.69, 0.87, 0.23, 0.67)
)


ladung_sch_eglit <- data.frame(
  Item = c("uclals01r", "uclals05r", "uclals06r", "uclals09r", "uclals10r", "uclals15r", "uclals16r", "uclals19r", "uclals20r",
           "uclals02", "uclals03",  "uclals04", "uclals07", "uclals08", "uclals11", "uclals12", "uclals13", "uclals14", "uclals17", "uclals18"),
  P = c(0.73, 0.60, 0.54, 0.54, 0.56, 0.62, 0.55, 0.61, 0.58, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
  N = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0.42, 0.34, 0.35, 0.33, 0.25, 0.43, 0.46, 0.33, 0.46, 0.74, 0.49),
  L = c(0.23, 0.40, 0.36, 0.19, 0.41, 0.40, 0.38, 0.46, 0.47, 0.52, 0.61, 0.53, 0.64, 0.50, 0.51, 0.45, 0.57, 0.66, 0.17, 0.51)
)

## Die Ladungen aus den reproduzierten Daten müssen auch in einem data.frame gespeichert werden mit den Items zur Identifizierung der gleichen Items:
ladung_nc <- tibble::rownames_to_column(as.data.frame(ladung_nc0), var = "Item")
ladung_sch <- tibble::rownames_to_column(as.data.frame(ladung_sch0), var = "Item")

## Zusätzlich erhalten die Datensätze jeweils eine Identifizierung,
## damit man im Plot später gut sehen kann, welche die reproduzierten Ladungen sind, und welche die von Eglit et al. (2018)
ladung_nc$ID <- "Reproduktion"
ladung_sch$ID <- "Reproduktion"
ladung_nc_eglit$ID <- "Eglit et al. (2018)"
ladung_sch_eglit$ID <- "Eglit et al. (2018)"

## Nun müssen beide Datensatz mit den Ladungen zusammengefügt werden:
ladung_nc <- rbind(ladung_nc_eglit, ladung_nc)
ladung_sch <- rbind(ladung_sch_eglit, ladung_sch)

## Namen der Items auf Deutsch Ändern:
ladung_nc$Fragen <- c("Frage 1r", "Frage 5r", "Frage 6r", " Frage 9r", "Frage 10r", "Frage 15r", "Frage 16r", "Frage 19r", "Frage 20r",
                       "Frage 2", "Frage 3",  "Frage 4", "Frage 7", "Frage 8", "Frage 11", "Frage 12", "Frage 13", "Frage 14", "Frage 17", "Frage 18",
                      "Frage 1r", "Frage 5r", "Frage 6r", " Frage 9r", "Frage 10r", "Frage 15r", "Frage 16r", "Frage 19r", "Frage 20r",
                      "Frage 2", "Frage 3",  "Frage 4", "Frage 7", "Frage 8", "Frage 11", "Frage 12", "Frage 13", "Frage 14", "Frage 17", "Frage 18")

ladung_sch$Fragen <- c("Frage 1r", "Frage 5r", "Frage 6r", " Frage 9r", "Frage 10r", "Frage 15r", "Frage 16r", "Frage 19r", "Frage 20r",
                      "Frage 2", "Frage 3",  "Frage 4", "Frage 7", "Frage 8", "Frage 11", "Frage 12", "Frage 13", "Frage 14", "Frage 17", "Frage 18",
                      "Frage 1r", "Frage 5r", "Frage 6r", " Frage 9r", "Frage 10r", "Frage 15r", "Frage 16r", "Frage 19r", "Frage 20r",
                      "Frage 2", "Frage 3",  "Frage 4", "Frage 7", "Frage 8", "Frage 11", "Frage 12", "Frage 13", "Frage 14", "Frage 17", "Frage 18")

ladung_sch_p <- ladung_sch[ladung_sch$Item %in% c("uclals01r", "uclals05r", "uclals06r", "uclals09r",
                                                  "uclals10r", "uclals15r", "uclals16r", "uclals19r", "uclals20r"), ]
ladung_nc_p <- ladung_nc[ladung_nc$Item %in% c("uclals01r", "uclals05r", "uclals06r", "uclals09r",
                                               "uclals10r", "uclals15r", "uclals16r", "uclals19r", "uclals20r"), ]
ladung_sch_n <- ladung_sch[ladung_sch$Item %in% c("uclals02", "uclals03", "uclals04", "uclals07", "uclals08",
                                                  "uclals11", "uclals12","uclals13", "uclals14","uclals17", "uclals18"), ]
ladung_nc_n <- ladung_nc[ladung_nc$Item %in% c("uclals02", "uclals03", "uclals04", "uclals07", "uclals08",
                                               "uclals11", "uclals12","uclals13", "uclals14","uclals17", "uclals18"), ]

## Jetzt lässt es sich mit einem Barplot die Unterschiede gut sehen, zunächst für Schizophreniegruppe:
## Faktor positiv:
(plot_sch_p <- ggplot(ladung_sch_p, aes(x = Fragen, y = P, fill = ID)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(P, digits = 2)), position = position_dodge(1), hjust = 1, size = 4) +
    labs(x = "Fragen", y = "Ladung", fill = "Legende") +
    coord_flip())
## Man sieht, die Ladungen unterscheiden sich bei allen Items.

## Faktor negativ:
(plot_sch_n <- ggplot(ladung_sch_n, aes(x = Fragen, y = N, fill = ID)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(N, digits = 2)), position = position_dodge(1), hjust = 1, size = 4)+
    labs(x = "Fragen", y = "Ladung", fill = "Legende")+
    coord_flip())
## Auch bei dem Faktor "Negativ" unterscheiden sie sich alle.

## Faktor Einsamkeit:
(plot_sch_l <- ggplot(ladung_sch, aes(x = Fragen, y = L, fill = ID)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(L, digits = 2)), position = position_dodge(1), hjust = 1, size = 4) +
    labs(x = "Fragen", y = "Ladung", fill = "Legende")+
    coord_flip())
## Auch bei Einsamkeit unterscheiden sie sich alle.

## Plot für die gesunde Gruppe:
## Faktor positiv:
(plot_nc_p <- ggplot(ladung_nc_p, aes(x = Fragen, y = P, fill = ID)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(P, digits = 2)), position = position_dodge(1), hjust = 1, size = 4) +
    labs(x = "Fragen", y = "Ladung", fill = "Legende")+
    coord_flip())
## Bei der Reproduktion keine Werte erhalten für den Faktor positiv.

## Faktor negativ:
(plot_nc_n <- ggplot(ladung_nc_n, aes(x = Fragen, y = N, fill = ID)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(N, digits = 2)), position = position_dodge(1), hjust = 1, size = 4) +
    labs(x = "Fragen", y = "Ladung", fill = "Legende")+
    coord_flip())
## Für Negativ unterscheiden sich auch alle bis auf Item 18. Wenn die reproduzierten Daten auf 2 Kommastellen
## gerundet werden, ist das Item 18 identisch mit dem Reproduktion.

## Faktor Einsamkeit:
(plot_nc_l <- ggplot(ladung_nc, aes(x = Fragen, y = L, fill = ID)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = round(L, digits = 2)), position = position_dodge(1), hjust = 1, size = 4) +
    labs(x = "Fragen", y = "Ladung", fill = "Legende") +
    coord_flip())
## Auch beim Faktor Einsamkeit sind alle bis auf ein Item unterschiedlich. Item 12 ist, wenn die reproduzierten Daten
## auf 2 Kommastellen gerundet werden identisch mit denen von Eglit et al. (2018).
## Alle Korrelationskoeffizienten, die sich um 0.01 von den Korrelationskoeffizienten von Eglit et al. (2018) unterscheiden
## werden als gleich gewertet.

## Zusammenfügen:
ggarrange(plot_sch_p, plot_sch_n, plot_sch_l, plot_nc_p, plot_nc_n, plot_nc_l,
          ncol = 3, nrow = 2,
          labels = "AUTO",
          font.label = list(size = 15, face = "bold", color = "black")) 

#####3.2 Resultat: Reproduktion Korrelationsanalyse#####
## Alter:
(cor_ageS <- cor.test(d_sch$uclalst, d_sch$agevisit))
(cor_ageNC <- cor.test(d_nc$uclalst, d_nc$agevisit))
# Die Korrelationen in beiden Gruppen sind gleich wie in der Studie.

## Alter von Krankheitsbeginn:
(cor_onsetS <- cor.test(d_sch$uclalst, d_sch$aos))
(cor_onsetNC <- cor.test(d_nc$uclalst, d_nc$aos)) # Hier kein Ergebnis da ja keine Krankheit gibt.
# Im Paper ist die Korrelation -0.19 während ich hier für die Schizophreniegruppe -0.21 erhalte.

## Geschlecht:
(cor_genS <- cor.test(d_sch$gender, d_sch$uclalst))
(cor_genNC <- cor.test(d_nc$gender, d_nc$uclalst))
# Die Korrelaion von Schizophrenie Patienten unterscheiden sich.
# Die bei der Kontrollgruppe stimmt in etwa überein (aufgerundet).

## Ausbildung
(cor_eduS <- cor.test(d_sch$educ, d_sch$uclalst))
(cor_eduNC <- cor.test(d_nc$educ, d_nc$uclalst))
# Stimmt überein mit dem Paper.

## Familienstand:
(cor_marS <- cor.test(d_sch$uclalst, d_sch$marital2)) 
(cor_marNC <- cor.test(d_nc$uclalst, d_nc$marital2)) 
## Hier stimmen die Vorzeichen nicht überein mit dem Paper

## Soziale Position:
(cor_sozS <- cor.test(d_sch$uclalst, d_sch$socposc, method = "spearman"))
(cor_sozNC <- cor.test(d_nc$uclalst, d_nc$socposc, method = "spearman")) 
## Die Korrelationen stimmen mit dem Paper überein.

## Höchste soziale Position:
(cor_hihS <- cor.test(d_sch$uclalst, d_sch$socposh, method = "spearman"))
(cor_hihNC <- cor.test(d_nc$uclalst, d_nc$socposh, method = "spearman"))
## Ergebnisse stimmen überein

## Längste soziale Position:
(cor_lonS <- cor.test(d_sch$uclalst, d_sch$socposl, method = "spearman"))
(cor_lonNC <- cor.test(d_nc$uclalst, d_nc$socposl, method = "spearman"))
## Stimmen auch mit Paper überein.

## Persönliches Einkommen:
## Antworten mit 9 rausnehmen:
schizophrenie.incp_no9 <- d_sch %>% filter(income_p != 9)
range(schizophrenie.incp_no9$income_p)

nc.incp_no9 <- d_nc %>% filter(income_p != 9)
range(nc.incp_no9$income_p)

(cor_incS <- cor.test(schizophrenie.incp_no9$uclalst, schizophrenie.incp_no9$income_p, method = "spearman"))
(cor_incNC <- cor.test(nc.incp_no9$uclalst, nc.incp_no9$income_p, method = "spearman"))
## die Ergebnisse stimmen überein.

## Familiäres Einkommen:

## Antworten mit 9 rausnehmen:
schizophrenie.incf_no9 <- d_sch %>% filter(income_f != 9)
range(schizophrenie.incf_no9$income_f)

nc.incf_no9 <- d_nc %>% filter(income_f != 9)
range(nc.incf_no9$income_f)


(cor_incfS <- cor.test(schizophrenie.incf_no9$uclalst, schizophrenie.incf_no9$income_f, method = "spearman"))
(cor_incfNC <- cor.test(nc.incf_no9$uclalst, nc.incf_no9$income_f, method = "spearman"))
## In der Schizophreniegruppe erhalte ich kein signifikantes Ergebnis, im Paper wird jedoch dieses Ergebnis
## als signifikant beschrieben.

## Antipsychotische Medikation:
(cor_medS <- cor.test(d_sch$uclalst, d_sch$apdddeq))
(cor_medNC <- cor.test(d_nc$uclalst, d_nc$apdddeq))
## Stimmt.

## Postiive Symptome (SAPS Total):
(cor_sapsS <- cor.test(d_sch$uclalst, d_sch$sapstot))
## Da die ungültigen Werte rausgenommen wurden, stimmt die Korrelation nicht überein.

## Negative Symptome (SANS Total):
(cor_sansS <- cor.test(d_sch$uclalst, d_sch$sanstot))
## Auch hier stimmen die Korrelationen nicht überein, da der ungültige Wert rausgenommen wurde.

## Depression (CDSS Total):
(cor_depS <- cor.test(d_sch$uclalst, d_sch$cdrstot))
(cor_depNC <- cor.test(d_nc$uclalst, d_nc$cdrstot))
## Die Ergebnisse stimmen überein.

## Ängste (BSIAS Total):
(cor_anxS <- cor.test(d_sch$uclalst, d_sch$bsiatot))
(cor_anxNC <- cor.test(d_nc$uclalst, d_nc$bsiatot))
## Die Korrelationen stimmen überein. Der p-Wert scheint viel kleiner zu sein als im Paper von der gesunden Gruppe.

## Exekutive Funktion:
(cor_exeS <- cor.test(d_sch$uclalst, d_sch$EXCOMP2))
(cor_exeNC <- cor.test(d_nc$uclalst, d_nc$EXCOMP2))
## Die Ergebnisse stimmen überein.

## Physikalische Gesundheit:
(cor_phyS <- cor.test(d_sch$uclalst, d_sch$phycomp))
(cor_phyNC <- cor.test(d_nc$uclalst, d_nc$phycomp))
## Die Ergebnisse stimmen überein.

## Mentale Gesundheit:
(cor_menS <- cor.test(d_sch$uclalst, d_sch$mencomp))
(cor_menNC <- cor.test(d_nc$uclalst, d_nc$mencomp))
## Auch hier stimmen die Ergebnisse überein.

## Happiness (CES-D Total):
(cor_hapS <- cor.test(d_sch$uclalst, d_sch$cesdhs))
(cor_hapNC <- cor.test(d_nc$uclalst, d_nc$cesdhs))
## Stimmen überein.

## Resilienz (CDR Total):
(cor_resS <- cor.test(d_sch$uclalst, d_sch$cdrs10_t))
(cor_resNC <- cor.test(d_nc$uclalst, d_nc$cdrs10_t))
## Korrelationen stimmen überein. Nur der p-Wert von der Schizophreniegruppe scheint kleiner zu sein als im Paper.

## Optimismus (LOT-R Total):
(cor_optS <- cor.test(d_sch$uclalst, d_sch$lotrt))
(cor_optNC <- cor.test(d_nc$uclalst, d_nc$lotrt))
## Stimmen überein.

## Stress (PSS Total):
(cor_strS <- cor.test(d_sch$uclalst, d_sch$psstot))
(cor_strNC <- cor.test(d_nc$uclalst, d_nc$psstot))
## Stimmt.

## Lebenszufriedenheit (SWLS Total):
(cor_lifS <- cor.test(d_sch$uclalst, d_sch$swlstot))
(cor_lifNC <- cor.test(d_nc$uclalst, d_nc$swlstot))
## Auch das Stimmt überein.

#####3.3 Resultat: Vergleich CART-Modell mit Faktorenanalyse#####
## Da der cp auf 0 gestellt wird, wird das Originalmodell erstellt.

## Auch hier sollen zunächst die Variablennamen geändert werden:


## CART-Modell für Faktor Einsamkeit für Schizophreniegruppe:
lon_sch <- rpart(uclalst ~ Frage.1r+Frage.2+Frage.3+Frage.4+Frage.5r+Frage.6r+Frage.7+Frage.8+Frage.9r+
                   Frage.10r+Frage.11+Frage.12+Frage.13+Frage.14+Frage.15r+Frage.16r+Frage.17+Frage.18+
                   Frage.19r+Frage.20r, method = "anova", cp = 0, data = d_sch_name)

## Wie sieht das Modell aus?
rpart.plot(lon_sch, extra = 1, fallen.leaves = TRUE)

## Welche Rangordnung haben die Variablen?
lon_sch$variable.importance
## Das Item 14 scheint eine optimale Frage für die Einsamkeit in der Schizophreniegruppe zu sein.
## Deshalb ist sie ganz oben im Wurzelknoten.

## CART-Modell für den Vergleich zum Faktor Positiv für Schizophreniegruppe:
pos_sch <- rpart(uclalst ~ Frage.1r+Frage.5r+Frage.6r+Frage.9r+Frage.10r+Frage.15r+Frage.16r+
                   Frage.19r+Frage.20r, method = "anova", cp = 0, data = d_sch_name)
## Wie sieht das Modell aus?
rpart.plot(pos_sch, extra = 1, fallen.leaves = TRUE)

## Welche sind die wichtigen Variablen?
pos_sch$variable.importance
## Variable 19 scheint die optimalste Frage zu sein. In diesem Modell ist die Frage 19 zuoberst.

## Das CART-Modell für den Faktor Negativ:
neg_sch <- rpart(uclalst ~ Frage.2+Frage.3+Frage.4+Frage.7+Frage.8+
                   Frage.11+Frage.12+Frage.13+Frage.14+Frage.17+Frage.18, method = "anova", cp = 0,
                 data = d_sch_name)
## Wie sieht das Modell aus?
rpart.plot(neg_sch, extra = 1, fallen.leaves = TRUE)

## Welche sind die wichtigen Items?
neg_sch$variable.importance
## Wie bei dem Faktor Einsamkeit ist das Item 14 die optimalste Frage und ist auch im Modell zuoberst.

## Wie sieht es mit der Kontrollgruppe aus?
## CART-Modell für Faktor Einsamkeit für gesunde Gruppe:
lon_nc <- rpart(uclalst ~ Frage.1r+Frage.2+Frage.3+Frage.4+Frage.5r+Frage.6r+Frage.7+Frage.8+Frage.9r+
                  Frage.10r+Frage.11+Frage.12+Frage.13+Frage.14+Frage.15r+Frage.16r+Frage.17+Frage.18+
                  Frage.19r+Frage.20r, cp = 0, method = "anova", data = d_nc_name)
## Wie sieht das Modell aus?
rpart.plot(lon_nc, extra = 1, fallen.leaves = TRUE)

## Welche sind die wichtigen Fragen?
lon_nc$variable.importance
## Frage 20 ist die optimalste Frage nach diesem Modell.

## CART-Modell für Faktor positiv für gesunde Gruppe:
pos_nc <- rpart(uclalst ~ Frage.1r+Frage.5r+Frage.6r+Frage.9r+Frage.10r+Frage.15r+Frage.16r+
                  Frage.19r+Frage.20r, cp = 0, method = "anova", data = d_nc_name)
## Wie sieht das Modell aus?
rpart.plot(pos_nc, extra = 1, fallen.leaves = TRUE)

## Welche sind die wichtigen Variablen?
pos_nc$variable.importance
## Das Item 20.

## Das CART-Modell für den Faktor Negativ:
neg_nc <- rpart(uclalst ~ Frage.2+Frage.3+Frage.4+Frage.7+Frage.8+
                  Frage.11+Frage.12+Frage.13+Frage.14+Frage.17+Frage.18, method = "anova", cp = 0, data = d_nc_name)
## Wie sieht das Modell aus?
rpart.plot(neg_nc, extra = 1, fallen.leaves = TRUE)

## Welche sind die wichtigen Items?
neg_nc$variable.importance
## Die Frage 7 ist die optimalste Frage nach diesem Modell.

#####3.4 Resultat: Vergleich der Korrelationskoeffizienten#####

## ACHTUNG: Hier werden wieder die originalen Variablennamen (so wie im Codebook) verwendet.
## Variablen anpassen:
d$marital2[d_sch$marital2==102] <- 0 # nicht Verheiratet
d$marital2[d_sch$marital2==101] <- 1 # Verheiratet
d$sapstot[d$sapstot<0] <- NA
d$sanstot[d$sanstot<0] <- NA
d$income_p[d$income_p==9] <- NA
d$income_f[d$income_f==9] <- NA

d_sch <- d %>% filter(Diagnosis == 1)
d_nc <- d %>% filter(Diagnosis == 0)

## Was wenn man den Summenscore für die ausgewählten UCLA-3 Items (nach dem CART-Modell für Einsamkeit) berechnet,
## wie hoch ist die Korrelation dann:
selectedItems_sch <- c("uclals03", "uclals07", "uclals14", "uclals15r", "uclals18", "uclals19r")
ucla3_sum6_sch <- apply(d_sch[,selectedItems_sch], 1, sum)
cor(d_sch$uclalst, ucla3_sum6_sch)
## r = 0.94

## Hänge "neuen" Summenscore an d_sch an:
d_sch$ucla3_sum6_sch <- ucla3_sum6_sch

## Was wenn man den Summenscore für die ausgewählten UCLA-3 Items (nach dem CART-Modell für Einsamkeit) berechnet,
## wie hoch ist die Korrelation dann:
selectedItems_nc <- c("uclals07", "uclals08", "uclals09r", "uclals11", "uclals13", "uclals18", "uclals19r", "uclals20r")
ucla3_sum8_nc <- apply(d_nc[,selectedItems_nc], 1, sum)
cor(d_nc$uclalst, ucla3_sum8_nc)
## r = 0.97

## Hänge "neuen" Summenscore an d_sch an:
d_nc$ucla3_sum8_nc <- ucla3_sum8_nc

## Gruppe Schizophrenie
table5TopToBottomSchizo <- c(
  # Sociodemographic and clinical characteristics
  "agevisit", "aos", "gender", "educ", "marital2", "socposc",
  "socposh", "socposl", "income_p", "income_f", "apdddeq",
  # Severity of psychopathology, cognition, and well-being
  "sapstot", "sanstot", "cdrstot", "bsiatot", "EXCOMP2", "phycomp", "mencomp",
  # Positive psychological characteristics
  "cesdhs", "cdrs10_t", "lotrt", "psstot", "swlstot"
)

corMethodSchizo <- c(
  # Sociodemographic and clinical characteristics
  1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1,
  # Severity of psychopathology, cognition, and well-being
  1, 1, 1, 1, 1, 1, 1,
  # Positive psychological characteristics
  1, 1, 1, 1, 1)

## Einzige logische Abweichung von Eglit et al. (2018) bezieht sich auf marital2 (spearman anstatt pearson),
## weil wenn gender mit spearman gerechnet wird, muss auch marital2 so berechnet werden, weil beide Variablen
## dichotom sind.
data.frame(table5TopToBottomSchizo, corMethodSchizo)

corSchizo1 <- corSchizo2 <- corSchizo3 <- c()
corNeuSchizo1 <- corNeuSchizo2 <- corNeuSchizo3 <- c()
## method = "pearson", "spearman", or "kendall"
## i <- 5
for(i in 1:length(table5TopToBottomSchizo)) {
  
  col_i <- table5TopToBottomSchizo[i]
  
  corSchizo1 <- c(corSchizo1,
                  cor(d_sch[,col_i], d_sch$uclalst, use = "complete.obs", method = "pearson"))
  
  corSchizo2 <- c(corSchizo2,
                  cor(d_sch[,col_i], d_sch$uclalst, use = "complete.obs", method = "spearman"))
  
  # Point biserial correlation (only if the outcome is dichotomous)
  if(length(unique(d_sch[[col_i]]))==2) {
    cortestRes <- cor.test(y=d_sch[[col_i]], x=d_sch$uclalst)
    corSchizo3 <- c(corSchizo3, cortestRes$estimate)
    # # Alternative, falls gewünscht, um point biseriale Korrelation zu berechnen:
    # ltm::biserial.cor(x=d_sch$uclalst, y=d_sch[[col_i]], use="complete.obs", level=2)
  } else {
    corSchizo3 <- c(corSchizo3, NA)
  }
  
  corNeuSchizo1 <- c(corNeuSchizo1,
                     cor(d_sch[,col_i], d_sch$ucla3_sum6_sch, use = "complete.obs", method = "pearson"))
  
  corNeuSchizo2 <- c(corNeuSchizo2,
                     cor(d_sch[,col_i], d_sch$ucla3_sum6_sch, use = "complete.obs", method = "spearman"))
  
  # Point biserial correlation (only if the outcome is dichotomous)
  if(length(unique(d_sch[[col_i]]))==2) {
    corNeuTestRes <- cor.test(y=d_sch[[col_i]], x=d_sch$ucla3_sum6_sch)
    corNeuSchizo3 <- c(corNeuSchizo3, cortestRes$estimate)
  } else {
    corNeuSchizo3 <- c(corNeuSchizo3, NA)
  }
}
# 
corSchizoDf <-
  data.frame(col1=table5TopToBottomSchizo,
             pearson=round(corSchizo1, digits=2), spearman=round(corSchizo2, digits=2),
             pointBiserial=round(corSchizo3, digits=2),
             pearsonCart=round(corNeuSchizo1, digits=2), spearmanCart=round(corNeuSchizo2, digits=2),
             pointBiserialCart=round(corNeuSchizo3, digits=2),
             corMethodSchizo)

## Setze Wert auf 2 in Spalte corMethodSchizo für point biseriale Korrelation
corSchizoDf$corMethodSchizo[!is.na(corSchizoDf$pointBiserial)] <- 2
corSchizoDf

# cocorSchizo <- list()
cocorSchizoPearsonFilon_z <- cocorSchizoPearsonFilon_p <- c()
for(i in 1:nrow(corSchizoDf)) {
  if(corSchizoDf$corMethodSchizo[i]==1) {
    rRef <- corSchizoDf$pearson[i]
    rCart <- corSchizoDf$pearsonCart[i]
  } else if(corSchizoDf$corMethodSchizo[i]==0) {
    rRef <- corSchizoDf$spearman[i]
    rCart <- corSchizoDf$spearmanCart[i]
  } else {
    rRef <- corSchizoDf$pointBiserial[i]
    rCart <- corSchizoDf$pointBiserialCart[i]
  }
  
  # cocorSchizo[[corSchizoDf$col1[i]]] <-
  #     cocor::cocor.dep.groups.overlap(r.jk = rRef, r.jh = rCart, r.kh = .9, n=nrow(d_sch))
  
  # pfz: pearson filon z value
  pfz <- cocor::cocor.dep.groups.overlap(r.jk = rRef, r.jh = rCart, r.kh = .94, n=nrow(d_sch), test = "pearson1898")
  # Collect z result and corresponding p value
  cocorSchizoPearsonFilon_z <- c(cocorSchizoPearsonFilon_z, pfz@pearson1898$statistic)
  cocorSchizoPearsonFilon_p <- c(cocorSchizoPearsonFilon_p, pfz@pearson1898$p.value)
  
}
corSchizoDf$z <- cocorSchizoPearsonFilon_z
corSchizoDf$pVal <- cocorSchizoPearsonFilon_p
## Show in R console:
## Spalte corMethodSchizo: 1 = Pearson, 0 = Spearman, 2 = Point biserial
corSchizoDf

## Effektgrössen:
## Alter:
fisherz(-0.05) - fisherz(-0.07)
## q = 0.02

## Alter bei Krankheitsbeginn:
fisherz(-0.21) - fisherz(-0.24)
## q = 0.03

## Geschlecht:
fisherz(-0.06) - fisherz(-0.06)
## q = 0

## Ausbildung:
fisherz(-0.09) - fisherz(-0.15)
## q = 0.06

## Familienstand:
fisherz(0.01) - fisherz(0.01)
## q = 0

## Soziale Position:
fisherz(0.19) - fisherz(0.17)
## q = 0.02

## Höchste soziale Position:
fisherz(0.17) - fisherz(0.14)
## q = 0.03

## Längste soziale Position:
fisherz(0.16) - fisherz(0.12)
## q = 0.04

## Persönliches Einkommen:
fisherz(0.02) - fisherz(-0.01)
## q = 0.03

## Familiäres Einkommen:
fisherz(-0.14) - fisherz(-0.25)
## q = 0.11

## Antipsychotikum:
fisherz(0.05) - fisherz(0.05)
## q = 0

## Positivsymptome:
fisherz(.48) - fisherz(.43)
## q = 0.06

## Negativsymptome:
fisherz(.11) - fisherz(.10)
## q = 0.01

## Depression:
fisherz(.46) - fisherz(.44)
## q = 0.03

## Angst:
fisherz(.39) - fisherz(.39)
## q = 0

## Exekutive Funktionen:
fisherz(.11) - fisherz(.11)
## q = 0

## Physische Gesundheit:
fisherz(-.12) - fisherz(-.13)
## q = 0.01

## Mentale Gesundheit:
fisherz(-.34) - fisherz(-.40)
## q = 0.07

## Glücklichkeit:
fisherz(-.36) - fisherz(-.41)
## q = 0.06

## Resilienz:
fisherz(-.32) - fisherz(-.36)
## q = 0.05

## Optimismus:
fisherz(-.25) - fisherz(-.31)
## q = 0.07

## Erlebter Stress:
fisherz(.43) - fisherz(.38)
## q = 0.06

## Lebenszufriedenheit:
fisherz(-.24) - fisherz(-.25)
## q = 0.01

## Kontrollgruppe

table5TopToBottomNc <- c(
  # Sociodemographic and clinical characteristics
  "agevisit", "gender", "educ", "marital2", "socposc", 
  "socposh", "socposl", "income_p", "income_f",
  # Severity of psychopathology, cognition, and well-being
  "cdrstot", "bsiatot", "EXCOMP2", "phycomp", "mencomp",
  # Positive psychological characteristics
  "cesdhs", "cdrs10_t", "lotrt", "psstot", "swlstot"
)

corMethodNc <- c(
  # Sociodemographic and clinical characteristics
  1, 0, 1, 0, 0, 0, 0, 0, 0,
  # Severity of psychopathology, cognition, and well-being
  1, 1, 1, 1, 1,
  # Positive psychological characteristics
  1, 1, 1, 1, 1)

data.frame(table5TopToBottomNc, corMethodNc)

corNc1 <- corNc2 <- corNc3 <- c()
corNeuNc1 <- corNeuNc2 <- corNeuNc3 <- c()
# method = "pearson", "spearman", or "kendall"
for(i in 1:length(table5TopToBottomNc)) {
  
  col_i <- table5TopToBottomNc[i]
  
  corNc1 <- c(corNc1,
              cor(d_nc[,col_i], d_nc$uclalst, use = "complete.obs", method = "pearson"))
  
  corNc2 <- c(corNc2,
              cor(d_nc[,col_i], d_nc$uclalst, use = "complete.obs", method = "spearman"))
  
  # Point biserial correlation (only if the outcome is dichotomous)
  if(length(unique(d_nc[[col_i]]))==2) {
    cortestRes <- cor.test(y=d_nc[[col_i]], x=d_nc$uclalst)
    corNc3 <- c(corNc3, cortestRes$estimate)
  } else {
    corNc3 <- c(corNc3, NA)
  }
  
  corNeuNc1 <- c(corNeuNc1,
                 cor(d_nc[,col_i], d_nc$ucla3_sum8_nc, use = "complete.obs", method = "pearson"))
  
  corNeuNc2 <- c(corNeuNc2,
                 cor(d_nc[,col_i], d_nc$ucla3_sum8_nc, use = "complete.obs", method = "pearson"))
  
  # Point biserial correlation (only if the outcome is dichotomous)
  if(length(unique(d_nc[[col_i]]))==2) {
    corNeuTestRes <- cor.test(y=d_nc[[col_i]], x=d_nc$ucla3_sum8_nc)
    corNeuNc3 <- c(corNeuNc3, corNeuTestRes$estimate)
  } else {
    corNeuNc3 <- c(corNeuNc3, NA)
  }
}
# 
corNcDf <-
  data.frame(col1=table5TopToBottomNc,
             pearson=round(corNc1, digits=2), spearman=round(corNc2, digits=2),
             pointBiserial=round(corNc3, digits=2),
             pearsonCart=round(corNeuNc1, digits=2), spearmanCart=round(corNeuNc2, digits=2),
             pointBiserialCart=round(corNeuNc3, digits=2),
             corMethodNc)

## Setze Wert auf 2 in Spalte corMethodSchizo für point biseriale Korrelation
corNcDf$corMethodNc[!is.na(corNcDf$pointBiserial)] <- 2
corNcDf

# cocorNc <- list()
cocorNcPearsonFilon_z <- cocorNcPearsonFilon_p <- c()
for(i in 1:nrow(corNcDf)) {
  if(corNcDf$corMethodNc[i]==1) {
    rRef <- corNcDf$pearson[i]
    rCart <- corNcDf$pearsonCart[i]
  } else if(corNcDf$corMethodNc[i]==0) {
    rRef <- corNcDf$spearman[i]
    rCart <- corNcDf$spearmanCart[i]
  } else {
    rRef <- corNcDf$pointBiserial[i]
    rCart <- corNcDf$pointBiserialCart[i]
  }
  
  # cocorNc[[corNcDf$col1[i]]] <-
  #     cocor::cocor.dep.groups.overlap(r.jk = rRef, r.jh = rCart, r.kh = .9, n=nrow(d_nc))
  
  # pfz: pearson filon z value
  pfz <- cocor::cocor.dep.groups.overlap(r.jk = rRef, r.jh = rCart, r.kh = .97, n=nrow(d_nc), test = "pearson1898")
  # Collect z result and corresponding p value
  cocorNcPearsonFilon_z <- c(cocorNcPearsonFilon_z, pfz@pearson1898$statistic)
  cocorNcPearsonFilon_p <- c(cocorNcPearsonFilon_p, pfz@pearson1898$p.value)
  
}
corNcDf$z <- cocorNcPearsonFilon_z
corNcDf$pVal <- cocorNcPearsonFilon_p
# Show in R console:
# Spalte corMethodNc: 1 = Pearson, 0 = Spearman, 2 = Point biserial
corNcDf

## Effektgrössen:
## Alter:
fisherz(.26) - fisherz(.23)
## q = 0.03

## Geschlecht:
fisherz(.00) - fisherz(-.02)
## q = 0.02

## Ausbildung:
fisherz(-.05) - fisherz(-.08)
## q = 0.03

## Familienstand:
fisherz(.24) - fisherz(.23)
## q = 0.01

## Soziale Position:
fisherz(.23) - fisherz(.20)
## q = 0.03

## Höchste soziale Position:
fisherz(.18) - fisherz(.15)
## q = 0.03

## Längste soziale Position:
fisherz(.20) - fisherz(.15)
## q = 0.05

## Persönliches Einkommen:
fisherz(-.33) - fisherz(-.33)
## q = 0

## Familiäres Einkommen:
fisherz(-.18) - fisherz(-.24)
## q = 0.06

## Depression:
fisherz(.42) - fisherz(.39)
## q = 0.04

## Angst:
fisherz(.37) - fisherz(.36)
## q = 0.01

## Exekutive Funktionen:
fisherz(-.10) - fisherz(-.10)
## q = 0

## Physische Gesundheit:
fisherz(-.31) - fisherz(-.32)
## q = 0.01

## Mentale Gesundheit:
fisherz(-.40) - fisherz(-.45)
## q = 0.06

## Glücklichkeit:
fisherz(-.54) - fisherz(-.61)
## q = 0.10

## Resilienz:
fisherz(-.48) - fisherz(-.54)
## q = 0.08

## Optimismus:
fisherz(-.50) - fisherz(-.57)
## q = 0.10

## Erlebter Stress:
fisherz(.57) - fisherz(.53)
## q = 0.06

## Lebenszufriedenheit:
fisherz(-.57) - fisherz(-.61)
## q = 0.06

