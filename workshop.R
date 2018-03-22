
## Wymagane pakiety 
library(dplyr)
library(ggplot2)
library(reshape2)
library(readxl) 

library(pROC)
library(zoo)

## Przetwarzanie i analiza danych 

## Folder roboczy
input_path <- "C:/Users/sam/Documents/Nauka/KNMF_workshop"

# Wczytanie danych z pominiêciem pierwszej lini, która opisuje Ÿród³o
loanbook <- read.csv(skip = 1, file = file.path(input_path,"LoanStats3a.csv"),
                     skipNul = T)

## Przegl¹d danych
dim(loanbook)
colnames(loanbook)
## Ktorych danych brakuje, typ
str(loanbook)

## Zamien wartoœæ int_rate oraz revol_util na zmienne character poniewa¿ s¹ w formacie character 
## funkcja sub()

sub("is not","is","Today is not great wheater")


## Konwersja int_rate oraz revol_util na zmienne numeryczne.
## Ozn: Revolving line utilization rate, or the amount of credit the borrower is using 
## relative to all available revolving credit.

loanbook <- loanbook %>% mutate(int_rate = as.numeric(sub("%", "", int_rate))/100,
                                revol_util = as.numeric(sub("%", "", revol_util))/100)

summary(loanbook)

## S³ownik bazy danych
dataDictionary <- read_excel(path = file.path(input_path,"LCDataDictionary.xlsx"))
colnames(dataDictionary)                           

## Opisy dostêpne w s³owniku
dd_names <- as.character(na.omit(dataDictionary$LoanStatNew))
View(dd_names)
## Opisy dostêpne w bazie danych
loanbook_names <- names(loanbook)

## Zmienne brakuj¹ce w zbiorze danych (pomimo ¿e s¹ w s³owniku)
diff <- setdiff(dd_names, loanbook_names)


## Analiza portfela

## Wartoœæ po¿yczkek zmienia³a siê w czasie
## Dywersyfikacja, po¿yczki na ró¿nych grada'ch s¹ udzielane
loanbook_grade <- loanbook %>% select(issue_d,loan_amnt,grade) %>%
  mutate(issue_d = substr(issue_d,5,8)) 

## Na podstawie wybranych zmiennych usuwamy brakujace wartoœci
loanbook_grade <- na.omit(loanbook_grade)

amnt_df_grade <- loanbook_grade %>% 
select(issue_d , loan_amnt, grade) %>% 
group_by(issue_d, grade) %>% # grupujemy obserwacje po dacie po¿yczki i grade
summarise(Amount = sum(loan_amnt)/1000000) # Suma po¿yczki dla tych grup, wyswietlane w mln


cbPalette <- 
  c("#999999", "#E69F00",
    "#009999",  "#993300", "#009E73", 
    "#CC6633", "#0072B2")

ts_amnt_grade <- ggplot(amnt_df_grade,aes(x = issue_d, y = Amount, fill = grade))

## stat definuje po³aczenie miêdzy geom_bar i stat_count, jak chcemy zliczaæ obserwacje
ts_amnt_grade + geom_bar(stat = "identity") 

## Formatowanie 
ts_amnt_grade + geom_bar(stat = "identity") + 
  xlab("Data po¿yczki") + ylab("Kwota (mn)") +
  theme_bw() + scale_fill_manual(values=cbPalette) +
    ggtitle("Rozmiar portfela po¿yczek 2007 - 2011 (wg poziomów ryzka)")

## Omówienie

## w zale¿noœci od poziomu ryzyka po¿yczki sa udzielane na wiêksze kwoty 
## Bank nie chce po¿yczaæ grade -G, chêtnie po¿ycza A,B,C, 
## czêœæ portfela stanowi¹ po¿yczki grade D i E

## Z poprzedniej analizy wnioskujemy ¿e w ka¿dym roku przybywa³o po¿yczek 
## Rozk³ad pomiêdzy grade wzrasta³ proporcjonalnie, rok po¿yczki nie zmienia rozk³adu


## Æwiczenie do wykonania - niekoniecznie
## Poziom ryzyka a stopa procentowa kredytu 
## Wybierz ze zbioru danych zmienne int_rate, loan_amnt, grade, issue_d
## datê po¿yczki zgrupuj do rocznych, narysuj wykres punktowy udzielanych po¿yczek
## int_rate w zale¿noœci od grade, pogrupuj kolorem po dacie po¿yczki

# ---------------Tutaj jest miejsce na Twoje rozwi¹zanie! 













loanbook_int <- loanbook %>% select(issue_d,loan_amnt,int_rate,grade)  %>%
  mutate(issue_d = substr(issue_d,5,8)) 
loanbook_int <- na.omit(loanbook_int)
## Stopy procentowe w zale¿noœci od grade, stopa zalezy jeszcze od kwoty pozyczki
## subgrade, innych czynnikow dlatego ma zakresy ok 5 %

ggplot(loanbook_int, aes(x = grade, y = int_rate)) + geom_point() 

## Formatowanie
ggplot(loanbook_int, aes(x = grade, y = int_rate)) + geom_point(aes(col = issue_d)) +
  xlab("Grade") + ylab("Stopa procentowa") + theme_bw() + 
  ggtitle("Zale¿noœci miêdzy stop¹ procentow¹ po¿yczek a jej poziomem ryzyka")
  

## Im wy¿szy poziom ryzyka, tym stopy procentowe powinny byæ wy¿sze dla naszego modelu
## Po dodaniu koloru okazuje siê ¿e rok rocznie stopy te¿ siê zmienialy 

## Budujemy zmienne dla naszego modelu 


## Analiza przewidywalnoœci defaultu za pomoc¹ danych - zmiennych objasnianych 
## Wybierz zmienne aby ograniczyæ zbiór do istotnego podzbioru danych

## Wybierz pozdbiór zmiennych
variables <- c("id", "loan_amnt", "term", "int_rate", "installment", 
               "grade", "sub_grade", "emp_length", "home_ownership", 
               "annual_inc", "issue_d",
               "loan_status", "purpose", "addr_state", "dti", 
               "delinq_2yrs", "earliest_cr_line", "inq_last_6mths", 
               "mths_since_last_delinq", "mths_since_last_record", 
               "open_acc", 
               "pub_rec", "revol_bal", "revol_util", "total_acc", 
               "initial_list_status", "collections_12_mths_ex_med", 
               "mths_since_last_major_derog")
loanbook_sub <- loanbook[,variables]

# Jeœli status jest  c("Current", "Fully Paid") to po¿yczka ma satus wykonania zobowi¹zania
# Jeœli inny - czyli charged off - to niewykonanie zobowi¹zania
# 1 default; 0 brak defaultu

# Dodajemy now¹ po¿yczkê w któej okreœlamy czy rekord ma status default czy nie
loanbook_sub$default <- factor(ifelse(loanbook_sub$loan_status %in% 
                                           c("Current", "Fully Paid"), 0, 1))

## Jak dostosowaæ stopê procentow¹ maj¹c dane ró¿ne profile ryzyka po¿yczkobiorców
## Po¿yczki w odpowiednim grade i subgrade maj¹ odpowiadaj¹ce stopy prcoentowe

##################################################################
## Analiza mocy zmiennych do przewidywania, ANALIZA ZALE¯NOŒCI 
##
##
##
##
##################################################################
## Zale¿noœæ od zmienej objaœnianej grade


loanbook_def <- na.omit(loanbook_sub %>%  mutate(issue_d = substr(issue_d,5,8)) %>%
#                          filter(grade %in% c("A","B","C","D","E","F","G")) %>%
                           select(grade, default, issue_d))
ggplot(loanbook_def , aes(grade, y = default, fill = default)) + 
  geom_bar( position = "fill", stat = 'identity') +
  theme_bw() + xlab("Status po¿yczki") +
  ylab('Liczba po¿yczek (100%)') + theme(legend.position = 'bottom') +
  ggtitle('Liczba sp³aconych vs. niesp³aconych')

## Tablica kontyngencji tk <- with(loanbook_sub, table(grade, default)) 
## Test Fishera sprawdza czy cecha default jest zale¿na od zmiennej grade
## Hipoteza H0: zmienne sa niezalezne przeciw H1: zmienne sa zalezne

## Test statystyczny: idea (s³owa kluczowe, p wartoœæ, test, b³ad pierwszego rodzaju,
## testy jenostajnie najmnocniejsze)
## Tworzymy statystykê testow¹ (np. opart¹ na ilorazie wiarogodnoœci)
## P(Statystyka < c_alpha) > alpha - poziom_istotnoœci(zadany w teœcie)
## W powy¿szy sposób obliczamy obszar krytyczny tzw. obszar odrzucenia hipotezy zerowej
## który wyznacza dla c_aplha
## Testy jednostajnie najmocniejsze przy ograniczeniu dla b³êdu pierwszego rodzaju
## Minimalizuj¹ b³¹d pierwszego rodzaju 
## B³¹d pierwszego rodzaju odrzucenie H0 gdy prawdziwa 
## B³¹d drugiego rodzaju odrzucenie H1 gdy prawdziwa 
## p wartoœæ tesu, dla zaobserwoanej wartoœci statystyki testowej 
## - Statystyka - funkcja na zaobserowanej próbie - liczymy wartoœæ 
## dystrybuanty w tym punkcie, dystrybuanty bo statystyki testowe zbiegaj¹ do 
## jakiœ rozk³adów, w teœcie Fishera do Chi-kwadrat. Dystrybuanta to prawdopodobienstwo
## Tak uzyskane prawdopodobieñœtwo to p-wartoœæ
## Jeœli jest ma³a, mniejsza od 0.05 to ODRZUCAMY HIPOTEZE ZEROW¥


## Wniosek 
## Odrzucamy hipotezê zerow¹, zmienna default jest zale¿na od -- tylko dla zmiennych nominalnych

## Æwiczenie napisaæ funkcjê która tworzy tablicê kontyngencji 
## dla zadanych kolumn oraz dodaje kolumnê frakcji sukcesów (Y=1) do 
## sumy liczby po¿yczek 


## Rozwi¹zanie
cont.table <- function(data,var1,var2){
  cont <- with(data,table(var1,var2))
  frakcja <- cont[,2]/(cont[,1]+cont[,2])
                       frakcja <- as.numeric(format(frakcja, digits = 4 ))
                       cont <- cbind(cont,frakcja)
                       return(cont)
                       
}

## Zwi¹zek pomiêdzy zmiennymi nominalnymi a statusem po¿yczki 
## NONE I OTHER mo¿na wy³¹czyæ z analizy bo ma³o obserwacji
## Nie mo¿na usun¹æ odrazu  wszystkich na. bo za ma³y zbiór dim(na.omit(loanbook_sub))
## Tablica kontyngencji dla zmiennej objaœnianej i zmiennej home_ownership

 
loanbook_sub <- loanbook_sub %>% filter(home_ownership %in% c("MORTGAGE","RENT","OWN")) 
with(loanbook_sub, table(home_ownership, default))
                                           
# tk_home <- with(loanbook_sub, table(home_ownership, default,exclude=c("OTHER","NONE","")))
## Test Fishera niezale¿noœci 
fisher.test(tk_home, simulate = T)
## Odrzucamy H0 o niezale¿noœci, default zale¿y od posiadania domu 

## Zmienna dti 
## A ratio calculated using the borrower’s total monthly 
## debt payments on the total debt obligations, excluding 
## mortgage and the requested LC loan, divided by the
## borrower’s self-reported monthly income.


## Debt to Income Ratio (break into factors at 5% levels)
## Ta zienna powinna byæ traktowana jako zmienna ciagla, czy spodziewamy sie ekstremum
## lokalnego? 

loanbook_dti <- na.omit(loanbook_sub[,c('dti', 'default')])
## Tablica kontyngencji dla zmiennej objaœnianej i zmiennej dti
tk_dti <- with(loanbook_dti , table(loanbook_dti$dti, loanbook_dti$default))

# Revolving Utilization 
# Revolving utilization percent is the portion of a borrower’s revolving 
# credit limit 
# Borrowers with high utilization rates are more likely to have high fixed credit 
# card payments which might affect their ability to repay their loans. A
# lso, a high utilization rate often reflects a lack of other financing options,
# (i.e. credit card limit) that they actually are using at any given point. 
# For example, if a borrower’s total credit limit is $15,000 and their outstanding
# balance is $1,500 their utilization rate would be 10%.


## Tablica kontyngencji dla zmiennej objaœnianej i zmiennej revol_util
with(loanbook_sub, table(revol_util, default)) 
tk_rev <- with(loanbook_sub, table(revol_util, default))

# Loan Purpose (exclude renewable energy because so few data points)
## Tablica kontyngencji dla zmiennej objaœnianej i zmiennej purpose
# small_bussines ma duza frakcje

loanbook_sub$purpose %>% levels
## Podzielimy na 4 kategorie ? consumer; small-bussines, educational medical 
loanbook_sub <- loanbook_sub %>% mutate(purpose = ifelse(purpose %in% c('home_improvement','house',
                                                              'moving'),'home', 
                                        ifelse(purpose %in% c('wedding','vacation',
                                                              'car','credit_card','other', 'major_purchase', "renewable_energy",""), 'consumer', 'medBUSedu')))
tk_p <- with(loanbook_sub, table(purpose, default)) 
# frakcja <- tk_p[,2]/(tk_p[,2] + tk_p[,1])
# cbind(tk_p,frakcja)  
fisher.test(tk_p, simulate = T)
## Odrzucamy H0 o niezale¿noœci, default zale¿y od purpose


## Æwiczenie
# total_acc liczba kont, podziel na faktory na zasadzie kwantyli
# Stwórz tablice kontyngencji
# PrzeprowadŸ test Fishera o niezale¿noœci 

## --------- Tutaj jest miejsce na Twoje roziw¹zanie 












total_acc <- na.omit(loanbook[,'total_acc'])
podzial_acc <- c(0, quantile(total_acc, 0.25, na.omit=T),
               quantile(total_acc, 0.5, na.omit=T),
               quantile(total_acc, 0.75, na.omit=T), Inf)

loanbook_sub <- loanbook_sub %>% mutate(total_acc = cut(total_acc, breaks = podzial_acc))

## Tablica kontyngencji dla zmiennej objaœnianej i zmiennej total_acc
tk_acc <- with(loanbook_sub, table(total_acc, default))
tk_acc 
## Test Fishera niezale¿noœci 
fisher.test(tk_acc, simulate = T)
## Odrzucamy H0 o niezale¿noœci, default zale¿y od total_acc



# Delinquencies in the past 2 Years (combine factors levels for any > 3) 

# CIAGLA
# Modyfikacja, zale¿y na uchwyceniu istotnej ró¿nicy miêdzy 0 a 1 lub 2 
# nie mo¿emy modelowaæ róznicy z tak¹ sam¹ wag¹ pomiêdzy 2 a 3, jak 0 a 1 
# policzymy kwantyl 0.9, tzn 90 % obserwacji ma wartoœæ poni¿ej 
delinq_2yrs <- na.omit(loanbook_sub[,'delinq_2yrs'])
quantile(delinq_2yrs, 0.90)

# The number of delinquencies in the past 2 years 
# indicates the number of times a borrower has been behind on payments.
loanbook_sub <- loanbook_sub %>% mutate(delinq_2yrs = ifelse(delinq_2yrs >1,1,0))

## Tablica kontyngencji dla zmiennej objaœnianej i zmiennej total_acc
tk_del <- with(loanbook_sub, table(delinq_2yrs, default))


# 'annual_inc' odrzucamy, zmienna dti zawiera informacje o dochodzi z uwzglêdnieniem dodatkowych 
# 'loan_amnt' odrzucamy, zmienna purpose mo¿e wiêcej powiedzieæ 


# Wybrane zmienne do modelu ze zmienna objasniana
data_set <- c('default','delinq_2yrs','total_acc', 'purpose',
              'revol_util', 'dti', 'home_ownership')
final <- loanbook_sub[,data_set ] 
PrimaryData <- na.omit(final) # duzo brakujacych

# PrimaryData o jedna zmienna wiecej  
# Dopasowanie modelu pelnego

glm_p<-glm(default ~ delinq_2yrs + total_acc + purpose + revol_util + dti +
          home_ownership, family=binomial(link=logit), data = final)

summary(glm_p)
# AIC: 41277

# home_ownership nie istotny, nie roznicy miedzy grupami
glm1<-glm(default ~ delinq_2yrs + total_acc + purpose + revol_util + dti,
          family=binomial(link=logit), data = final)
summary(glm1)
# AIC: 41273

# data = PrimaryData[sample(nrow(PrimaryData),100000)]
data = PrimaryData
FullGLM <- glm(PrimaryData, family = "binomial") # logistyczna jest domyslna

FullGLM_Probit <- glm(PrimaryData, family = "binomial"(link="probit"))
# AIC 41 271

TestData = as.data.frame(cbind(FullGLM$fitted.values, FullGLM$y))
## Score ckredytowy nie zbankrutowa³/ zbankrutowal P(Y=0) = 1 - P(Y=1) / P(Y=1)
#TestData = as.data.frame(cbind(log((1-FullGLM$fitted.values)/FullGLM$fitted.values), FullGLM$y))

colnames(TestData) = c("Score","Defaults")


# interest wypadkowa z innych czynnikow cel, wielkosc kwoyn
# plots cumulative accuracy profile

CAP <- function(var = "Score", data = TestData, defaultIndicator = "Defaults") {
  data[,defaultIndicator] <- as.numeric(data[,defaultIndicator]) #juz bylo numeric
  data[,var] <- as.numeric(data[,var]) #juz bylo numeric
  
  n <- nrow(data)
  n_d <- sum(data[,defaultIndicator])
  
  data <- data[order(data[,var]), c(var,defaultIndicator)]
  if (mean(cumsum(data[,defaultIndicator])/n_d)<0.5) 
  { data[,"CAP"] <- cumsum(data[rev(rownames(data)),defaultIndicator])/n_d 
  # wez kolumne default indicator, rev zamienia kolejnoœæ wierszy 
  } else {data[,"CAP"] <- cumsum(data[,defaultIndicator])/n_d}
  
  data[,"FRACTION_CTP"] <- 1:n/n # podzialka osi, to samo co radnom model
  data[,"RANDOM_MODEL"] <- data[,"FRACTION_CTP"]
  data[,"PERFECT_MODEL"] <- c(1:n_d , rep(n_d, n-n_d))/n_d
  # pole jako srednia - calka 
  AR <- round((mean(data[,"CAP"])-mean(data[,"RANDOM_MODEL"]))/(mean(data[,"PERFECT_MODEL"])-mean(data[,"RANDOM_MODEL"])),3)
  dane <- melt(data = data[,c(-1,-2)], id.vars = "FRACTION_CTP")
  
  wykres <- ggplot(dane, aes(x = FRACTION_CTP, y = value, col = variable )) + geom_line() +
  xlab("Fraction of counterparties") + ylab("Fraction of defaulted counterparties") +
    scale_colour_discrete(name  = paste("Model AR =",AR),
                          labels = c("actual model", "random model","perfect model")) +
  ggtitle("Cumulative accuracy profile") + theme_bw() 
  plot(wykres)
  return(AR)
}

# plots receiver operating characteristics curve
ROC <- function(var = "Score", data=TestData, defaultIndicator = "Defaults") {
  n <- nrow(data)
  n_d <- sum(data[,defaultIndicator])
  data[,defaultIndicator] <- as.numeric(data[,defaultIndicator])
  data <- data[order(data[,var]),]
  if (mean(cumsum(data[,defaultIndicator])/n_d)>0.5) {
    data <- data[order(data[,var], decreasing=TRUE),] # zmien porzadek , jesli score nie rosnacy
    } 
  ROC <- data.frame(TPR = cumsum(data[,defaultIndicator])/sum(data[,defaultIndicator]),
                    FPR = cumsum(!data[,defaultIndicator])/sum(!data[,defaultIndicator]),
                    Random = 1:n/n)
  
  AUC <- sum(diff(ROC[,"TPR"])*rollmean(ROC[,"FPR"],2))
  
  wykres <- ggplot(ROC) + geom_line(aes(x = TPR, y = FPR, col = 'red')) +
    geom_line(aes(x =Random, y = Random, col = 'blue')) + 
    xlab("False positive rate") + ylab("True positive rate") +
    scale_colour_discrete(name  = paste("Model AUC =",round(AUC,3)),
                          labels = c( "random model", "actual model")) +
    ggtitle("Receiver operating characteristic curve") + theme_bw() 
  plot(wykres)
  
  return(AUC)
  
}

###Testowanie CAP i ROC functions 

CAP(var = "Score", data=TestData, defaultIndicator = "Defaults")

## Wykres rozkladu score , wedlug default i non-default
## The construction of a ROC curve is illustrated in Figure below which shows possible 
## distributions of rating scores for defaulting and non-defaulting debtors.
## For a perfect rating model the left
## distribution and the right distribution in Figure  would be separate. 
## For real rating systems,perfect discrimination in general is not possible. 
## Both distributions will overlap as illustrated 

# Assume someone has to find out from the rating scores which debtors will survive during the
# next period and which debtors will default. One possibility for the decision-maker would be to
# introduce a cut-off value C as in Figure , and to classify each debtor with a rating score
#lower than C as a potential defaulter and each debtor with a rating score higher than C as a
# non-defaulter. Then four decision results would be possible. If the rating score is below the
# cut-off value C and the debtor defaults subsequently, the decision was correct. Otherwise the
# decision-maker wrongly classified a non-defaulter as a defaulter. If the rating score is above
# the cut-off value and the debtor does not default, the classification was correct. Otherwise a
# defaulter was incorrectly assigned to the non-defaulters’ group.




View(TestData)
ggplot(TestData, aes(Score, group= Defaults, col = Defaults)) + geom_density() + theme_bw()+
ggtitle("Distribution of rating scores for defaulting and non-defaulting debtors.")
 
## Wzory 
# hit rate (true positive rate)  HR(C) = H(C)/ND 
# where H(C) is the number of defaulters predicted correctly (with the cut-off value C, )
# and ND is the total number of defaulters in the sample.

# The false alarm rate FAR(C) is defined as 
# FAR(C) = F(C)/NND
# where F(C) is the number of false alarms, i.e. the number of non-defaulters that were
# classified incorrectly as defaulters by using the cut-off value C. 
# The total number of nondefaulters in the sample is denoted byNND


# HR(C) is the area to the left of the
# cut-off value C under the score distribution of the defaulters (coloured plus hatched area),
# while FAR(C) is the area to the left of C under the score distribution of the non-defaulters
# (chequered plus hatched area).

ROC(var = "Score", data=TestData, defaultIndicator = "Defaults")


AccuracyRatio = CAP(var = "Score", data=TestData, defaultIndicator = "Defaults")
AUC_t = ROC(var = "Score", data=TestData, defaultIndicator = "Defaults")


# wlasciwy petla(loop) dla wszystkich kombinacji
# count for progress baru



T <- dim(na.omit(data))[1]
nv.max <- min(T-2, length(colnames(data)[-1]),4) # liczba zmiennych do modelu


lst1<-list()
AIC <- vector()
AUC <-vector()
AccuracyRatios <-vector()



n.max <- 0
progress.bar <-0
for (i in 1:nv.max) {
  for (s in 1:dim(combn(length(colnames(data)[-1]),i))[2]){
    n.max <- n.max + 1
  }}

pb <- txtProgressBar(min = 0, max = n.max , style = 3)
for (i in 1:nv.max) {
  for (s in 1:dim(combn(length(colnames(data)[-1]),i))[2]){
    
    tryCatch({ # pomija b³êdy
      progress.bar <- progress.bar + 1
      Sys.sleep(0.1)
      # update progress bar
      setTxtProgressBar(pb, progress.bar)  
      
      
      
      columns<-combn(length(colnames(data)[-1]),i)[,s]
      d <-glm(data[,c(1,columns+1)], family = "binomial")
      
      
      TestedData = as.data.frame(cbind(d$fitted.values, d$y))
      colnames(TestedData) = c("Score","Defaults")
      rr<- ROC(var = "Score", data=TestedData, defaultIndicator = "Defaults")
      acr <- CAP(var = "Score", data=TestedData, defaultIndicator = "Defaults")
      
      lst1 <- c(lst1,list(d))
      AIC<- c(AIC,d$aic)
      AUC <- c(AUC, rr)
      AccuracyRatios <- c(AccuracyRatios, acr)
      
      
    }, error=function(e){cat("\n","ERROR :",conditionMessage(e), "\n")}) #!!!! pomija b³êdy
  }
}
close(pb)
#if(length(AIC)>0) {
#names(AIC)<- 1:length(AIC)

#}

if(length(AUC)>0) {
  names(AUC)<- 1:length(AUC)
  names(AIC)<-1:length(AIC)
  names(AccuracyRatios)<-1:length(AccuracyRatios)
}


#Liczba wybieranych modeli:   
l = 4

sink("TheBest_models.txt")
## Najpierw selection
print("Najlepsze modele wg AUC")

for(i in 1:4){
  print(i);
  bestAUC = lst1[[as.numeric(names(sort(AUC, decreasing=TRUE)))[i]]]
  AucBest = AUC[[as.numeric(names(sort(AUC, decreasing=TRUE)))[i]]]
  print(summary(bestAUC))
  print(paste("AUC =",AucBest))
  
}

print("Najlepsze modele wg accuracy ratio")

for(i in 1:4){
  print(i);
  bestAR = lst1[[as.numeric(names(sort(AccuracyRatios, decreasing=TRUE)))[i]]]
  AucBestAR = AUC[[as.numeric(names(sort(AccuracyRatios, decreasing=TRUE)))[i]]]
  ARbest = AccuracyRatios[[as.numeric(names(sort(AccuracyRatios, decreasing=TRUE)))[i]]]
  print(summary(bestAR))
  print(paste("Accuracy Ratio =", ARbest))
  print(paste("AUC =",AucBestAR))
}


print("Najlepsze modele wg kryterium Akaike ratio")


for(i in 1:4){
  print(i);
  bestAIC = lst1[[as.numeric(names(sort(AIC, decreasing=FALSE)))[i]]]
  AucBestAIC = AUC[[as.numeric(names(sort(AIC, decreasing=FALSE)))[i]]]
  print(summary(bestAIC))
  print(paste("AUC =",AucBestAIC))
}
readline("Proszê nacisn¹æ dowolny klawisz, aby kontynuowaæ.")
sink()
 

