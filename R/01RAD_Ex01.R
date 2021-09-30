################################## 
##### 01RAD Exercise 01 ##########
#################################
#
# Poznamka na uvod: Text je psan bez hacku a cerek kvuli ruznym kodovanim
#                   Doporucuji veskery kod ukladat v UTF-8, 
#                   nastavite v tool -> global options -> code -> saving
#
# Jak nastavit Rstudio, k cemu editor slouzi atd ukazi online behem cviceni.
#
# Spustte program RStudio.
# Je to editor pro praci s R a veskere potrebne veci jsou tam jiz nastaveny.
# V jednom z oken je spustena konzole, kam muzete primo psat prikazy, ktere ma R vykonat.
# Konzoli poznate nejen podle jmena okna (Console), ale i podle vyvolavaciho symbolu >
#
# Pokud vami zadany prikaz nedokoncite a predcasne spustite pomoci ENTER, program zahlasi +
# a ceka na dokonceni prikazu.
# Jiz provedene prikazy lze zpetne vyvolat postupnym stisknutim kurzorove sipky nahoru.
#
# Jak ziskavat kod na cviceni:
# Git: clone https://gitlab.fjfi.cvut.cz/francji1/01rad_2020.git
# MS-teams: files -> exercises -> 01rad_2020  (je o kopie toho co je na gitu)
# 
# Spuste R projekt ze stahnute slozky: 01rad_2020.Rproj
# potom mate relativni cestu nastavenu a otevrete si v adresari /R prislusne cviceni

# Pokud chcete nastavit cestu absolutne, tak zalozte si prisusny adresar,
# zkopirujte do nej prislusny kod (adresar z MS teams) a 
# nastavte ho jako pracovni pomoci prikazu:
# setwd("D:/Vyuka/01RAD/")  
#(upravte prikaz podle sve cesty)  


#	zkontrolujte nastaveni pracovni slozky zkontrolujete pomoci prikazu 
   getwd()


# Zadejte nekolik prikazu s ruzymi matematickymi operacemi primo do konzole
#
 40+2
 50-8
 21*2
#   
# Mnohem praktictejsi je ale psat prikazy do souboru scriptu, 
# ktery je mozno ulozit, opakovane spustit, psat k nemu komenty atd.
# Naprikad tento dokument nesouci nazev REAN2017_Ex01.R
   
# Pozn. znak # uvozuje komentar 
   
# Ze souboru odesleme oznacenou cast kodu do scriptu pomoci prikazu
#   Ctrl+R 
# pokud neni oznacen zadny text odesle se radek, kde je kurzor.
   
#Default package
(getOption("defaultPackages"))  

# informace o spustene session vcetne base balicku   
sessionInfo()
   
# vycet vsech base a recommended balicku
subset(as.data.frame(installed.packages()), Priority %in% c("base","recommended"), select=c(Package, Priority))
   
# vycet vsech nainstalovanych a dostupnych knihoven v pocitaci   
library()


# Jak stahnout a nainstalovat knihovnu - balicek (package)
# Priklad balicku "car" - Companion to Applied Regression
?install.packages
install.packages("car")
library(car)
  
# V R-studiu lze naklikat Packages -> install -> (zadat jmeno) a potvrdit Install 

# napoveda ke knihovne
library(help = "datasets")   # priority: base
library(help = "car")       # priority: recommended

# Jednotlive balicky maji napovedu, pdf se seznamem funkci a medajlonky
vignette(all = T)  # prehled vsech medailonku u vsech instalovanych balicku
vignette("embedding", package = "car")

# Nektere balicky obsahuji krom funkci i data
#Priklady dat z balicku datasets
Titanic
WWWusage
# prohlizeni dat v editoru
View(Titanic)

# R rozlisuje velikost pismen, tj
titanic = 42
Titanic == titanic
class(titanic)
class(Titanic)
is.table(Titanic)
is.data.frame(Titanic)
Titanic_df = as.data.frame(Titanic)
Titanic_df
# datum a data.frame se budeme venovat pozdeji 

### Ukol: instalujte balicek MASS, nacteteho a projdete si k nemu help, 
#         jsou v nem obsazena i data popisující cenu nemovitostí v okolí Bostnu?



######################################
## Aritmetika - R jako kalkulacka
40 + 2              # scitani
44 - 2              # odcitani
6  * 7              # nasobeni
294 / 7             # deleni
42^7                # mocnina
sqrt(1764)          # druha odmocnina
230539333248^(1/7)  # (od)mocnina, zavorky nutne

#Konstanty a funkce 
exp(1)             # eulerovo cislo
exp(42)            # exponenciela
pi                 # konstanta pi
sin(pi/2)          # sinus 
log(exp(1))        # přirozený logaritmus (pozor! jinde se casto značí ln)
ln(exp(1))         # funkci ln R nezna !
factorial(42)      # 42! 
choose(5, 2)       # 5 nad 2 = kombinacni cislo
10/3
options(digits = 15) # chcili zobrazit vice cislic
10/3
options(digits = 7)  # zpet na 7


######################################
### Prace s promennymi

x <- 42    # ulozeni hodnoty do 
x	       # vytisteni hodnoty v promenne ulozene
x = 42     # jiny zpusob, ale  nelze pouzit v kombinaci s jinym prikazem
y <- 3    # uložení do jiné proměnné
x + y    
z <- x + y

print(z <- x + y)
print(z = x + y)	 

### Vymazani promennych
### --------------
ls()          # vypis pouzivanych a definovanych objektu
rm(list=ls()) # vymaze vsechn objekty 
ls() 

WWWusage2 = WWWusage
##### Typy objektů
#  integer, double: real numbers (rational and irrational)
#  character, logical: includes TRUE, FALSE,
#  NA stands for “not available”, i.e., a missing value.
#  NULL znamena prazdnou promennou

A <- NULL  
B <- NA
is.na(A)   # nelze zjistit, A je prazdna
is.na(B)

sqrt(-1) # isn't defined
sqrt(-1+0i) # is defined
sqrt(as.complex(-1)) # same thing
(0 + 1i)^2 # should be -1
typeof((0 + 1i)^2)
class((0 + 1i)^2)

 
x <- (0 + 1i)^2
x
y <- as.numeric(x)
y
class(y)
class(x)
y == x

x = -1
is.complex(x)
x = as.complex(x)
sqrt(x)
x = as.integer(x)
sqrt(x)


### --- Vectors ---
a <- c(1,2,5.3,6,-2,4)                   # numeric vector
b <- c("one","two","three")              # character vector
c <- c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE)  # logical vector
c
is.logical(c)
cc = as.numeric(c)
cc

#dalsi vektory:
seq(from = 1, to = 5)
1:5
seq(from = 2, by = -0.1, length.out = 4)
seq(from = 2, to = 10, by = 2)
seq(from = as.Date("2019-09-24"), to = as.Date("2019-09-28"),by =1)
rep(5,times = 6)

x <- c(74, 31, 95, 61, 76, 34, 23, 54, 96)
x
x[2:4]
x[c(1, 3, 4, 8)]
x[-c(1, 3, 4, 8)]
LETTERS[1:5]
letters[-(6:23)]


### --- data.frame ---
# data.frame je seznam vektorů stejné délky, každý sloupec má svůj datový typ.
trees
stromy = trees

head(trees)       # prvnich par radku
summary(trees)    # prehled promennych
table(trees[,"Girth"])
str(trees)
View(trees)

Girth           # nezna
trees$Girth	    # takto nahlidneme na prvni promenou
G = trees$Girth # uz zname G
G
attach(trees)   # pouzivat jen pokud cely projekt budeme pouzivat jeden dataset
                # casto vede k chybam, pouzivejte jen tam kde si jste jisti
Girth
detach(trees)

### --- tibbles ---
# https://r4ds.had.co.nz/tibbles.html
# Modern format of data.frame from the tidyverse 
# Krom rychlejsiho na
# Lisi se napriklad v tom, ze nemeni typ promennych, 
#  nekonvertuje string do factoru, nemeni jmena promennych, a mnoho dalsiho 
# Interaguje s vetsinou funkci stejne jako data.frame


### --- matrices ---
# matrix má dvě dimenze a celá matice musí mít jeden datový typ.
# Je užitečná pro algebraické operace a může zrychlit numerické operace.
M <- matrix(c(3:14), nrow = 4, byrow = TRUE)
print(M)


##	Lze pracovat jen s nekterými sloupci, radky databaze

trees[,1]		      #	jen 1. radek
trees[-1,]		      #	bez 1. radku
trees[c(2,3),]	      #	jen konkretni radky
trees[,c("Girth","Volume")]
trees[,-trees$Volume]	#	vykricnik  znamena negaci
trees$novapromenna = "vyska" #c(1:length(trees$Volume))
trees$novapromenna[trees$Height<70] = "maly strom"

#	jen radky splnujici podminku 
# podobne pro sloupce


### Ukol zadan zvlast v Assignemts:







