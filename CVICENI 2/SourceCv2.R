source("SourceCv2.R")

## CV1 ///////////////////////////////////////////////////////
#Variace s opakovanim a bez opakovani
#V(n,k)
variace <- function(n,k){
  return(factorial(n)/factorial(n-k))
}
#V*(n,k)
variace_opak <- function(n,k){
  return(n^k)
}

#permutace s opakovanim a bez opakovani
#P(n)=V(n,n)
permutace <- function(n){
  return(variace(n,n))
}
#P*(n1,n2,n3,....,nk)
permutace_opak <- function(vec_n){
  n=sum(vec_n);
  res_temp=factorial(n);
  for (i in 1:length(vec_n)){
    res_temp=res_temp/factorial(vec_n[i]);
  }
  return(res_temp)
}

#kombinace s opakovanim a bez opakovani
#C(n,k)
kombinace <- function(n,k){
  return(choose(n,k))
}
#C*(n,k)
kombinace_opak <- function(n,k){
  return(choose(n+k-1,k))
}


## CV2 ///////////////////////////////////////////////////////
#uplna pravdepodobnost
uplna_pravdepodobnost <- function(P_B,P_AB){
  P_A=0;
  for (i in 1:length(P_B)){
    P_A=P_A+P_AB[i]*P_B[i];
  }
  return(P_A)
} 


#Bayes
#P(B_k|A)=(P(A|B_k)*P(B_k))/P(A);  kde P(A)=SUMA(P(A|B_i)*P(B_i))
bayes <- function(P_B,P_AB,k){
  P_A=uplna_pravdepodobnost(P_B,P_AB);
  P_BkA=P_AB[k]*P_B[k]/P_A;
  return(P_BkA)
} 


## Reseni cv
# ukol1

omega=1:20
A=c(15,16,17,18,19,20)
#pravdepodobnost je 
length(A)/length(omega)

#ukol 2
p_liche=1/(20+10)
p_sude=2*p_liche
pravdepodobnosti=c(p_liche,p_sude,p_liche,p_sude,p_liche,p_sude,p_liche,p_sude,p_liche,p_sude,p_liche,p_sude,p_liche,p_sude,p_liche,p_sude,p_liche,p_sude,p_liche,p_sude)
#pravdepodobnost je
sum(pravdepodobnosti[15:20])

#ukol 3
#musim mit 4 cisla z 6 a 2 cisla z 43 oproti celkovemu poctu 6 cisel ze 49
(kombinace(6,4)*kombinace(43,2))/kombinace(49,6)

#ukol 4
#kazdy student musi mit sve znameni (je jich 12 a znameni taky 12) takze pripustne moznosti jsou permutace, naopak vsechny moznosti  jsou variace s opakovanim
permutace(12)/variace_opak(12,12)

#ukol 5
#rozdielime na bloky I=(A,B) a II=(C,D,E)
PI=(1-0.1)*(1-0.3)
nonPII=(0.2)*(0.3)*(0.2)
PII=1-nonPII
# vysledek
PI*PII

#ukol 6
#geometricka pravdepodobnosti
ohrada=40*100
#blize k jihu oproti severu je polovina 20*100 z tĂ©to poloviny jsou jeĹˇtÄ› dva boÄŤnĂ� trojuhelnĂ�ky blĂ�Ĺľe k vĂ˝chodu/zĂˇpadu
blize_J=20*100-20*20
#pravdepodobnosti
blize_J/ohrada

#ukol 7
#vÄ›ta o ĂşplnĂ© pravdÄ›podobnosti P(B_k)=(0.1,0.2,0.4,0.3) P(A|B_k)=(0.5,0.75,0.15,0.2)
uplna_pravdepodobnost(c(0.1,0.2,0.4,0.3),c(0.5,0.75,0.15,0.2))

#ukol 8
#bayes P(B_k)=(0.6,0.4) P(A|B_k)=(0.75,0.25)
bayes(c(0.6,0.4),c(0.75,0.25),1)

#ukol 9
# a) opÄ›t bayes  P(B_k)=(0.15,0.85) P(A|B_k)=(0.80,0.20)
bayes(c(0.15,0.85),c(0.8,0.20),1)
#b) prvni moĹľnost, poÄŤĂ�tĂˇme s upravenou pravdÄ›podobnosti
p_z=bayes(c(0.15,0.85),c(0.8,0.20),1)
bayes(c(p_z,1-p_z),c(0.8,0.20),1)
# nebo odpovÄ›ÄŹ najedou ale nezĂˇvislĂˇ
bayes(c(0.15,0.85),c(0.8^2,0.20^2),1)

#ukol 10
# vÄ›ta o ĂşplnĂ© pravdÄ›podobnosti P(ANO)=P(koruna_lic)*P(ANO|koruna_lic)+P(koruna_rub)*P(dvoukoruna_lic|koruna_rub)
# rovnice 120/320=0.5*x+0.5*0.5
(120/320-0.5^2)/0.5
