# Importing PDF Files in varaibles:

install.packages("pdftools")
library(pdftools)

mov1=file.path("band_baaja_baraat.pdf")
txt1=pdf_text(mov1)
mov2=file.path("barfi.pdf")
txt2=pdf_text(mov2)
mov3=file.path("Dev_D.pdf")
txt3=pdf_text(mov3)
mov4=file.path("Devdas.pdf")
txt4=pdf_text(mov4)
mov5=file.path("Dum_Laga_Ke_Haisa.pdf")
txt5=pdf_text(mov5)
mov6=file.path("Highway.pdf")
txt6=pdf_text(mov6)
mov7=file.path("Honeymoon_travels.pdf")
txt7=pdf_text(mov7)
mov8=file.path("Jaane_Tu_Ya_Jaane_Na.pdf")
txt8=pdf_text(mov8)
mov9=file.path("Jab_We_Met.pdf")
txt9=pdf_text(mov9)
mov10=file.path("Jodhaa_Akbar.pdf")
txt10=pdf_text(mov10)
mov11=file.path("Arjun_Reddy.pdf")
txt11=pdf_text(mov11)
mov12=file.path("Kal_Ho_Na_Hoo.pdf")
txt12=pdf_text(mov12)
mov13=file.path("Masaan.pdf")
txt13=pdf_text(mov13)
mov14=file.path("Raanjhanaa.pdf")
txt14=pdf_text(mov14)
mov15=file.path("Rockstar.pdf")
txt15=pdf_text(mov15)
mov16=file.path("Ek_Tha_Tiger.pdf")
txt16=pdf_text(mov16)
mov17=file.path("Tamasha.pdf")
txt17=pdf_text(mov17)
mov18=file.path("The_Lunchbox.pdf")
txt18=pdf_text(mov18)
mov19=file.path("Aashiqui2.pdf")
txt19=pdf_text(mov19)
mov20=file.path("Yeh_Jawaani_Hai_Deewani.pdf")
txt20=pdf_text(mov20)

#Checking Head, Structure and Summary of files:
head(txt1,10)
structure(txt1)
summary(txt1)
# repeat same for all 20 variables


#Importing Libraries Needed for our analysis and visualization:
install.packages("tidyverse")
install.packages("tidytext")
install.packages("dplyr")
install.packages("stringr")
install.packages("tm")
library(tidyverse)
library(tidytext)
library(dplyr)
library(stringr)
library(tm)

# Creating Corpuses for each variable:
corpus1=iconv(txt1)
corpus1=Corpus(VectorSource(corpus1))

corpus2=iconv(txt2)
corpus2=Corpus(VectorSource(corpus2))

corpus3=iconv(txt3)
corpus3=Corpus(VectorSource(corpus3))

corpus4=iconv(txt4)
corpus4=Corpus(VectorSource(corpus4))

corpus5=iconv(txt5)
corpus5=Corpus(VectorSource(corpus5))

corpus6=iconv(txt6)
corpus6=Corpus(VectorSource(corpus6))

corpus7=iconv(txt7)
corpus7=Corpus(VectorSource(corpus7))

corpus8=iconv(txt8)
corpus8=Corpus(VectorSource(corpus8))

corpus9=iconv(txt9)
corpus9=Corpus(VectorSource(corpus9))

corpus10=iconv(txt10)
corpus10=Corpus(VectorSource(corpus10))

corpus11=iconv(txt11)
corpus11=Corpus(VectorSource(corpus11))

corpus12=iconv(txt12)
corpus12=Corpus(VectorSource(corpus12))

corpus13=iconv(txt13)
corpus13=Corpus(VectorSource(corpus13))

corpus14=iconv(txt14)
corpus14=Corpus(VectorSource(corpus14))

corpus15=iconv(txt15)
corpus15=Corpus(VectorSource(corpus15))

corpus16=iconv(txt16)
corpus16=Corpus(VectorSource(corpus16))

corpus17=iconv(txt17)
corpus17=Corpus(VectorSource(corpus17))

corpus18=iconv(txt18)
corpus18=Corpus(VectorSource(corpus18))

corpus19=iconv(txt19)
corpus19=Corpus(VectorSource(corpus19))

corpus20=iconv(txt20)
corpus20=Corpus(VectorSource(corpus20))

# Insptecting each created Corpuses:
inspect(corpus2[1:2])
# Repeat the same with all 20 created corpuses

# Cleaning the text:

line1=tibble(line=1:length(txt1),text=txt1)
line1
token1=line1 %>% unnest_tokens(word,text)
token1 %>% count(word,sort=TRUE)
newtoken1=token1 %>% anti_join(stop_words)
newtoken1 = newtoken1 %>%mutate_all(~str_remove_all(., "\\b(?:\\d+|page)\\b"))
count1= newtoken1 %>% count(word,sort=TRUE) %>% filter(n>30)%>%filter(n<200)
#count1


line2=tibble(line=1:length(txt2),text=txt2)
line2
token2=line2 %>% unnest_tokens(word,text)
token2 %>% count(word,sort=TRUE)
newtoken2=token2 %>% anti_join(stop_words)
newtoken2 <- newtoken2 %>%mutate_all(~str_remove_all(.,"\\b(?:\\d+|page)\\b"),"")
count2=newtoken2 %>% count(word,sort=TRUE)%>% filter(n>10)%>%filter(n<60)
#count2


line3=tibble(line=1:length(txt3),text=txt3)
line3
token3=line3 %>% unnest_tokens(word,text)
token3 %>% count(word,sort=TRUE)
newtoken3=token3 %>% anti_join(stop_words)
newtoken3 <- newtoken3 %>%mutate_all(~str_remove_all(.,"\\b(?:\\d+|page)\\b"),"")
count3=newtoken3 %>% count(word,sort=TRUE)%>% filter(n>19)%>%filter(n<60)
#count3

line4=tibble(line=1:length(txt4),text=txt4)
line4
token4=line4 %>% unnest_tokens(word,text)
token4 %>% count(word,sort=TRUE)
newtoken4=token4 %>% anti_join(stop_words)
newtoken4 <- newtoken4 %>%mutate_all(~str_remove_all(.,"\\b(?:\\d+|page)\\b"),"")
count4=newtoken4 %>% count(word,sort=TRUE)%>% filter(n>19)%>%filter(n<200)
#count4


line5=tibble(line=1:length(txt5),text=txt5)
line5
token5=line5 %>% unnest_tokens(word,text)
token5 %>% count(word,sort=TRUE)
newtoken5=token5 %>% anti_join(stop_words)
newtoken5 <- newtoken5 %>%mutate_all(~str_remove_all(.,"\\b(?:\\d+|page)\\b"),"")
count5=newtoken5 %>% count(word,sort=TRUE)%>% filter(n>15) %>%filter(n<100)
#count5

line6=tibble(line=1:length(txt6),text=txt6)
line6
token6=line6 %>% unnest_tokens(word,text)
token6 %>% count(word,sort=TRUE)
newtoken6=token6 %>% anti_join(stop_words)
newtoken6 <- newtoken6 %>%mutate_all(~str_remove_all(.,"\\b(?:\\d+|page)\\b"),"")
count6=newtoken6 %>% count(word,sort=TRUE)%>% filter(n<100)%>%filter(n>10)
#count6


line7=tibble(line=1:length(txt7),text=txt7)
line7
token7=line7 %>% unnest_tokens(word,text)
token7 %>% count(word,sort=TRUE)
newtoken7=token7 %>% anti_join(stop_words)
newtoken7 <- newtoken7 %>%mutate_all(~str_remove_all(.,"\\b(?:\\d+|page)\\b"),"")
count7=newtoken7 %>% count(word,sort=TRUE)%>% filter(n>19)%>% filter(n<100)
#count7



line8=tibble(line=1:length(txt8),text=txt8)
line8
token8=line8 %>% unnest_tokens(word,text)
token8 %>% count(word,sort=TRUE)
newtoken8=token8 %>% anti_join(stop_words)
newtoken8 <- newtoken8 %>%mutate_all(~str_remove_all(.,"\\b(?:\\d+|page)\\b"),"")
count8=newtoken8 %>% count(word,sort=TRUE)%>% filter(n>15)%>% filter(n<100)
#count8


line9=tibble(line=1:length(txt9),text=txt9)
line9
token9=line9 %>% unnest_tokens(word,text)
token9 %>% count(word,sort=TRUE)
newtoken9=token9 %>% anti_join(stop_words)
newtoken9 <- newtoken9 %>%mutate_all(~str_remove_all(.,"\\b(?:\\d+|page)\\b"),"")
count9=newtoken9 %>% count(word,sort=TRUE)%>% filter(n>19)%>% filter(n<100)
#count9




line10=tibble(line=1:length(txt10),text=txt10)
line10
token10=line10 %>% unnest_tokens(word,text)
token10 %>% count(word,sort=TRUE)
newtoken10=token10 %>% anti_join(stop_words)
newtoken10 <- newtoken10 %>%mutate_all(~str_remove_all(.,"\\b(?:\\d+|page)\\b"),"")
count10=newtoken10 %>% count(word,sort=TRUE)%>% filter(n>19)%>% filter(n<100)
#count10


line11=tibble(line=1:length(txt11),text=txt11)
line11
token11=line11 %>% unnest_tokens(word,text)
token11 %>% count(word,sort=TRUE)
newtoken11=token11 %>% anti_join(stop_words)
newtoken11 <- newtoken11 %>%mutate_all(~str_remove_all(.,"\\b(?:\\d+|page)\\b"),"")
count11=newtoken11 %>% count(word,sort=TRUE)%>% filter(n>30)%>% filter(n<100)
#count11

line12=tibble(line=1:length(txt12),text=txt12)
line12
token12=line12 %>% unnest_tokens(word,text)
token12 %>% count(word,sort=TRUE)
newtoken12=token12 %>% anti_join(stop_words)
newtoken12 <- newtoken12 %>%mutate_all(~str_remove_all(.,"\\b(?:\\d+|page)\\b"),"")
count12=newtoken12 %>% count(word,sort=TRUE)%>% filter(n>30)%>% filter(n<200)
#count12

line13=tibble(line=1:length(txt13),text=txt13)
line13
token13=line13 %>% unnest_tokens(word,text)
token13 %>% count(word,sort=TRUE)
newtoken13=token13 %>% anti_join(stop_words)
newtoken13 <- newtoken13 %>%mutate_all(~str_remove_all(.,"\\b(?:\\d+|page)\\b"),"")
count13=newtoken13 %>% count(word,sort=TRUE)%>% filter(n>10)%>% filter(n<100)
#count13

line14=tibble(line=1:length(txt14),text=txt14)
line14
token14=line14 %>% unnest_tokens(word,text)
token14 %>% count(word,sort=TRUE)
newtoken14=token14 %>% anti_join(stop_words)
newtoken14 <- newtoken14 %>%mutate_all(~str_remove_all(.,"\\b(?:\\d+|page)\\b"),"")
count14=newtoken14 %>% count(word,sort=TRUE)%>% filter(n>19)%>% filter(n<100)
#count14

line15=tibble(line=1:length(txt15),text=txt15)
line15
token15=line15 %>% unnest_tokens(word,text)
token15 %>% count(word,sort=TRUE)
newtoken15=token15 %>% anti_join(stop_words)
newtoken15 <- newtoken15 %>%mutate_all(~str_remove_all(.,"\\b(?:\\d+|page)\\b"),"")
count15=newtoken15 %>% count(word,sort=TRUE)%>% filter(n>15)%>% filter(n<100)
#count15

line16=tibble(line=1:length(txt16),text=txt16)
line16
token16=line16 %>% unnest_tokens(word,text)
token16 %>% count(word,sort=TRUE)
newtoken16=token16 %>% anti_join(stop_words)
newtoken16 <- newtoken16 %>%mutate_all(~str_remove_all(.,"\\b(?:\\d+|page)\\b"),"")
count16=newtoken16 %>% count(word,sort=TRUE)%>% filter(n>5)%>% filter(n<60)
#count16

line17=tibble(line=1:length(txt17),text=txt17)
line17
token17=line17 %>% unnest_tokens(word,text)
token17 %>% count(word,sort=TRUE)
newtoken17=token17 %>% anti_join(stop_words)
newtoken17 <- newtoken17 %>%mutate_all(~str_remove_all(.,"\\b(?:\\d+|page)\\b"),"")
count17=newtoken17 %>% count(word,sort=TRUE)%>% filter(n>10)%>% filter(n<60)
#count17


line18=tibble(line=1:length(txt18),text=txt18)
line18
token18=line18 %>% unnest_tokens(word,text)
token18 %>% count(word,sort=TRUE)
newtoken18=token18 %>% anti_join(stop_words)
newtoken18 <- newtoken18 %>%mutate_all(~str_remove_all(.,"\\b(?:\\d+|page)\\b"),"")
count18=newtoken18 %>% count(word,sort=TRUE)%>%filter(n<80)%>%filter(n>9)
#count18

line19=tibble(line=1:length(txt19),text=txt19)
line19
token19=line19 %>% unnest_tokens(word,text)
token19 %>% count(word,sort=TRUE)
newtoken19=token19 %>% anti_join(stop_words)
newtoken19 <- newtoken19 %>%mutate_all(~str_remove_all(.,"\\b(?:\\d+|page)\\b"),"")
count19=newtoken19 %>% count(word,sort=TRUE)%>% filter(n>19)%>%filter(n<100)
#count19

line20=tibble(line=1:length(txt20),text=txt20)
line20
token20=line20 %>% unnest_tokens(word,text)
token20 %>% count(word,sort=TRUE)
newtoken20=token20 %>% anti_join(stop_words)
newtoken20 <- newtoken20 %>%mutate_all(~str_remove_all(.,"\\b(?:\\d+|page)\\b"),"")
count20=newtoken20 %>% count(word,sort=TRUE)%>% filter(n>15)%>%filter(n<150)
#count20

#Displaying most frequently used word for each script:
plot1=count1 %>% ggplot(aes(n,word))+geom_col()+labs(title="Band Bajaa Barat",y=NULL)
plot1

plot2=count2 %>% ggplot(aes(n,word))+geom_col()+labs(title="Barfi",y=NULL)
plot2

plot3=count3 %>% ggplot(aes(n,word))+geom_col()+labs(title="Dev D",y=NULL)
plot3

plot4=count4 %>% ggplot(aes(n,word))+geom_col()+labs(title="Devdas",y=NULL)
plot4

plot5=count5 %>% ggplot(aes(n,word))+geom_col()+labs(title="Dum Laga Ke Haisha",y=NULL)
plot5

plot6=count6 %>% ggplot(aes(n,word))+geom_col()+labs(title="Highway",y=NULL)
plot6

plot7=count7 %>% ggplot(aes(n,word))+geom_col()+labs(title="Honeymoon Travels",y=NULL)
plot7

plot8=count8 %>% ggplot(aes(n,word))+geom_col()+labs(title="Jaane Tu Ya Jaane Na",y=NULL)
plot8

plot9=count9 %>% ggplot(aes(n,word))+geom_col()+labs(title="Jab We Met",y=NULL)
plot9

plot10=count10 %>% ggplot(aes(n,word))+geom_col()+labs(title="Jodhaa Akbar",y=NULL)
plot10

plot11=count11 %>% ggplot(aes(n,word))+geom_col()+labs(title="Arjun Reddy",y=NULL)
plot11

plot12=count12 %>% ggplot(aes(n,word))+geom_col()+labs(title="Kal Ho Na Hoo",y=NULL)
plot12

plot13=count13 %>% ggplot(aes(n,word))+geom_col()+labs(title="Masaan",y=NULL)
plot13

plot14=count14 %>% ggplot(aes(n,word))+geom_col()+labs(title="Raanjhanaa",y=NULL)
plot14

plot15=count15 %>% ggplot(aes(n,word))+geom_col()+labs(title="Rockstar",y=NULL)
plot15

plot16=count16 %>% ggplot(aes(n,word))+geom_col()+labs(title="Ek Tha Tiger",y=NULL)
plot16

plot17=count17 %>% ggplot(aes(n,word))+geom_col()+labs(title="Tamasha",y=NULL)
plot17

plot18=count18 %>% ggplot(aes(n,word))+geom_col()+labs(title="The Lunchbox",y=NULL)
plot18

plot19=count19 %>% ggplot(aes(n,word))+geom_col()+labs(title="Aashiqui 2",y=NULL)
plot19

plot20=count20 %>% ggplot(aes(n,word))+geom_col()+labs(title="Yeh Jawaani Hai Deewani",y=NULL)
plot20



