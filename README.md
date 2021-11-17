
---
title: "Untitled"
author: "Tolga Kurt"
date: "16 10 2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Bu veriseti 26.06.2021 tarihine kadar ülkemizde yapılan aşılar hakkında bilgi vermektedir.Ek olarak harita çizimi için 
https://gadm.org/download_country_v3.html adresine göz atıp uygun veriyi indirebilirsiniz.

This dataset provides information about vaccines made in Turkey until 26.06.2021. In addition, for map drawing
You can browse https://gadm.org/download_country_v3.html and download the appropriate data.

İşimize yarayacak bazı kütüphaneleri yüklemeyle işe başlayalım.

```{r,warning = FALSE,message=FALSE}
library(dplyr)
library(tidyverse)
library(ggplot2)
library(magick)
library(ggimage)
library(gtsummary)
library(kableExtra)
library(maps)
library(mapdata)
library(ggimage)
library(paletteer)
library(stringr)
library(gridExtra)
```

Şimdi aşılanma verisini içe aktaralım.

```{r,warning = FALSE,message = FALSE}

covidvactr = readxl::read_excel("C:/Users/tolga/Desktop/covid19Vaccination.xlsx")
attach(covidvactr)
covidvactr%>%head()
```

Verideki değişkenlere kısa bir göz atıyoruz.
```{r}
str(covidvactr)
```

Verimizde bu analiz için işimize yaramayacak değişkenler var, bunları çıkararak devam edebiliriz.
```{r}
covidvactr = covidvactr%>%select(-ID,-SEQID,-DATE_,-CITY,-PREVID)
covidvactr%>%head()
```

Şimdi de veriye her il için aşılanma yüzdeliklerini ekleyelim ve analizin devamında kolaylık olması için değişken isimlerini değiştirelim.
```{r}
covidvactr = covidvactr%>%mutate(percentage_total = `_TOTAL`/POPULATION,percentage_dose1 = `_1DOSE`/POPULATION,percentage_dose2 = `_2DOSE`/POPULATION)
colnames(covidvactr) =c("city","dose1","dose2","total","population","diff_dose1","diff_dose2","diff_total","percentage_total","percentage_dose1","percentage_dose2") 

```

Analizin devamında bölgeleri de kullanabilmek için şehirlerin bulunduğu bölgeleri de verimize yeni bir değişken olarak ekleyebiliriz.
```{r}
marmara = c("Istanbul", "Balikesir", "Bursa", "Tekirdag", "Canakkale",
           "Yalova", "Kocaeli", "Kirklareli", "Edirne", "Bilecik", "Sakarya")

ege = c("Izmir", "Manisa", "Aydin", "Denizli", "Usak", "Afyon", "Kutahya", "Mugla")

ic_anadolu = c("Ankara", "Konya", "Kayseri", "Eskisehir", "Sivas", "Kirikkale", "Aksaray",
              "Karaman", "Kirsehir", "Nigde", "Nevsehir", "Yozgat", "Cankiri")

karadeniz = c("Amasya", "Artvin", "Bartin", "Bayburt", "Bolu", "Corum", "Gumushane", "Giresun",
             "Karabuk", "Kastamonu", "Ordu", "Rize", "Samsun", "Sinop", "Tokat", "Trabzon", "Zonguldak","Duzce")

dogu_anadolu = c("Agri", "Ardahan", "Bitlis", "Bingol", "Elazig", "Erzincan", "Erzurum",
                "Hakkari", "Igdir", "Kars", "Malatya", "Mus","Tunceli", "Van")

guneydogu_anadolu = c("Gaziantep", "Diyarbakir", "Sanliurfa", "Batman", "Adiyaman",
                 "Siirt", "Mardin", "Kilis", "Sirnak")

akdeniz = c("Antalya", "Adana", "Mersin", "Hatay", "Burdur", "Osmaniye","Kahramanmaras", "Isparta")

covidvactr = covidvactr%>%mutate(regions = case_when((city%in%marmara)~"Marmara",(city%in%ege)~"Ege",(city%in%karadeniz)~"Karadeniz",
                                       (city%in%dogu_anadolu)~"D.Anadolu",(city%in%guneydogu_anadolu)~"GuneydoguAnadolu",(city%in%akdeniz)~"Akdeniz",
                                       (city%in%ic_anadolu)~"Ic Anadolu"))
covidvactr[,c("city","regions")]%>%head(10)
```

Bölgeleri de eşleştirdikten sonra bazı görselleştirme işlemlerine geçebiliriz.
```{r}
image = "https://d2v9ipibika81v.cloudfront.net/uploads/sites/247/COVAX.jpg"
p<-ggplot(covidvactr[order(-covidvactr$total),][1:10,],aes(x = reorder(city,total),y = total))+geom_bar(stat = "identity",fill = "tomato3")+scale_y_continuous(labels = scales::comma)+xlab("Sehirler")+ylab("")+ggtitle("Covid-19 Vaccination")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "white"),text = element_text(colour = "white"),axis.text.x = element_text(angle = 45, hjust = 1, colour = "white"),axis.text.y = element_text(angle = 45, hjust = 1, colour = "white"))+labs(subtitle = "Date : 26.06.2021")
ggbackground(p,image)

```


Görüldüğü ve beklenildiği üzere ülkemizde en çok aşılanma en kalabalık ilk 3 şehrimizde bulunmaktadır.Şimdi de aşılanma oranına göre en yüksek şehirlerimize bakalım.


```{r}

image = "https://d2v9ipibika81v.cloudfront.net/uploads/sites/247/COVAX.jpg"
p<-ggplot(covidvactr[order(-covidvactr$percentage_total),][1:10,],aes(x = reorder(city,percentage_total),y = percentage_total))+geom_bar(stat = "identity",fill = "tomato3")+scale_y_continuous(labels = scales::comma)+xlab("Sehirler")+ylab("")+ggtitle("Covid-19 Vaccination Percentage")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "white"),text = element_text(colour = "white"),axis.text.x = element_text(angle = 45, hjust = 1, colour = "white"),axis.text.y = element_text(angle = 45, hjust = 1, colour = "white"))+labs(subtitle = "Date : 26.06.2021")
ggbackground(p,image)

```

Aşılanma oranlara bakıldığında ise nüfusu görece az olan şehirlerimizde aşılanma oranları oldukça yüksek.
Bölgelerin en çok aşılanma oranları bulunan şehirlerine bakalım.
```{r}
akd = covidvactr[which(covidvactr$regions=="Akdeniz"),]
kdz = covidvactr[which(covidvactr$regions=="Karadeniz"),]
mar = covidvactr[which(covidvactr$regions=="Marmara"),]
ege= covidvactr[which(covidvactr$regions=="Ege"),]
ıc_anadolu = covidvactr[which(covidvactr$regions=="Ic Anadolu"),]
dogu = covidvactr[which(covidvactr$regions=="D.Anadolu"),]
gdogu = covidvactr[which(covidvactr$regions=="GuneydoguAnadolu"),]

p1<-ggplotGrob(ggplot(akd[order(-akd$percentage_total),][1:5,],aes(x = reorder(city,percentage_total*100),y = percentage_total*100))+geom_bar(stat = "identity",fill = "tomato3")+scale_y_continuous(labels = scales::comma)+xlab("Şehirler")+ylab("Aşılanma Oranı (%)")+ggtitle("Akdeniz")+theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")))
p2<-ggplotGrob(ggplot(kdz[order(-kdz$percentage_total),][1:5,],aes(x = reorder(city,percentage_total*100),y = percentage_total*100))+geom_bar(stat = "identity",fill = "tomato3")+scale_y_continuous(labels = scales::comma)+xlab("Şehirler")+ylab("Aşılanma Oranı (%)")+ggtitle("Karadeniz")+theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")))
p3<-ggplotGrob(ggplot(mar[order(-mar$percentage_total),][1:5,],aes(x = reorder(city,percentage_total*100),y = percentage_total*100))+geom_bar(stat = "identity",fill = "tomato3")+scale_y_continuous(labels = scales::comma)+xlab("Şehirler")+ylab("Aşılanma Oranı (%)")+ggtitle("Marmara")+theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")))
p4<-ggplotGrob(ggplot(ege[order(-ege$percentage_total),][1:5,],aes(x = reorder(city,percentage_total*100),y = percentage_total*100))+geom_bar(stat = "identity",fill = "tomato3")+scale_y_continuous(labels = scales::comma)+xlab("Şehirler")+ylab("Aşılanma Oranı (%)")+ggtitle("İç Anadolu")+theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")))
p5<-ggplotGrob(ggplot(ıc_anadolu[order(-ıc_anadolu$percentage_total),][1:5,],aes(x = reorder(city,percentage_total*100),y = percentage_total*100))+geom_bar(stat = "identity",fill = "tomato3")+scale_y_continuous(labels = scales::comma)+xlab("Şehirler")+ylab("Aşılanma Oranı (%)")+ggtitle("Ege")+theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")))
p6<-ggplotGrob(ggplot(dogu[order(-dogu$percentage_total),][1:5,],aes(x = reorder(city,percentage_total*100),y = percentage_total*100))+geom_bar(stat = "identity",fill = "tomato3")+scale_y_continuous(labels = scales::comma)+xlab("Şehirler")+ylab("Aşılanma Oranı (%)")+ggtitle("Güneydoğu Anadolu")+theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")))
p7<-ggplotGrob(ggplot(gdogu[order(-gdogu$percentage_total),][1:5,],aes(x = reorder(city,percentage_total*100),y = percentage_total*100))+geom_bar(stat = "identity",fill = "tomato3")+scale_y_continuous(labels = scales::comma)+xlab("Şehirler")+ylab("Aşılanma Oranı (%)")+ggtitle("Doğu Anadolu")+theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = "black")))
fig <- function(width, heigth){
     options(repr.plot.width = width, repr.plot.height = heigth)
}
fig(width = 10,heigth = 10)
grid.arrange(p1,p2,p3,p4,p5,p6,p7, nrow = 2 )

```



Bölgelerin en yüksek oranlı şehirlerini tablolaştıralım.

```{r,warning=FALSE,message=FALSE}
regi<-rbind(akd[which.max(akd$percentage_total),],
      kdz[which.max(kdz$percentage_total),],
      mar[which.max(mar$percentage_total),],
      dogu[which.max(dogu$percentage_total),],
      ege[which.max(ege$percentage_total),],
      gdogu[which.max(gdogu$percentage_total),],
      ıc_anadolu[which.max(ıc_anadolu$percentage_total),])
regi = regi%>%mutate(per_total=paste0("%",round(regi$percentage_total*100,2)))
regi%>%
  select(regions,city,per_total)%>%kbl()%>%kable_material(position = "left")%>%
  column_spec(3,color = "white",background =spec_color(as.numeric(str_remove(regi$per_total,"%")),end = .9))
```



Şimdi ise sırasıyla 1.doz,2.doz ve toplam aşılanma oranlarının bölgelere göre özet tablolarını oluşturualım.

## {.tabset .tubset-fade .tabset-pills}

### 1.Doz
```{r,warning=FALSE,message=FALSE}
regions_dose1 = covidvactr%>%dplyr::group_by(regions)%>%summarise(SehirSayısı =n(),ToplamAsilanma = sum(dose1) ,AsilanmaOranı = paste0("%",round((sum(dose1)/sum(population))*100,2)),Ortalama = mean(dose1),StandartSapma = round(sd(dose1)),Medyan = median(dose1),CeyreklerAralığı = IQR(dose1),GeometrikOrtalama = exp(mean(log(dose1))))
kbl(regions_dose1[order(-as.numeric(str_remove(regions_dose1$AsilanmaOranı,"%"))),])%>%kable_material(position = "left")%>%column_spec(3,color = "white",background =spec_color(regions_dose1$ToplamAsilanma[1:7],end = .9))%>%column_spec(4,color = "white",background =spec_color(as.numeric(str_remove(regions_dose1$AsilanmaOranı,"%"))[1:7],end = .9))
```
### 2.Doz
```{r,warning=FALSE,message=FALSE}
regions_dose2 = covidvactr%>%dplyr::group_by(regions)%>%summarise(SehirSayısı =n(),ToplamAsilanma = sum(dose2) ,AsilanmaOranı = paste0("%",round((sum(dose2)/sum(population))*100,2)),Ortalama = mean(dose2),StandartSapma = round(sd(dose2)),Medyan = median(dose2),CeyreklerAralığı = IQR(dose2),GeometrikOrtalama = exp(mean(log(dose2))))
kbl(regions_dose2[order(-as.numeric(str_remove(regions_dose2$AsilanmaOranı,"%"))),])%>%kable_material(position = "left")%>%column_spec(3,color = "white",background =spec_color(regions_dose2$ToplamAsilanma[1:7],end = .9))%>%column_spec(4,color = "white",background =spec_color(as.numeric(str_remove(regions_dose2$AsilanmaOranı,"%"))[1:7],end = .9))


```
### Toplam Aşılanma
```{r}
regions_total = covidvactr%>%dplyr::group_by(regions)%>%summarise(SehirSayısı =n(),ToplamAsilanma = sum(total) ,AsilanmaOranı = paste0("%",round((sum(total)/sum(population))*100,2)),Ortalama = mean(total),StandartSapma = round(sd(total)),Medyan = median(total),CeyreklerAralığı = IQR(total),GeometrikOrtalama = exp(mean(log(total))))
kbl(regions_total[order(-as.numeric(str_remove(regions_total$AsilanmaOranı,"%"))),])%>%kable_material(position = "left")%>%column_spec(3,color = "white",background =spec_color(regions_total$ToplamAsilanma[1:7],end = .9))%>%column_spec(4,color = "white",background =spec_color(as.numeric(str_remove(regions_total$AsilanmaOranı,"%"))[1:7],end = .9))

```
## {-}


Bu tablolar sonucunda ne yazık ki Güney Doğu Anadolu bölgemizde aşılama oranı çok düşük olduğunu farkedebiliriz.Bunun hastane yetersizliği, aşıya karşı önyargı gibi bir çok nedeni olabilir.Ortalamadan sapma en fazla Marmara Bölgesinde var.Bunun nedeni tabiki İstanbul'un fazla nüfusunun getirdiği yüksek aşılanma sayısıdır.


Şimdi ise ölgelerin toplam aşılanmalarına bakalım.
```{r,warning=FALSE,message=FALSE}
attach(regions_dose1)
attach(regions_dose2)
attach(regions_total)
d1<-ggplot(regions_dose1[order(-ToplamAsilanma),],aes(x = reorder(regions,ToplamAsilanma),y = ToplamAsilanma))+geom_bar(stat = "identity",fill = "tomato3")+scale_y_continuous(labels = scales::comma)+xlab("Bölgeler")+ylab("")+ggtitle("Bölgelere Göre 1.Doz Aşılanma Sayıları")+theme_minimal()
d2<-ggplot(regions_dose2[order(-ToplamAsilanma),],aes(x = reorder(regions,ToplamAsilanma),y = ToplamAsilanma))+geom_bar(stat = "identity",fill = "tomato3")+scale_y_continuous(labels = scales::comma)+xlab("Bölgeler")+ylab("")+ggtitle("Bölgelere Göre 2.Doz Aşılanma Sayıları")+theme_minimal()
d3<-ggplot(regions_total[order(-ToplamAsilanma),],aes(x = reorder(regions,ToplamAsilanma),y = ToplamAsilanma))+geom_bar(stat = "identity",fill = "tomato3")+scale_y_continuous(labels = scales::comma)+xlab("Bölgeler")+ylab("")+ggtitle("Bölgelere Göre Toplam Aşılanma Sayıları")+theme_minimal()
grid.arrange(d1,d2,d3)
```



Şimdi ise harita üzerinde bazı görselleştirme çalışmaları yapalım.
Haritayı illere bölerek kullanmak için başlangıçta paylaştığım linkteki veriyi içeri aktaralım ve bir göz atalım.



İndirdiğimiz harita verisinde bazı şehir isimlerinde Türkçe karakter hatası var veya yanlış yazılmış,işe bunları düzeltmekle başlayabiliriz.
```{r}
library(sp)
tr = readRDS("C:/Users/tolga/Downloads/tr.rds")
plot(tr)

```



İndirdiğimiz harita verisinde bazı şehir isimlerinde Türkçe karakter hatası var veya yanlış yazılmış,işe bunları düzeltmekle başlayabiliriz.
```{r,warning=FALSE,message=FALSE}
tr@data$NAME_1[49] = "Kirikkale"
tr@data$NAME_1[13] = "Bartin"
tr@data$NAME_1[17] = "Bingol"
tr@data$NAME_1[22:24] = c("Canakkale","Cankiri","Corum")
tr@data$NAME_1[27] = "Duzce"
tr@data$NAME_1[29] = "Elazig"
tr@data$NAME_1[35] = "Gumushane"
tr@data$NAME_1[38] = "Igdir"
tr@data$NAME_1[42:43] = c("Kahramanmaras","Karabuk")
tr@data$NAME_1[54] = "Kutahya"
tr@data$NAME_1[81]  = "Zonguldak"
tur_p = fortify(tr)

```
Ana veriseti ve ek verisetini şehir değişkenleriortak değişkenler olduğu için birleştirebiliriz.Bu birleştirme işleminden sonra fortify ettiğimiz datayla da ilk birleştirdiğimiz datayı ID değişkenine göre birleştirelim.

```{r}
left_data = data.frame(id = row.names(tr@data),city = tr@data$NAME_1)%>%left_join(covidvactr,by = "city")
final_map = left_join(tur_p,left_data,by = "id")
```
Ve görselleştirme zamanı...

İlk olarak 1.doz aşılanmanın nüfusa oranına bakalım.


```{r,warning=FALSE,message=FALSE}
ggplot(final_map) +geom_polygon(aes(x = long, y = lat, group = group, fill = percentage_dose1*100), color = "grey") +
  coord_map() +theme_void() + labs(title = "1.Doz Aşılanma Oranı",subtitle = paste0("Tarih : 26.06.2021",'\n',"Ülke Genelinde Aşılanma Oranı : ","%",round((sum(covidvactr$dose1)/sum(covidvactr$population))*100,2)),caption = "Kaynak: Kaggle/OmerColakoglu") +
  scale_fill_distiller(palette = "RdPu", limits = c(0,100), na.value = "white") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))+scale_fill_paletteer_c("viridis::plasma",
                                                                                                                 limits = c(0,100), na.value = "white",name = "")

```
 2.Doz
```{r,warning=FALSE,message=FALSE}
ggplot(final_map) +geom_polygon(aes(x = long, y = lat, group = group, fill = percentage_dose2*100), color = "grey") +
  coord_map() +theme_void() + labs(title = "2.Doz Aşılanma Oranı",subtitle = paste0("Tarih : 26.06.2021",'\n',"Ülke Genelinde Aşılanma :","%",round((sum(covidvactr$dose2)/sum(covidvactr$population))*100,2)),caption = "Kaynak: Kaggle/OmerColakoglu") +
  scale_fill_distiller(palette = "RdPu", limits = c(0,100), na.value = "white") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))+scale_fill_paletteer_c("viridis::plasma",
                                                                                                                 limits = c(0,100), na.value = "white",name = "")


```

 Toplam Aşılanma
```{r,warning=FALSE,message=FALSE}
ggplot(final_map) +geom_polygon(aes(x = long, y = lat, group = group, fill = percentage_total*100), color = "grey") +
  coord_map() +theme_void() + labs(title = "Aşılanma Oranı",subtitle = paste0("Tarih : 26.06.2021",'\n',"Ülke Genelinde Aşılanma Oranı : ","%",round((sum(covidvactr$total)/sum(covidvactr$population))*100,2)),caption = "Kaynak: Kaggle/OmerColakoglu") +
  scale_fill_distiller(palette = "RdPu", limits = c(0,100), na.value = "white") +
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))+scale_fill_paletteer_c("viridis::plasma",
                                                                                                                 limits = c(0,100), na.value = "white",name = "")
ggsave("toplam1.jpg")
```

Maalesef ki harita görselinde doğu kesimlerde aşılanma oranının çok düşük olduğu görülüyor. Farkındalık oluşması adına daha çok çalışma yürütülmesi gerekebilir.
