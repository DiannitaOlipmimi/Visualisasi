## VISUALISASI DATA KUANTITATIF DAN KUALITATIF ##
mencari perbedaan visualisasi data kuantitatif dan kualitatif serta statistik deskriptif

## DEFINISI ##
---
data kuantitatif adalah data yang berwujud angka atau bilangan. Data
kuantitatif biasanya dijadikan sebagai bahan dasar bagi setiap permasalahan yang
bersifat statistik. sedangkan data kualitatif adalah data yang berbentuk selain angka. Data kualitatif
dapat dikumpulkan dengan cara wawancara, analisis dokumen, observasi,
pemotretan gambar atau perekaman video.

Statistik deskriptif adalah peringaksan data.
Peringakasan data ini dapat dilakukan dengan 2 ukuran yaitu, ukuran pemusatan
dan penyebaran. Ukuran pemusatan antara lain, median, modus, kuartil, rata-rata,
dan lain lain. Sedangkan ukuran penyebaran adalah range, ragam.

---
## PERSOALAN DAN DATA ##
---
Data yang digunakan adalah data Prestige dan data kanker sel darah
dari data tersebut digunakan visualisasi untuk menggambarkan data kualitatif dan kuantitatif didalamnya

---


#install.packages("carData")
library(carData)

View(Prestige) #melihat data
?Prestige #keterangan data

#tipe dan statistik deskriptif
str(Prestige)
summary(Prestige)

#visualisasi data kualitatif dataset kanker sel darah
data=read.csv(file.choose(), header = TRUE)

jenis_sel = data$JenisSel
jenis_sel

freq.jenis_sel = table(jenis_sel)
freq.jenis_sel
cbind(freq.jenis_sel)

pie(freq.jenis_sel, labels = c("Myeloblast, 65%", "Myelocyte, 17%", 
                               "Promyelocyte, 18%"), 
    main = "Diagram Lingkatan Jenis Sel Kanker")

#visualisasi durasi letusan pada dataset faithfull (min 5)
#install.packages("mixAK")
library(mixAK)
data("Faithful")
View(Faithful)
?Faithful

durasi_erupsi=Faithful$eruptions
waktu=faithful$waiting

hist(durasi_erupsi, main = "histogram durasi erupsi")
boxplot(durasi_erupsi, main = "boxplot durasi erupsi")
summary(durasi_erupsi)
plot(durasi_erupsi, type = "l", main = "line chart durasi erupsi")
plot(durasi_erupsi, main = "diagram pencar durasi erupsi")

durasi_erupsi.sort=sort(durasi_erupsi, decreasing=TRUE)
#install.packages("tigerstats")
library(tigerstats)
barchartGC(durasi_erupsi.sort, main = "bar chart durasi erupsi")
barchartGC(durasi_erupsi.sort, horizontal = TRUE, main = "column chart")

#melakukan visualisasi korelasi
cor.test(durasi_erupsi, waktu)

plot(durasi_erupsi, waktu)
lm.faithful=lm(waktu~durasi_erupsi, data = Faithful)
lines(Faithful$eruptions, fitted(lm.faithful), col="red")

