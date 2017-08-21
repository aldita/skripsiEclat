Halaman help ini membantu anda untuk menggunakan Fast statistical software untuk melakukan beberapa pengujian hipotesis Nonparametrik

### Cara penggunaan 

1. Pilih variabel pada kotak "Select one variabel"
	Apabila variabel tidak dikotomi, aplikasi akan menampilkan kotak isian inputan data
	
2. Pilih jenis hipotesis pada "Hipotesis type"

3. Pilih besaran alpha pada "Alpha"

4. Apabila ingin melakukan unduhan hasil analisis, silahkan pilih jenis output pada bagian "Generate summary", lalu tekan "Download"


### Fungsi dan Teori

Uji ini digunakan ketika populasi hanya terdiri dari dua kelas, contohnya : laki – laki dan perempuan, iya dan tidak, sekolah dan tidak sekolah, setuju dan tidak setuju. Peluang terpilihnya kelas pertama adalah  p . Sedangkan peluang terpilihnya kelas kedua adalah   q = 1 – p .  Populasi yang demikian disebut populasi binomial, binari, atau dikotomi.

Siegel dan Castellan (1988), menyatakan bahwa binomial distribution digunakan untuk menghitung peluang mendapatkan suatu hasil dari pengambilan sampel pada populasi binomial. Dengan melakukan uji hipotesis awal H0 : p = p0 , kita dapat melihat apakah wajar untuk percaya proporsi kedua kategori dari sampel yang ditarik sama dengan nilai hipotesis p0 atau 1 - p0.  

Hipotesis awal yang digunakan pada aplikasi ini adalah H0 : p = q = 0.5 . Jika keputusannya gagal menolak H0, maka tidak ada perbedaan peluang atau frekuensi terjadinya kelas pertama dengan kelas kedua. Hipotesis alternatif yang disediakan pada aplikasi ini ada dua, yaitu :


1.	H1 : p > q , keputusan menerima H1 bermakna  peluang  atau frekuensi terjadinya kelas pertama lebih besar dari kelas kedua. <br/>
2.  H1 : p != q , keputusan menerima H1 bermakna peluang / frekuensi terjadinya kelas pertama tidak sama dengan kelas kedua. Statistik uji yang digunakan adalah  dua kali statistik uji pada H1 : p  > q . <br/>

Untuk jumlah sampel  ≤  35, penghitungan nilai kritisnya menggunakan tabel D pada buku “Nonparametric Statistics for Behavioral Science” karangan Sidney Siegel dan N. John Castellan Jr tahun 1988. Statistik uji pada uji ini adalah α yang ditentukan sendiri oleh peneliti. Tolak H0 saat nilai kritis lebih kecil dari statistik uji.

![](figures/nonpar/binomial.png)<br/> 