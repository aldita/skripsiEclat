
Halaman help ini membantu anda untuk menggunakan Fast statistical software untuk melakukan analisis cluster menggunakan Fuzzy Geographically Weighted Clustering.


### Step 1: Set Cluster Properties

Terdapat pilihan 'Use shapefiles' yang berfungsi untuk menghasilkan file jarak antar wilayah dan menambah peta pada tampilan hasil analisis. 

* <b>Pilih variabel dengan cara memblok variabel yang ingin digunakan</b>
<br>
* <b>Pilih metode yang akan digunakan</b><br>
* <b>Tentukan jumlah kelas</b><br>

### Step 2 : Set Parameter
Terdapat dua pilihan, <b>'Default'</b> dan <b>'Custom'</b>. Pilihan default akan memberikan setting parameter sebagai berikut : 
```
fuzzifier : 1.5
alfa : 0.5
beta : 0.5
a : 1
b : 1
Treshold / error : 0.0001
Maximum iteration (MaxCycle) : 100
Limit : 5

```
Pilihan <b>'Custom'</b> dapat dipilih apabila ingin mengedit parameter sesuai yang diinginkan.

### Step 3 : View Option
Terdapat pilihan tentang tabel yang ditampilkan dan pengaturan plot.

### Step 4 : Hasil Analisis
![FGWC](figures/hasilfgwc.png)
Gambar 1. Hasil output dengan shapefiles
<br>
<br>
![FGWC-ABC](figures/fgwcnoshp.png)
Gambar 1. Hasil output tanpa shapefiles

Output hasil yang akan ditampilkan adalah matrix keanggotaan <b>'Membership matrix'</b> yang didalamnya terdapat hasil klaster. Selain itu juga akan ditampilkan rata-rata nilai dari masing-masing variabel pada tiap kelas pada <b>'Cluster center'</b>. 

Hasil evaluasi dari klaster yang dihasilkan terdapat pada <b>'Index Evaluation'</b>

* IFV dan PC menunjukkan hasil yang lebih baik pada nilai yang lebih besar.
* CE, PI dan XB menunjukkan hasil yang lebih baik pada nilai yang lebih kecil.

<br/>&copy; Saad Tazkiahtu Dienulloh (2015) 