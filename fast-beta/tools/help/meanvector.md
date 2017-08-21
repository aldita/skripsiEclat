Halaman help ini membantu anda untuk menggunakan Fast statistical software untuk melakukan beberapa analisis MANOVA

File yang akan diolah dapat di load melalui tab Data. 
File contoh dapat diload dengan klik radio buttton 'examples' dan tekan 'Load examples'.
Jika ingin menggunakan file dari direktori anda, pilih radio buttton untuk menentukan ekstensi file yang diinginkan. Kemudian klik tombol choose file untuk memilih file mana yang akan diupload.

### Contoh: Uji perbandingan dua vektor rata-rata dengan data Iris
Data iris merupakan sebuah data yang diperkenalkan oleh Sir Ronald Fisher pada tahun 1936 sebagai contoh dari analisis diskriminan. Data ini dikumpulkan oleh Edgar Anderson untuk mengukur variasi morfologi dari tiga spesies bunga iris.
Variabel: 
- Panjang kelopak (Sepal.Length)
- Lebar kelopak (Sepal.Width)
- Panjang mahkota (Petal.Length) 
- Lebar mahkota (Petal.Width)

### Step 1
Membagi data menjadi 2 populasi dengan memberi ceklis pada pilihan split data into two groups
![Mean - Data](figures/data_meanvector.png)<br/>
Gambar 1. Data uji perbandingan dua vektor rata-rata

### Step 2
Menguji kenormalan data.
Pertama pilih metode uji normal, kemudian lihat hasilnya di tab Normality Test.
![Mean - Normality](figures/figures/mvn2.png.png)<br/>
Gambar 2. Hasil uji normal

### Step 3
Pengujian Manova
Lihat hasil uji di tab Summary.
![Mean - Summary](figures/summary_meanvector.png)<br/>
Gambar 3. Hasil uji perbandingan dua vektor rata-rata<br/>
Tambahan: anda dapat mengubah nilai apha dengan mengganti nilai alpha