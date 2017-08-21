Halaman "Help" ini dapat digunakan untuk menjelaskan prosedur analisis regresi dengan menggunakan Seemingly Unrelated Regression (SUR)

### Seemingly Unrelated Regression (SUR)
Sebagai contoh awal, anda dapat menggunakan data "GrunfeldGreene". Data ini dapat anda load melalui menu Data, langkahnya sebagai berikut:<br/>
1. Pilih menu "Data".<br/>
2. Pilih "Example" pada menu "Load data" yang ada di sidebar(sisi kiri) halaman.<br/>
3. Tekan tombol "Load Example".<br/>
4. Pilih data "GrunfeldGreen" pada menu "Datasets".<br/>

### Contoh: Analisis SUR dengan data GrunfeldGreen
Data GrunfeldGreen digunakan oleh Zellner (1962) dalam melakukan analisis Seemingly Unrelated Regression (SUR). Data ini merupakan data nilai investasi dari 5 perusahaan pada tahun 1935-1954.<br/>
Variabel-variabel pada data Grunfeld Greene:<br/>
1. Invest<br/>
2. value<br/>
3. Capital<br/>
4. Firm<br/>
5. Year<br/>
Untuk melihat struktur data dilihat pada menu "View" yang tersedia pada halaman menu "Data".<br/>

### Step 1
Mengatur parameter (pada sidebar halaman) yang akan digunakan untuk proses analisis.<br/>
1. Menentukan "Variable Grouping". (Variabel yang akan digunakan sebagai dasar pembagian data)<br/>
2. Menentukan "Dependent Variable".<br/>
3. Menentukan "Independent Variables". (Minimal 2)<br/>
4. Menentukan "Method". (Metode analisis)<br/>
![SUR - Param](figures/sur_param.png)<br/>
Gambar 1. Pengaturan parameter<br/>

### Step 2
Identifikasi data untuk melihat struktur data dan adanya korelasi kesebayangan pada data. Langkah-langkah:<br/>
1. Pilih tab menu "Data Identification".<br/>
2. Pilih sub tab menu "Identification".<br/>
![Manova - Identify](figures/sur_identify.png)<br/>
Gambar 2. Hasil Identifikasi Struktur Data<br/>
3. Pilih sub tab menu "Comtemporaneous Correlation Test".<br/>
![Manova - Comtemporaneous Correlation](figures/sur_CCT.png)<br/>
Gambar 3. Hasil Uji Korelasi Kesebayangan<br/>

### Step 3
Estimasi model persamaan. Langkah-langkah:<br/>
1. Pilih tab menu "SUmmary".<br/>
2. Pilih sub tab menu "Covariance Matrix".<br/>
![Manova - Covariance](figures/sur_cov.png)<br/>
Gambar 4. Covariance Matrix Residuals<br/>
3. Pilih sub tab menu "Model Estimation".<br/>
![Manova - Model](figures/sur_model.png)<br/>
Gambar 5. Hasil Estimasil Model<br/>

### Step 4
Uji kenormalan residual dari model yang diestimasi dengan Q-Q plot. Langkah-langkah:<br/>
1. Pilih tab menu "Plot".<br/>
![Manova - Plot](figures/sur_plot.png)<br/>
Gambar 6. Hasil Q-Q Plot<br/>

Demikian langkah-langkah dalam melakukan analisis SUR pada aplikasi ini. Semoga bermanfaat.<br/>
<hr></hr>
<center>&copy; Rifana Yuniar Rahman (2016)</center>