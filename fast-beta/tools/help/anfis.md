Halaman help ini dapat digunakan untuk menjelaskan prosedur analisis regresi dengan menggunakan ANFIS.

<h3><b>Regresi ANFIS</b></h3>
Sebagai contoh awal, anda bisa menggunakan data Gas Furnace. Data ini dapat anda load melalui menu Data. Klik pada radio button Examples dan tekan Load Examples.<br/>

Data Gas Furnace ini merupakan data benchmark yang terdiri dari 292 observasi dan 3 variabel. V1 adalah jumlah gas methane pada waktu (t-4), V2 adalah jumlah gas CO2 pada waktu (t-1). V3 adalah jumlah CO2 pada waktu ke t, yang digunakan sebagai output variable, atau variabel dependen.<br/>

Dalam melakukan analisis regresi dengan ANFIS, anda hanya perlu mengakses tab data identification dan tab regression.<br/>
<h5>Pada tab data identification:</h5>
Cek range dimana nilai maksimum dan minimum dari data. Untuk nilai maksimum dan minimum variabel dependen dapat anda lihat pada sub tab dependent variable plot. Nilai maksimum dan minimum variabel independen dapat anda lihat pada sub tab independent variable. Usahakan agar nilai-nilai maksimum dan minimum dari variabel dependen dan variabel independen ini tercakup dalam range yang akan anda jadikan sebagai data training.<br/>
<h5>Pada tab regression:</h5>
1. Dari informasi yang anda dapatkan pada tab data identification, tentukan range data yang hendak anda jadikan sebagai data training.<br/>
2. Set parameter. Rule of thumb pengaturan parameter adalah<br/>
-Mengatur nilai variabel linguistik seminimum mungkin. Untuk kasus nonlinear, anda bisa gunakan nilai variabel linguistik 5. Untuk kasus standar, anda bisa gunakan nilai variabel linguistik 2-4. Usahakan tidak menggunakan nilai variabel linguistik lebih dari 7 agar anda tidak terjebak oleh curse of dimensionality dan perhitungan yang lambat.<br/>
-Mulai iterasi dari iterasi rendah terlebih dahulu (2-10) untuk memastikan bahwa inisiasi random yang digunakan pada optimasi gradient descent berada posisi dimana dimungkinkan dilakukan optimasi. Untuk mengganti atau melanjutkan iterasi, anda bisa gunakan set seed.<br/>
-T norm, S norm dan implication function dapat disesuaikan secara subyektif.<br/>
3. Mulai proses learning dengan menekan tombol Start Learning.<br/>
4. Lihat summary, plot dan residual fitting dan predicting. Jika error telah memenuhi kriteria, anda bisa menyimpan model dengan menekan tombol download yang terletak pada sub tab summary. Model ini dapat anda gunakan untuk memprediksi keluaran dari data baru pada tab Predict New Data Using Existing Model.<br/>

<h3><b>K-Fold Cross Validation</b></h3>
K-fold cross validation ini dapat digunakan untuk pemilihan model. Untuk pencarian model, anda bisa gunakan fungsi pada tab regression. Namun, jika anda hendak menentukan model mana yang hendak dipilih dari serangkaian model, anda bisa gunakan fitur ini.<br/>
Mekanismenya pada dasarnya sama dengan regresi biasa. Hanya saja disini anda tidak perlu lagi memilih range data training. Range data akan ditentukan secara otomatis berdasarkan pemilihan k.<br/>
Penjelasan singkat tentang k: k adalah banyaknya fold yang akan anda gunakan. Jika fold=2, maka anda akan bagi data menjadi 2 fold (lipatan). Selanjutnya pada iterasi pertama, data pada fold pertama akan digunakan untuk membuat model, dimana model ini akan digunakan untuk melakukan prediksi terhadap fold kedua. Pada iterasi kedua, data pada fold kedua akan digunakan untuk membuat model, dimana model ini akan digunakan untuk melakukan prediksi terhadap fold pertama. Nilai error akan dihitung kemudian dikembalikan ke pengguna. Singkatnya, pada iterasi ke-n, fold ke-n akan digunakan untuk menerapkan model yang dibuat dari hasil learing terhadap (n-1) fold sisanya.<br/>
<h5>Langkah-langkahnya:</h5>
1. Set jumlah k.
2. Set parameter.
3. Tekan tombol Start K-Fold Cross Validation, proses akan berjalan.
4. Hasil akan diberikan berupa residual per fold dan rata-ratanya.

<h3><b>Predict New Data Using Existing Model</b></h3>
Ini adalah fitur yang bisa anda gunakan jika anda hendak memprediksi data baru dengan menggunakan model yang telah anda miliki. Jika anda belum memiliki model, maka anda harus membuatnya terlebih dahulu di tab regression.<br/>
<h5>Langkah-langkahnya:</h5>
1. Upload model, klik next.
2. Upload data baru, klik next.
3. Hasil prediksi data baru anda peroleh.

Itulah langkah-langkah penggunaan aplikasi ini untuk melakukan analisis menggunakan regresi ANFIS. Semoga bermanfaat.<br/>
<hr></hr>
<center>&copy; Prima Tahapary (2015)</center>