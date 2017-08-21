Halaman help ini membantu anda untuk mengetahui analisis Random Forest pada FAST

Semua file contoh dapat di load melalui Data > Manage. Klik radio buttton 'examples' dan tekan 'Load examples'

####Pemilihan Variabel####
1. Variabel Dependen: variabel target/respons/kelas yang ingin diklasifikasi berupa factor.
2. Variabel Independen: variabel yang berperan sebagai predictor untuk mengklasifikasikan variabel dependen.

####Random Forest Options####
Bagi pengguna yang mengerti random forest mengatur parameter random forest secara manual untuk mendapatkan model yang optimal.

1. Jumlah sampel yang ditarik: ini adalah tahapan bootstrap, dimana setiap bootstrap yang diambil sebanyak n yang ditentukan.
2. Jumlah predictor: Ketika akan melakukan pembuatan pohon terbaik, maka jumlah predictor yang dipilih secara acak adalah 
   sebanyak m < jumlah variabel independen yang digunakan.
   