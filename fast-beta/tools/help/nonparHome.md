Halaman help ini membantu anda untuk menggunakan Fast statistical software untuk melakukan beberapa pengujian hipotesis Nonparametrik

### What can you do ?

Bagian ini menunjukkan metode - metode nonparametrik apa saja yang dapat pengguna gunakan untuk menganalisis data inputan.Pilihan metode - metode yang dapat digunakan, tergantung kepada Data Information

Terdapat 4 macam bentuk dari bagian ini sesuai dengan kondisinya :<br/>
1. Menampilkan 9 fungsi metode nonparametrik. ( kondisi : jumlah variabel pada Data Information lebih dari 2, dan salah satunya qualitatif atau quantitatif yang tidak normal)<br/>
2. Menampilkan 6 fungsi metode nonparametrik. ( kondisi : jumlah variabel pada Data Information adalah 2, dan salah satunya qualitatif atau quantitatif yang tidak normal)<br/>
3. Menampilkan 3 fungsi metode nonparametrik. ( kondisi : jumlah variabel pada Data Information adalah satu, dan berjenis qualitatif atau quantitatif yang tidak normal)<br/>
4. Tidak menampilkan fungsi, dan memberikan saran penggunaan uji parametrik.  ( kondisi :  semua variabel pada Data Information berjenis qualitatif atau quantitatif yang tidak normal)<br/>

Apabila bagian ini menunjukkan jenis ke-4, maka tombol "Start Hipotesis Testing" tidak akan muncul

### Data Information

Bagian ini menampilkan informasi mengenai data inputan pengguna. Awalnya, aplikasi akan secara otomatis menentukan type tiap variabel. Alurnya adalah : <br/>
1. Aplikasi mendeteksi apakah data pada variabel berbentuk numerik atau tidak. <br/>
2. Apabila data tidak numerik, maka type variabel ditetapkan qualitatif <br/>
3. Apabila numerik, maka akan dilakukan pengujian kenormalan dengan uji Shapiro wilk ( n < 50) atau uji Lilliefors (n >= 50), alpha 5%<br/>

Dibawah tabel data information, terdapat kotak isian "Change variable type to qualitative"

Kotak isian ini berfungsi untuk mengubah secara manual type variabel yang ada pada Data information dari qualitatif ke quantitatif.
Aplikasi tidak dapat mendeteksi data qualitatif yang berbentuk numerik. Contohnya data kode jenis kelamin (1,0), aplikasi akan tetap mendeteksi data tersebut bertipe quantitatif.
Pada beberapa metode, dibutuhkan penegasan mengenai type variabel dikarenakan perlakuan antar type variabel berbeda.

 
