<h5><b> TEORI </b></h5>
Association rules mining adalah teknik dalam data mining untuk mencari pola dalam data besar yang berkaitan dengan studi “if-then”. Menurut Han et. al. (2006), association rule adalah metode data mining untuk mencari suatu hubungan yang menunjukkan kondisi di dalam satu set data. Association rule digunakan untuk menunjukkan hubungan antar-item data. Hubungan ini dituliskan dalam bentuk X ⇒ Y, dimana X dan Y adalah sekumpulan item. Association rules dapat dihasilkan setelah diketahui frequent itemsets dari basis data. Salah satu algoritme yang digunakan dalam menentukan frequent itemsets adalah Eclat.

Algoritme Equivalence Class Transformation (ECLAT) merupakan algoritme yang sangat sederhana untuk menemukan itemset yang paling sering muncul, pada dasarnya algoritma ECLAT melakukan pencarian secara depth-first search pada database dengan tata letak vertikal, jika database berbentuk horizontal maka harus dikonversikan ke bentuk vertikal terlebih dahulu.
Proses pencarian dilakukan dari item yang paling sering muncul hingga yang paling jarang muncul tanpa harus memperhatikan urutan, sehingga proses pemindaian tidak perlu dilakukan secara berulang-ulang. Data setiap itemset disimpan di sebuah Transaction Id List (TID List), kemudian TID List diurutkan berdasarkan transaksi yang mengandung itemset yang sama (frequent itemset). Selanjutnya k-itemset diatur kedalam kelas-kelas berdasarkan kriteria tertentu yang terbentuk dengan mempartisi suatu himpunan (equivalence class), (k+1)-itemset bisa didapat dengan menggabungkan pasangan frequent k-itemset dari kelas yang sama. Dalam prosesnya, algoritma ini dilakukan secara rekursif, dimana pencarian itemset akan terus dilakukan sepanjang masih ada itemset yang tersisa.

Dalam menentukan kemenarikan aturan hasil association rule, terdapat beberapa ukurun yang menjadi indikator kemenarikan aturan (interest). Beberapa ukuran tersebut diantaranya:

-Support : ukuran yang menunjukkan seberapa besar tingkat dominasi suatu item/itemset dari keseluruhan transaksi. Ukuran ini akan menentukan apakah suatu item/itemset layak untuk dicari confidence-nya (misal, dari seluruh transaksi yang ada, seberapa besar tingkat dominasi yang menunjukkan bahwa item A dan B dibeli bersamaan) dapat juga digunakan untuk mencari tingkat dominasi item tunggal.

-Confidence : ukuran yang menunjukkan hubungan antar 2 item secara conditional (misal, seberapa sering item B dibeli jika orang membeli item A).

-Lift : ukuran   yang menunjukkan kevalidan suatu aturan.

-Chi-Squared : ukuran yang menunjukkan hubungan yang saling bebas antar 2 item (misal, item A dan item B adalah 2 item yang tidak terkait).

-Imbalance Ratio : ukuran yang menunjukkan ketimpangan antar 2 item dalam suatu aturan (semakin timpang suatu aturan, semakin menarik).

-Kulczynski : ukuran yang menunjukkan korelasi antar 2 item.

-RPF : ukuran yang menunjukkan tingkat kepentingan dari suatu aturan.


<h5><b> LANGKAH-LANGKAH </b></h5>
Langkah-langkah dalam melakukan Analisis Association Rules menggunakan ECLAT antara lain:

1. Memilih variabel yang akan dianalisis pada Sidebar Data Properties;
2. Mencentang "Aggregate item" jika ingin melakukan agregasi dari single menjadi basket, lalu memilih variabel yang akan menjadi acuan agregasi(TID);
3. Menekan Subtab Transaction Data untuk melihat data dalam bentuk transaksi dan Subtab Vertical Transaction Data untuk melihat data dalam bentuk transaksi vertikal pada tab Data;
4. Memasukkan nilai minimum support pada Sidebar Data Properties;
5. Memasukkan nilai minimum confidence pada Sidebar Data Properties;
6. Memasukkan nilai minimum item pada Sidebar Data Properties;
7. Memasukkan nilai maximum item pada Sidebar Data Properties;
8. Mencentang "Remove Redundant" jika ingin melakukan penghapusan terhadap aturan yang berredudansi;
9. Mencentang "Add Interest Measure" jika ingin menambahkan ukuran interest, lalu memilih ukuran interest yang diinginkan;
10. Menekan Subtab Frequent Itemset untuk melihat item-item yang memiliki support diatas minimum support dan Subtab Rules untuk melihat aturan-aturan yang terbentuk pada tab Result;
11. Memasukkan jumlah item teratas untuk dilakukan plotting pada Sidebar Plot Properties;
12. Memasukkan jumlah aturan untuk dilakukan plotting pada Sidebar Plot Properties;
13. Menekan Subtab Plot untuk melihat Visualisasi dari hasil analisis. Terdapat empat Plot yang tersedia;
14. Menekan Sidebar Report jika ingin mengekspor hasil analisis dan membagikan ke dalam forum FAST.