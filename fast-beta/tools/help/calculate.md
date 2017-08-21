<p align="justify">Halaman ini memfasilitasi <i>user</i> untuk meng<i>cluster</i>kan data ke dalam peta. Algoritma <i>Fuzzy Geographically Weighted Clustering- Particle Swarm Optimization</i> membutuhkan data <b>populasi</b> dan <b>jarak antar wilayah</b> dari data yang akan di<i>cluster</i></p>
<br/><br/>
<b>LANGKAH-LANGKAH <i>CLUSTERING</i> DATA :</b>
<p align="justify"><b>1. </b>Pilih menu <i>Cluster Your Data</i></p> 
<p align="justify"><b>2. </b>Jika <i>user</i> menggunakan <b>Use Map</b>, maka <i>user</i> harus menyediakan sebuah peta. Kemudian pilih <i>Choose File</i></p> 
![CALCULATE upload_map](figures/uploadmap1.png)
<br/><br/><br/>
<p><i>User</i> harus memilih sejumlah file berekstensi .shp, .dbf, .gml, .shx, .xsd, dll</p>
![CALCULATE select_map](figures/selectmap1.png)
<br/><br/><br/>
<p align="justify"><b>3. </b>Masukkan data populasi. Data jarak tidak diperlukan, disebabkan jarak diambil dari pusat <i>polygon</i> shapefile yang telah di<i>upload</i>. Namun, user perlu mengupload data jarak jika user tidak mengupload shapefile</p>
<p align="justify"><b>4. </b>Pilih metode yang akan digunakan : FGWC atau FGWC-PSO</p>
![CALCULATE select_method](figures/selectmethod1.png)
<br/><br/><br/>
<p align="justify"><b>5. </b>Pilihan <i>user definable</i> digunakan apabila <i>user</i> ingin mengganti nilai default parameter dari metode yang digunakan </p>
<p align="justify"><b>6. </b>Masukkan nilai <i>fuzzifier</i> dan jumlah <i>cluster</i></p>
![CALCULATE select_cluster](figures/selectcluster1.png)
<br/><br/><br/>
<p align="justify"><b>7. </b>User dapat menyimpan hasil perhitungan menggunakan fungsi <b>download</b></p>
<br/><br/><br/>
<hr/>
<p>Pengaturan nilai parameter default adalah sebagai berikut</p>
<b>FGWC</b>
<p>Parameter&nbsp&nbsp-&nbsp&nbspValue</p>
<p>Alfa&nbsp&nbsp0.5</p>
<p>Beta&nbsp&nbsp0.5</p>
<p>a&nbsp&nbsp1</p>
<p>b&nbsp&nbsp1</p>
<p>e&nbsp&nbsp0.0001</p>
<br/>
<b>PSO</b>
<p>Parameter&nbsp&nbsp-&nbsp&nbspValue</p>
<p>c1&nbsp&nbsp2</p>
<p>c2&nbsp&nbsp2</p>
<p>Maks.Iter&nbsp&nbsp100</p>
<p>e&nbsp&nbsp0.0001</p>
<hr/>
<br/>
<b>HASIL CLUSTERING DATA</b>
<p align="justify">Hasil akan divisualisasikan dalam bentuk peta, jumlah cluster sesuai dengan input dari user. Jika user memberikan nilai c = 2, maka akan dihasilkan dua warna. Warna pertama menunjukkan cluster ke-1, sedangkan warna kedua menunjukkan cluster ke-2</p>
<br/>
![CALCULATE view_result](figures/viewresult1.png)
<br/><br/><br/>
<p align="justify">Pada tabel akan ditampilkan sejumlah data, antara lain</p>
![CALCULATE view_table](figures/viewresult21.png)
<br/><br/><br/>
<p align="justify"><b>1. </b>ID menunjukkan nomor daerah pada map</p>
<p align="justify"><b>2. </b>Proportion menunjukkan proporsi keanggotaan suatu daerah terhadap suatu cluster. Semakin besar nilai proporsi, maka semakin besar kecenderungan suatu daerah menjadi anggota cluster tertentu</p>
<p align="justify"><b>3. </b>Angka cluster menunjukkan suatu daerah menjadi anggota cluster tertentu </p>
<p align="justify"><b>4. </b>Data yang lain merupakan informasi yang terkandung dalam shapefile yang telah diupload oleh user</p>
