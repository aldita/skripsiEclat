<h5><b>Gambaran Umum </b></h5>
<hr></hr>
Fuzzy Geographically Weigthed Clustering merupakan metode yang diusulkan oleh G.A Mason dan R.D Jacobson.
Fuzzy Geographically Weigthed Clustering merupakan pengembangan dari Fuzzy C-Means yang dikembangkan oleh Bezdek. Fuzzy Geographically Weigthed Clustering memberikan pendekatan dasar interaksi spasial dengan memberikan penimbang pada membership berdasarkan data jarak dan populasi.
<br/>
<hr></hr>
<h5><b> Langkah Kerja </b></h5>
1. Inisialisasi penimbang<br/>
![Inisialisasi Penimbang](/figures/penimbangFgwc.jpg)
2. Inisialisasi matriks U.<br/>
3. Hitung pusat klaster<br/>
![Update V](/figures/matriksVFgwc.jpg)
4. Perbaharui matriks U<br/>
![Update U](/figures/updateUFgwc.jpg)
5. Berikan penimbang pada matriks U<br/>
![Weigthing](/figures/beripenimbangFgwc.jpg)
6. Ulangi langkah 2 hingga kriteria berhenti terpenuhi.


<hr></hr>
<h5><b> Referensi: </b></h5>
<p>Mason, G. A., & Jacobson, R. D. (2007). Fuzzy Geographically Weighted Clustering. in Proceedings of the 9th International Conference on Geocomputation, (pp. 1-7). </p>