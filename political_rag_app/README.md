# 🏛️ Political-RAG Indonesia

**Sistem Analisis Cerdas untuk Ekstraksi Informasi Politik dari Media Indonesia**

Political-RAG adalah aplikasi berbasis web yang menggunakan teknologi Retrieval-Augmented Generation (RAG) dan Generative AI untuk mengekstrak dan menganalisis informasi politik dari konten media Indonesia.

![Python](https://img.shields.io/badge/Python-3.9+-blue.svg)
![Streamlit](https://img.shields.io/badge/Streamlit-1.28+-red.svg)
![License](https://img.shields.io/badge/License-MIT-green.svg)

## ✨ Fitur Utama

- **📄 Multi-Format Support**: Upload file CSV, Excel (.xlsx, .xls), dan TXT (multiple files)
- **🔍 Smart Semantic Search**: Pencarian berbasis AI menggunakan embeddings
- **📊 Interactive Dashboard**: Visualisasi data politik yang menarik dan insightful
- **💬 AI-Powered Q&A**: Tanya jawab cerdas tentang konten politik
- **🎯 Topic Clustering**: Analisis dan pengelompokan topik otomatis
- **📥 Export Capabilities**: Export hasil analisis ke CSV

## 🚀 Cara Menjalankan

### Prerequisites

- Python 3.9 atau lebih tinggi
- pip (Python package manager)

### Instalasi

1. Clone repository atau navigasi ke direktori project:
```bash
cd political_rag_app
```

2. Install dependencies:
```bash
pip install -r requirements.txt
```

3. Jalankan aplikasi:
```bash
streamlit run app.py
```

4. Buka browser dan akses:
```
http://localhost:8501
```

## 📁 Struktur Direktori

```
political_rag_app/
├── app.py                 # Aplikasi Streamlit utama
├── document_processor.py  # Modul pemrosesan dokumen
├── rag_engine.py         # RAG engine dan vector store
├── requirements.txt      # Dependencies
├── README.md            # Dokumentasi
└── sample_data/         # Data contoh
    ├── berita_politik_sample.csv
    └── berita_politik_sample.txt
```

## 📋 Format Data yang Didukung

### CSV/Excel
```csv
judul,konten,tanggal,sumber
"Judul Berita 1","Isi konten berita politik...","2024-01-15","Kompas"
"Judul Berita 2","Isi konten berita lainnya...","2024-01-16","Tempo"
```

### TXT
```
Judul: Berita Politik

Paragraf pertama berita politik...

Paragraf kedua berita politik...

---

Judul: Berita Selanjutnya

Isi berita berikutnya...
```

## 🔧 Konfigurasi

### Google AI API Key (Opsional)
Untuk mengaktifkan fitur Generative AI, dapatkan API key dari:
https://makersuite.google.com/app/apikey

Masukkan API key di sidebar aplikasi.

## 📊 Fitur Analisis

### 1. Dashboard
- Statistik dokumen dan relevansi politik
- Grafik partai politik teratas
- Distribusi institusi pemerintah
- Word cloud kata kunci politik

### 2. Tanya Jawab AI
Mode analisis yang tersedia:
- **Default**: Jawaban langsung
- **Analisis Mendalam**: Breakdown komprehensif
- **Ringkasan**: Poin-poin kunci
- **Entitas Politik**: Identifikasi aktor politik
- **Sentimen**: Analisis sentimen berita

### 3. Eksplorasi Dokumen
- Pencarian dan filter dokumen
- Ekstraksi entitas per dokumen
- Skor relevansi politik

### 4. Analisis Topik
- Clustering dokumen otomatis
- Identifikasi tema utama

## 🏷️ Entitas Politik yang Dikenali

### Partai Politik
PDI-P, Golkar, Gerindra, Nasdem, PKB, Demokrat, PKS, PAN, PPP, Perindo, PSI, Hanura

### Institusi
KPU, Bawaslu, MK, MA, KPK, BPK, DPR, MPR, dan kementerian

### Kata Kunci
Pemilu, Pilpres, Pilkada, Koalisi, Oposisi, Kampanye, dll.

## 🛠️ Teknologi yang Digunakan

- **Streamlit**: Web framework
- **LangChain**: RAG framework
- **Sentence Transformers**: Text embeddings
- **Google Generative AI**: LLM (optional)
- **Plotly**: Visualisasi interaktif
- **Pandas**: Data processing

## 🚀 Deployment

Aplikasi ini dapat di-deploy ke berbagai platform cloud. Lihat [DEPLOYMENT.md](DEPLOYMENT.md) untuk panduan lengkap.

### Quick Deploy Options:

| Platform | Link | Status |
|----------|------|--------|
| **Streamlit Cloud** | [share.streamlit.io](https://share.streamlit.io/) | Recommended |
| **Render** | [render.com](https://render.com/) | Free tier |
| **Railway** | [railway.app](https://railway.app/) | Easy setup |
| **Hugging Face** | [huggingface.co/spaces](https://huggingface.co/spaces) | AI community |

### Deploy ke Streamlit Cloud (Tercepat):
1. Push repo ke GitHub
2. Buka [share.streamlit.io](https://share.streamlit.io/)
3. Connect repo dan pilih `political_rag_app/app.py`
4. Klik Deploy!

## 📝 Lisensi

MIT License - Bebas digunakan untuk keperluan pendidikan dan penelitian.

## 👥 Kontribusi

Kontribusi sangat diterima! Silakan buat pull request atau issue untuk perbaikan dan fitur baru.

---

**Dibuat dengan ❤️ untuk analisis politik Indonesia**
