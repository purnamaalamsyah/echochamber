# 🚀 Panduan Deployment Political-RAG Indonesia

Dokumen ini berisi panduan lengkap untuk deploy aplikasi Political-RAG ke berbagai platform cloud.

---

## 📋 Daftar Isi

1. [Streamlit Community Cloud](#1-streamlit-community-cloud-rekomendasi)
2. [Render](#2-render)
3. [Railway](#3-railway)
4. [Hugging Face Spaces](#4-hugging-face-spaces)
5. [Docker](#5-docker-deployment)

---

## 1. Streamlit Community Cloud (REKOMENDASI)

**Keunggulan:** Gratis, mudah, khusus untuk Streamlit

### Langkah-langkah:

#### Step 1: Push ke GitHub
```bash
# Pastikan kode sudah di GitHub repository
git add .
git commit -m "Prepare for Streamlit Cloud deployment"
git push origin main
```

#### Step 2: Daftar Streamlit Cloud
1. Buka https://share.streamlit.io/
2. Klik **"Sign up"** dengan akun GitHub

#### Step 3: Deploy App
1. Klik **"New app"**
2. Isi form:
   - **Repository:** `username/echochamber`
   - **Branch:** `main` atau `claude/political-rag-streamlit-app-kjIi3`
   - **Main file path:** `political_rag_app/app.py`
3. Klik **"Deploy!"**

#### Step 4: Konfigurasi (Opsional)
1. Buka **"Settings"** > **"Secrets"**
2. Tambahkan API key jika diperlukan:
   ```toml
   GOOGLE_API_KEY = "your-api-key-here"
   ```

#### URL Aplikasi:
```
https://[your-app-name].streamlit.app
```

---

## 2. Render

**Keunggulan:** Free tier, auto-deploy dari GitHub

### Langkah-langkah:

#### Step 1: Daftar Render
1. Buka https://render.com/
2. Sign up dengan GitHub

#### Step 2: Create New Web Service
1. Klik **"New +"** > **"Web Service"**
2. Connect repository GitHub Anda
3. Konfigurasi:
   - **Name:** `political-rag-indonesia`
   - **Region:** `Singapore` (terdekat ke Indonesia)
   - **Branch:** `main`
   - **Root Directory:** `political_rag_app`
   - **Runtime:** `Python 3`
   - **Build Command:** `pip install -r requirements.txt`
   - **Start Command:** `streamlit run app.py --server.port $PORT --server.address 0.0.0.0 --server.headless true`

#### Step 3: Environment Variables
Tambahkan di **"Environment"**:
```
STREAMLIT_SERVER_HEADLESS=true
STREAMLIT_SERVER_ENABLE_CORS=false
```

#### Step 4: Deploy
Klik **"Create Web Service"**

#### URL Aplikasi:
```
https://political-rag-indonesia.onrender.com
```

---

## 3. Railway

**Keunggulan:** Cepat, mudah, generous free tier

### Langkah-langkah:

#### Step 1: Daftar Railway
1. Buka https://railway.app/
2. Sign up dengan GitHub

#### Step 2: Create New Project
1. Klik **"New Project"**
2. Pilih **"Deploy from GitHub repo"**
3. Pilih repository Anda

#### Step 3: Configure Service
1. Setelah deploy, klik service
2. Buka tab **"Settings"**
3. Set **Root Directory:** `political_rag_app`
4. Set **Start Command:**
   ```
   streamlit run app.py --server.port $PORT --server.address 0.0.0.0 --server.headless true
   ```

#### Step 4: Generate Domain
1. Buka tab **"Settings"**
2. Klik **"Generate Domain"**

#### URL Aplikasi:
```
https://[random-name].up.railway.app
```

---

## 4. Hugging Face Spaces

**Keunggulan:** Gratis, komunitas AI, GPU tersedia

### Langkah-langkah:

#### Step 1: Daftar Hugging Face
1. Buka https://huggingface.co/
2. Create account

#### Step 2: Create New Space
1. Klik **"New Space"**
2. Konfigurasi:
   - **Space name:** `political-rag-indonesia`
   - **License:** `MIT`
   - **SDK:** `Streamlit`
   - **Space hardware:** `CPU Basic` (gratis)

#### Step 3: Upload Files
Upload semua file dari `political_rag_app/`:
- `app.py`
- `document_processor.py`
- `rag_engine.py`
- `utils.py`
- `config.py`
- `requirements.txt`
- `.streamlit/config.toml`
- `sample_data/` (folder)

#### Step 4: Rename README
Rename `README_HF.md` menjadi `README.md` untuk Hugging Face

#### URL Aplikasi:
```
https://huggingface.co/spaces/[username]/political-rag-indonesia
```

---

## 5. Docker Deployment

**Untuk:** VPS, Cloud VM, Kubernetes

### Build Image:
```bash
cd political_rag_app
docker build -t political-rag-indonesia .
```

### Run Container:
```bash
docker run -d -p 8501:8501 --name political-rag political-rag-indonesia
```

### Docker Compose (docker-compose.yml):
```yaml
version: '3.8'
services:
  political-rag:
    build: .
    ports:
      - "8501:8501"
    environment:
      - STREAMLIT_SERVER_HEADLESS=true
    restart: unless-stopped
```

### Run dengan Compose:
```bash
docker-compose up -d
```

---

## 🔧 Troubleshooting

### Error: "No module named 'sentence_transformers'"
```bash
# Tambahkan ke requirements.txt
sentence-transformers>=2.2.0
```

### Error: Memory limit exceeded
- Gunakan plan berbayar dengan lebih banyak RAM
- Atau optimalkan chunk size di `config.py`

### Error: Port already in use
- Pastikan port 8501 tersedia
- Atau gunakan port lain dengan `--server.port`

### Slow cold start
- Normal untuk free tier
- Upgrade ke plan berbayar untuk performa lebih baik

---

## 📊 Perbandingan Platform

| Platform | Gratis | Sleep? | RAM | Deploy Time |
|----------|--------|--------|-----|-------------|
| Streamlit Cloud | ✅ | ✅ 7 hari | 1GB | ~2 menit |
| Render | ✅ | ✅ 15 menit | 512MB | ~5 menit |
| Railway | ✅ $5 | ❌ | 512MB | ~3 menit |
| HF Spaces | ✅ | ✅ 48 jam | 2GB | ~3 menit |

**Rekomendasi:** Streamlit Community Cloud untuk kemudahan dan stabilitas.

---

## 🔗 Quick Links

- Streamlit Cloud: https://share.streamlit.io/
- Render: https://render.com/
- Railway: https://railway.app/
- Hugging Face: https://huggingface.co/spaces

---

*Dibuat untuk Political-RAG Indonesia v1.0*
