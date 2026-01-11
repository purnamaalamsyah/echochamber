"""
Political-RAG: AI-Powered Political News Analyzer for Indonesia
A Streamlit-based web application for extracting and analyzing political information
from Indonesian media content using Retrieval-Augmented Generation (RAG)
"""

import streamlit as st
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import numpy as np
from collections import Counter
import os
from typing import List, Dict, Any

# Import custom modules
from document_processor import DocumentProcessor, TextChunker, PoliticalEntityExtractor, Document
from rag_engine import RAGEngine, QueryEngine, SimpleVectorStore


# ==================== PAGE CONFIGURATION ====================
st.set_page_config(
    page_title="Political-RAG Indonesia",
    page_icon="🏛️",
    layout="wide",
    initial_sidebar_state="expanded"
)

# ==================== CUSTOM CSS ====================
st.markdown("""
<style>
    /* Main theme colors */
    :root {
        --primary-color: #1e3a5f;
        --secondary-color: #c41e3a;
        --accent-color: #ffd700;
        --bg-light: #f8f9fa;
        --text-dark: #212529;
    }

    /* Header styling */
    .main-header {
        background: linear-gradient(135deg, #1e3a5f 0%, #2d5a87 100%);
        padding: 2rem;
        border-radius: 15px;
        color: white;
        margin-bottom: 2rem;
        box-shadow: 0 4px 15px rgba(0,0,0,0.2);
    }

    .main-header h1 {
        font-size: 2.5rem;
        font-weight: 700;
        margin-bottom: 0.5rem;
    }

    .main-header p {
        font-size: 1.1rem;
        opacity: 0.9;
    }

    /* Card styling */
    .metric-card {
        background: white;
        padding: 1.5rem;
        border-radius: 12px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.08);
        border-left: 4px solid #c41e3a;
        margin-bottom: 1rem;
    }

    .metric-card h3 {
        color: #1e3a5f;
        font-size: 0.9rem;
        text-transform: uppercase;
        letter-spacing: 1px;
        margin-bottom: 0.5rem;
    }

    .metric-card .value {
        font-size: 2rem;
        font-weight: 700;
        color: #c41e3a;
    }

    /* Chat styling */
    .chat-message {
        padding: 1rem;
        border-radius: 10px;
        margin-bottom: 1rem;
    }

    .chat-user {
        background: #e3f2fd;
        border-left: 4px solid #1976d2;
    }

    .chat-assistant {
        background: #f5f5f5;
        border-left: 4px solid #c41e3a;
    }

    /* Insight box */
    .insight-box {
        background: linear-gradient(135deg, #fff9e6 0%, #fff3cd 100%);
        padding: 1rem;
        border-radius: 10px;
        border-left: 4px solid #ffc107;
        margin: 1rem 0;
    }

    /* Source card */
    .source-card {
        background: #f8f9fa;
        padding: 1rem;
        border-radius: 8px;
        border: 1px solid #dee2e6;
        margin-bottom: 0.5rem;
    }

    /* Feature box */
    .feature-box {
        background: white;
        padding: 1.5rem;
        border-radius: 12px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.06);
        text-align: center;
        height: 100%;
    }

    .feature-box .icon {
        font-size: 2.5rem;
        margin-bottom: 1rem;
    }

    /* Sidebar styling */
    .sidebar-info {
        background: #f0f2f6;
        padding: 1rem;
        border-radius: 8px;
        margin-top: 1rem;
    }

    /* Button styling */
    .stButton>button {
        background: linear-gradient(135deg, #c41e3a 0%, #a01830 100%);
        color: white;
        border: none;
        padding: 0.5rem 2rem;
        border-radius: 25px;
        font-weight: 600;
        transition: all 0.3s ease;
    }

    .stButton>button:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(196, 30, 58, 0.4);
    }

    /* Tab styling */
    .stTabs [data-baseweb="tab-list"] {
        gap: 8px;
    }

    .stTabs [data-baseweb="tab"] {
        background: #f0f2f6;
        border-radius: 8px 8px 0 0;
        padding: 10px 20px;
    }

    .stTabs [aria-selected="true"] {
        background: #1e3a5f;
        color: white;
    }

    /* Hide Streamlit branding */
    #MainMenu {visibility: hidden;}
    footer {visibility: hidden;}
</style>
""", unsafe_allow_html=True)


# ==================== SESSION STATE INITIALIZATION ====================
if 'documents' not in st.session_state:
    st.session_state.documents = []
if 'chunked_documents' not in st.session_state:
    st.session_state.chunked_documents = []
if 'rag_engine' not in st.session_state:
    st.session_state.rag_engine = None
if 'query_engine' not in st.session_state:
    st.session_state.query_engine = None
if 'chat_history' not in st.session_state:
    st.session_state.chat_history = []
if 'analysis_results' not in st.session_state:
    st.session_state.analysis_results = None
if 'indexed' not in st.session_state:
    st.session_state.indexed = False


# ==================== HELPER FUNCTIONS ====================
def create_wordcloud_data(text: str) -> Dict[str, int]:
    """Create word frequency data for visualization"""
    words = text.lower().split()
    stop_words = {'yang', 'dan', 'di', 'ke', 'dari', 'ini', 'itu', 'untuk',
                  'dengan', 'pada', 'adalah', 'dalam', 'akan', 'tidak',
                  'juga', 'atau', 'ada', 'oleh', 'setelah', 'karena', 'saya',
                  'kami', 'kita', 'mereka', 'anda', 'tersebut', 'dapat', 'bisa',
                  'sudah', 'telah', 'belum', 'masih', 'seperti', 'sebagai', 'melalui'}

    filtered = [w for w in words if w not in stop_words and len(w) > 3]
    return dict(Counter(filtered).most_common(50))


def render_header():
    """Render the main header"""
    st.markdown("""
    <div class="main-header">
        <h1>🏛️ Political-RAG Indonesia</h1>
        <p>Sistem Analisis Cerdas untuk Ekstraksi Informasi Politik dari Media Indonesia</p>
    </div>
    """, unsafe_allow_html=True)


def render_sidebar():
    """Render sidebar with configuration options"""
    with st.sidebar:
        st.image("https://upload.wikimedia.org/wikipedia/commons/thumb/9/9f/Flag_of_Indonesia.svg/255px-Flag_of_Indonesia.svg.png",
                width=150)

        st.markdown("### ⚙️ Konfigurasi")

        # API Key input
        api_key = st.text_input(
            "Google AI API Key (Opsional)",
            type="password",
            help="Masukkan API key untuk mengaktifkan fitur AI Generatif. Dapatkan di https://makersuite.google.com/app/apikey"
        )

        if api_key:
            st.session_state.api_key = api_key
            st.success("✓ API Key tersimpan")

        st.markdown("---")

        # File upload section
        st.markdown("### 📁 Upload Dokumen")

        uploaded_files = st.file_uploader(
            "Upload file berita politik",
            type=['csv', 'xlsx', 'xls', 'txt'],
            accept_multiple_files=True,
            help="Format yang didukung: CSV, Excel (.xlsx, .xls), dan Text (.txt)"
        )

        if uploaded_files:
            if st.button("🔄 Proses Dokumen", use_container_width=True):
                process_documents(uploaded_files)

        st.markdown("---")

        # Stats
        if st.session_state.documents:
            st.markdown("### 📊 Statistik Data")
            st.markdown(f"""
            <div class="sidebar-info">
                <p><strong>Dokumen:</strong> {len(st.session_state.documents)}</p>
                <p><strong>Chunks:</strong> {len(st.session_state.chunked_documents)}</p>
                <p><strong>Status:</strong> {'✅ Terindeks' if st.session_state.indexed else '⏳ Belum Diindeks'}</p>
            </div>
            """, unsafe_allow_html=True)

        st.markdown("---")
        st.markdown("""
        <div style="text-align: center; color: #666; font-size: 0.8rem;">
            <p>Political-RAG v1.0</p>
            <p>Powered by Generative AI</p>
        </div>
        """, unsafe_allow_html=True)

    return api_key if 'api_key' in dir() else None


def process_documents(uploaded_files):
    """Process uploaded documents"""
    with st.spinner("Memproses dokumen..."):
        # Initialize processor
        processor = DocumentProcessor()
        chunker = TextChunker(chunk_size=1000, chunk_overlap=200)

        # Process files
        documents = processor.process_uploaded_files(uploaded_files)

        if not documents:
            st.error("Tidak ada dokumen yang dapat diproses.")
            return

        # Chunk documents
        chunked_docs = chunker.chunk_documents(documents)

        # Store in session state
        st.session_state.documents = documents
        st.session_state.chunked_documents = chunked_docs

        # Initialize RAG engine
        api_key = st.session_state.get('api_key', None)
        rag_engine = RAGEngine(api_key=api_key)
        rag_engine.initialize()

        # Index documents
        rag_engine.index_documents(chunked_docs)
        st.session_state.rag_engine = rag_engine
        st.session_state.query_engine = QueryEngine(rag_engine)
        st.session_state.indexed = True

        # Perform initial analysis
        st.session_state.analysis_results = rag_engine.analyze_political_landscape(documents)

        st.success(f"✅ Berhasil memproses {len(documents)} dokumen ({len(chunked_docs)} chunks)")


def render_welcome_page():
    """Render welcome page when no documents are uploaded"""
    st.markdown("""
    <div style="text-align: center; padding: 2rem;">
        <h2>Selamat Datang di Political-RAG Indonesia!</h2>
        <p style="font-size: 1.1rem; color: #666; margin: 1rem 0 2rem 0;">
            Sistem analisis cerdas berbasis AI untuk mengekstrak dan menganalisis informasi politik dari konten media Indonesia
        </p>
    </div>
    """, unsafe_allow_html=True)

    # Feature boxes
    col1, col2, col3, col4 = st.columns(4)

    with col1:
        st.markdown("""
        <div class="feature-box">
            <div class="icon">📄</div>
            <h4>Multi-Format</h4>
            <p>Upload CSV, Excel, atau file TXT</p>
        </div>
        """, unsafe_allow_html=True)

    with col2:
        st.markdown("""
        <div class="feature-box">
            <div class="icon">🔍</div>
            <h4>Smart Search</h4>
            <p>Pencarian semantik berbasis AI</p>
        </div>
        """, unsafe_allow_html=True)

    with col3:
        st.markdown("""
        <div class="feature-box">
            <div class="icon">📊</div>
            <h4>Visualisasi</h4>
            <p>Grafik dan insights interaktif</p>
        </div>
        """, unsafe_allow_html=True)

    with col4:
        st.markdown("""
        <div class="feature-box">
            <div class="icon">🤖</div>
            <h4>Generative AI</h4>
            <p>Jawaban kontekstual dari AI</p>
        </div>
        """, unsafe_allow_html=True)

    st.markdown("<br>", unsafe_allow_html=True)

    # Instructions
    st.markdown("""
    ### 📋 Cara Menggunakan:

    1. **Upload Dokumen** - Klik tombol upload di sidebar dan pilih file berita Anda
    2. **Proses Data** - Klik "Proses Dokumen" untuk mengindeks konten
    3. **Eksplorasi** - Gunakan tab yang tersedia untuk menganalisis data
    4. **Tanya AI** - Ajukan pertanyaan tentang konten politik

    ### 📁 Format Data yang Didukung:

    | Format | Deskripsi |
    |--------|-----------|
    | **CSV** | File dengan kolom yang berisi teks berita |
    | **Excel** | Spreadsheet dengan data berita politik |
    | **TXT** | File teks dengan konten berita (bisa multiple files) |

    """)

    # Sample data info
    with st.expander("📝 Contoh Format Data"):
        st.markdown("""
        **Untuk CSV/Excel:**
        ```
        judul,konten,tanggal,sumber
        "Pemilu 2024...","Isi berita tentang pemilu...","2024-01-15","Kompas"
        "Koalisi Partai...","Isi berita tentang koalisi...","2024-01-16","Tempo"
        ```

        **Untuk TXT:**
        ```
        Judul: Perkembangan Politik Terkini

        Paragraf pertama berita...

        Paragraf kedua berita...

        ---

        Judul: Berita Politik Lainnya

        Isi berita lainnya...
        ```
        """)


def render_dashboard():
    """Render main dashboard with analytics"""
    if not st.session_state.analysis_results:
        st.warning("Belum ada data untuk ditampilkan. Upload dokumen terlebih dahulu.")
        return

    results = st.session_state.analysis_results

    # Metrics row
    col1, col2, col3, col4 = st.columns(4)

    with col1:
        st.markdown(f"""
        <div class="metric-card">
            <h3>Total Dokumen</h3>
            <div class="value">{results['total_documents']}</div>
        </div>
        """, unsafe_allow_html=True)

    with col2:
        st.markdown(f"""
        <div class="metric-card">
            <h3>Relevansi Politik</h3>
            <div class="value">{results['avg_relevance']:.0%}</div>
        </div>
        """, unsafe_allow_html=True)

    with col3:
        top_party = results['top_parties'][0][0] if results['top_parties'] else "N/A"
        st.markdown(f"""
        <div class="metric-card">
            <h3>Partai Dominan</h3>
            <div class="value" style="font-size: 1.2rem;">{top_party}</div>
        </div>
        """, unsafe_allow_html=True)

    with col4:
        total_mentions = sum([c for _, c in results['top_parties']])
        st.markdown(f"""
        <div class="metric-card">
            <h3>Mentions Partai</h3>
            <div class="value">{total_mentions}</div>
        </div>
        """, unsafe_allow_html=True)

    st.markdown("<br>", unsafe_allow_html=True)

    # Charts row
    col1, col2 = st.columns(2)

    with col1:
        st.markdown("### 🏛️ Partai Politik Teratas")
        if results['top_parties']:
            df_parties = pd.DataFrame(results['top_parties'], columns=['Partai', 'Mentions'])
            fig = px.bar(
                df_parties,
                x='Mentions',
                y='Partai',
                orientation='h',
                color='Mentions',
                color_continuous_scale='Reds'
            )
            fig.update_layout(
                height=400,
                showlegend=False,
                xaxis_title="Jumlah Kemunculan",
                yaxis_title=""
            )
            st.plotly_chart(fig, use_container_width=True)
        else:
            st.info("Tidak ada data partai politik ditemukan")

    with col2:
        st.markdown("### 📊 Institusi Pemerintah")
        if results['top_institutions']:
            df_inst = pd.DataFrame(results['top_institutions'], columns=['Institusi', 'Mentions'])
            fig = px.pie(
                df_inst,
                values='Mentions',
                names='Institusi',
                color_discrete_sequence=px.colors.sequential.RdBu
            )
            fig.update_layout(height=400)
            st.plotly_chart(fig, use_container_width=True)
        else:
            st.info("Tidak ada data institusi ditemukan")

    # Keywords section
    st.markdown("### 🔑 Kata Kunci Politik Populer")

    if results['top_keywords']:
        df_keywords = pd.DataFrame(results['top_keywords'], columns=['Keyword', 'Count'])

        fig = px.treemap(
            df_keywords,
            path=['Keyword'],
            values='Count',
            color='Count',
            color_continuous_scale='Blues'
        )
        fig.update_layout(height=400)
        st.plotly_chart(fig, use_container_width=True)

    # Relevance distribution
    st.markdown("### 📈 Distribusi Relevansi Politik")

    if results['relevance_distribution']:
        fig = go.Figure()
        fig.add_trace(go.Histogram(
            x=results['relevance_distribution'],
            nbinsx=20,
            marker_color='#c41e3a',
            opacity=0.8
        ))
        fig.update_layout(
            xaxis_title="Skor Relevansi",
            yaxis_title="Jumlah Dokumen",
            height=300
        )
        st.plotly_chart(fig, use_container_width=True)


def render_qa_interface():
    """Render Q&A chat interface"""
    st.markdown("### 💬 Tanya Jawab dengan AI")

    if not st.session_state.indexed:
        st.warning("Upload dan proses dokumen terlebih dahulu untuk menggunakan fitur ini.")
        return

    # Suggested questions
    if st.session_state.query_engine:
        suggestions = st.session_state.query_engine.get_suggested_questions(
            st.session_state.documents
        )

        st.markdown("**💡 Pertanyaan yang Disarankan:**")
        cols = st.columns(4)
        for idx, suggestion in enumerate(suggestions[:4]):
            with cols[idx]:
                if st.button(suggestion[:40] + "...", key=f"suggest_{idx}"):
                    st.session_state.current_question = suggestion

    st.markdown("---")

    # Query mode selection
    col1, col2 = st.columns([3, 1])

    with col1:
        question = st.text_input(
            "Pertanyaan Anda:",
            value=st.session_state.get('current_question', ''),
            placeholder="Contoh: Apa isu politik utama dalam berita-berita ini?"
        )

    with col2:
        mode = st.selectbox(
            "Mode Analisis:",
            ["default", "analysis", "summary", "entities", "sentiment"],
            format_func=lambda x: {
                "default": "📝 Default",
                "analysis": "🔬 Analisis Mendalam",
                "summary": "📋 Ringkasan",
                "entities": "👥 Entitas Politik",
                "sentiment": "😊 Analisis Sentimen"
            }[x]
        )

    if st.button("🚀 Kirim Pertanyaan", use_container_width=True):
        if question:
            with st.spinner("Mencari dan menganalisis..."):
                result = st.session_state.query_engine.ask(question, mode)

                # Add to chat history
                st.session_state.chat_history.append({
                    'type': 'user',
                    'content': question
                })
                st.session_state.chat_history.append({
                    'type': 'assistant',
                    'content': result['response'],
                    'sources': result['sources']
                })

    # Display chat history
    st.markdown("---")
    st.markdown("### 📜 Riwayat Percakapan")

    for msg in st.session_state.chat_history[-10:]:  # Show last 10 messages
        if msg['type'] == 'user':
            st.markdown(f"""
            <div class="chat-message chat-user">
                <strong>🧑 Anda:</strong><br>
                {msg['content']}
            </div>
            """, unsafe_allow_html=True)
        else:
            st.markdown(f"""
            <div class="chat-message chat-assistant">
                <strong>🤖 AI:</strong><br>
                {msg['content']}
            </div>
            """, unsafe_allow_html=True)

            # Show sources
            if 'sources' in msg and msg['sources']:
                with st.expander("📚 Lihat Sumber"):
                    for source in msg['sources']:
                        st.markdown(f"""
                        <div class="source-card">
                            <strong>📄 {source['source']}</strong><br>
                            <small>Relevansi: {source['score']:.2%}</small><br>
                            <em>{source['preview']}...</em>
                        </div>
                        """, unsafe_allow_html=True)


def render_document_explorer():
    """Render document explorer interface"""
    st.markdown("### 📚 Eksplorasi Dokumen")

    if not st.session_state.documents:
        st.warning("Belum ada dokumen yang diupload.")
        return

    # Search and filter
    col1, col2 = st.columns([3, 1])

    with col1:
        search_term = st.text_input("🔍 Cari dalam dokumen:", placeholder="Masukkan kata kunci...")

    with col2:
        sort_by = st.selectbox("Urutkan:", ["Relevansi", "Sumber", "Panjang Teks"])

    # Filter documents
    filtered_docs = st.session_state.documents

    if search_term:
        filtered_docs = [
            doc for doc in filtered_docs
            if search_term.lower() in doc.content.lower()
        ]

    # Display documents
    st.markdown(f"**Menampilkan {len(filtered_docs)} dari {len(st.session_state.documents)} dokumen**")

    for idx, doc in enumerate(filtered_docs[:20]):  # Limit display
        extractor = PoliticalEntityExtractor()
        relevance = extractor.calculate_political_relevance(doc.content)
        entities = extractor.extract_entities(doc.content)

        with st.expander(f"📄 {doc.source} - Section {idx+1} (Relevansi: {relevance:.0%})"):
            st.markdown(f"**Sumber:** {doc.source}")
            st.markdown(f"**Relevansi Politik:** {relevance:.0%}")

            col1, col2, col3 = st.columns(3)
            with col1:
                st.markdown("**Partai:**")
                st.write(", ".join(entities['parties']) if entities['parties'] else "Tidak ada")
            with col2:
                st.markdown("**Kata Kunci:**")
                st.write(", ".join(entities['keywords'][:5]) if entities['keywords'] else "Tidak ada")
            with col3:
                st.markdown("**Institusi:**")
                st.write(", ".join(entities['institutions']) if entities['institutions'] else "Tidak ada")

            st.markdown("---")
            st.markdown("**Konten:**")
            st.text_area("", doc.content, height=200, key=f"doc_{idx}", disabled=True)


def render_topic_analysis():
    """Render topic clustering analysis"""
    st.markdown("### 🎯 Analisis Topik")

    if not st.session_state.rag_engine:
        st.warning("Proses dokumen terlebih dahulu untuk melihat analisis topik.")
        return

    num_topics = st.slider("Jumlah Cluster Topik:", 3, 10, 5)

    if st.button("🔄 Generate Topik"):
        with st.spinner("Menganalisis topik..."):
            topics = st.session_state.rag_engine.get_topic_clusters(num_topics)

            if topics:
                for topic in topics:
                    st.markdown(f"""
                    <div class="metric-card">
                        <h3>Topik {topic['id'] + 1}</h3>
                        <p><strong>Dokumen:</strong> {topic['num_documents']}</p>
                        <p><strong>Kata Kunci:</strong> {', '.join(topic['top_words'])}</p>
                        <p><em>{topic['sample_content']}...</em></p>
                    </div>
                    """, unsafe_allow_html=True)
            else:
                st.info("Tidak cukup dokumen untuk clustering topik.")


def render_export_page():
    """Render export/download options"""
    st.markdown("### 📥 Export Data")

    if not st.session_state.analysis_results:
        st.warning("Tidak ada data untuk diexport. Upload dan proses dokumen terlebih dahulu.")
        return

    results = st.session_state.analysis_results

    col1, col2 = st.columns(2)

    with col1:
        st.markdown("#### Hasil Analisis")

        # Create summary dataframe
        summary_data = {
            'Metrik': ['Total Dokumen', 'Rata-rata Relevansi Politik'],
            'Nilai': [results['total_documents'], f"{results['avg_relevance']:.2%}"]
        }
        df_summary = pd.DataFrame(summary_data)
        st.dataframe(df_summary, use_container_width=True)

        # Download button
        csv = df_summary.to_csv(index=False)
        st.download_button(
            "📥 Download Summary (CSV)",
            csv,
            "political_rag_summary.csv",
            "text/csv",
            use_container_width=True
        )

    with col2:
        st.markdown("#### Data Partai Politik")

        if results['top_parties']:
            df_parties = pd.DataFrame(results['top_parties'], columns=['Partai', 'Mentions'])
            st.dataframe(df_parties, use_container_width=True)

            csv = df_parties.to_csv(index=False)
            st.download_button(
                "📥 Download Partai (CSV)",
                csv,
                "political_parties.csv",
                "text/csv",
                use_container_width=True
            )

    # Chat history export
    if st.session_state.chat_history:
        st.markdown("#### Riwayat Percakapan")

        chat_export = []
        for msg in st.session_state.chat_history:
            chat_export.append({
                'type': msg['type'],
                'content': msg['content']
            })

        df_chat = pd.DataFrame(chat_export)
        csv = df_chat.to_csv(index=False)
        st.download_button(
            "📥 Download Chat History (CSV)",
            csv,
            "chat_history.csv",
            "text/csv",
            use_container_width=True
        )


# ==================== MAIN APPLICATION ====================
def main():
    """Main application entry point"""
    render_header()
    api_key = render_sidebar()

    # Check if documents are uploaded
    if not st.session_state.documents:
        render_welcome_page()
    else:
        # Main tabs
        tab1, tab2, tab3, tab4, tab5 = st.tabs([
            "📊 Dashboard",
            "💬 Tanya Jawab",
            "📚 Dokumen",
            "🎯 Analisis Topik",
            "📥 Export"
        ])

        with tab1:
            render_dashboard()

        with tab2:
            render_qa_interface()

        with tab3:
            render_document_explorer()

        with tab4:
            render_topic_analysis()

        with tab5:
            render_export_page()


if __name__ == "__main__":
    main()
