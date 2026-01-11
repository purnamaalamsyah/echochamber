"""
Configuration settings for Political-RAG Application
"""

# Application Settings
APP_TITLE = "Political-RAG Indonesia"
APP_ICON = "🏛️"
APP_VERSION = "1.0.0"

# Document Processing Settings
CHUNK_SIZE = 1000
CHUNK_OVERLAP = 200
MIN_CHUNK_LENGTH = 50

# Embedding Settings
EMBEDDING_MODEL = "all-MiniLM-L6-v2"
EMBEDDING_DIMENSION = 384

# Search Settings
DEFAULT_TOP_K = 5
MAX_TOP_K = 20

# UI Settings
MAX_CHAT_HISTORY = 50
MAX_DOCUMENT_PREVIEW = 500
MAX_DOCUMENTS_DISPLAY = 20

# Color Scheme
COLORS = {
    'primary': '#1e3a5f',
    'secondary': '#c41e3a',
    'accent': '#ffd700',
    'success': '#28a745',
    'warning': '#ffc107',
    'danger': '#dc3545',
    'info': '#17a2b8',
    'light': '#f8f9fa',
    'dark': '#212529'
}

# Indonesian Political Entities
POLITICAL_PARTIES = [
    ('PDI-P', 'Partai Demokrasi Indonesia Perjuangan'),
    ('Golkar', 'Partai Golongan Karya'),
    ('Gerindra', 'Partai Gerakan Indonesia Raya'),
    ('Nasdem', 'Partai Nasional Demokrat'),
    ('PKB', 'Partai Kebangkitan Bangsa'),
    ('Demokrat', 'Partai Demokrat'),
    ('PKS', 'Partai Keadilan Sejahtera'),
    ('PAN', 'Partai Amanat Nasional'),
    ('PPP', 'Partai Persatuan Pembangunan'),
    ('Perindo', 'Partai Persatuan Indonesia'),
    ('PSI', 'Partai Solidaritas Indonesia'),
    ('Hanura', 'Partai Hati Nurani Rakyat'),
    ('PKP', 'Partai Keadilan dan Persatuan Indonesia'),
    ('Berkarya', 'Partai Berkarya'),
    ('Garuda', 'Partai Gerakan Perubahan Indonesia')
]

GOVERNMENT_INSTITUTIONS = [
    ('KPU', 'Komisi Pemilihan Umum'),
    ('Bawaslu', 'Badan Pengawas Pemilu'),
    ('MK', 'Mahkamah Konstitusi'),
    ('MA', 'Mahkamah Agung'),
    ('KPK', 'Komisi Pemberantasan Korupsi'),
    ('BPK', 'Badan Pemeriksa Keuangan'),
    ('DPR', 'Dewan Perwakilan Rakyat'),
    ('MPR', 'Majelis Permusyawaratan Rakyat'),
    ('DPD', 'Dewan Perwakilan Daerah'),
    ('Kemendagri', 'Kementerian Dalam Negeri'),
    ('Kemlu', 'Kementerian Luar Negeri'),
    ('Kemhan', 'Kementerian Pertahanan'),
    ('Kemenkeu', 'Kementerian Keuangan'),
    ('Kemenkes', 'Kementerian Kesehatan'),
    ('Kemendikbud', 'Kementerian Pendidikan dan Kebudayaan'),
    ('Kemenag', 'Kementerian Agama'),
    ('Kemensos', 'Kementerian Sosial'),
    ('Kominfo', 'Kementerian Komunikasi dan Informatika')
]

# Analysis Modes
ANALYSIS_MODES = {
    'default': {
        'name': 'Default',
        'icon': '📝',
        'description': 'Jawaban langsung berdasarkan konteks'
    },
    'analysis': {
        'name': 'Analisis Mendalam',
        'icon': '🔬',
        'description': 'Breakdown komprehensif dengan implikasi politik'
    },
    'summary': {
        'name': 'Ringkasan',
        'icon': '📋',
        'description': 'Poin-poin kunci dalam format ringkas'
    },
    'entities': {
        'name': 'Entitas Politik',
        'icon': '👥',
        'description': 'Identifikasi aktor dan institusi politik'
    },
    'sentiment': {
        'name': 'Analisis Sentimen',
        'icon': '😊',
        'description': 'Analisis tone dan sentimen berita'
    }
}

# Suggested Questions
DEFAULT_QUESTIONS = [
    "Apa isu politik utama yang dibahas dalam berita-berita ini?",
    "Bagaimana sentimen media terhadap pemerintah?",
    "Siapa saja tokoh politik yang paling sering disebutkan?",
    "Apa dinamika koalisi politik yang terlihat?",
    "Bagaimana liputan media tentang pemilu?",
    "Apa kebijakan pemerintah yang paling banyak disorot?",
    "Bagaimana posisi partai oposisi dalam pemberitaan?",
    "Apa isu kontroversial yang muncul dalam berita?"
]
