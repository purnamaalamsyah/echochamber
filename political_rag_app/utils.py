"""
Utility Functions for Political-RAG Application
"""

import re
from typing import List, Dict, Any, Tuple
from collections import Counter
import numpy as np


def clean_text(text: str) -> str:
    """Clean and normalize Indonesian text"""
    if not text:
        return ""

    # Remove extra whitespace
    text = re.sub(r'\s+', ' ', text)

    # Remove special characters but keep Indonesian characters
    text = re.sub(r'[^\w\s\-.,!?()\'\"]+', '', text)

    # Normalize quotes
    text = text.replace('"', '"').replace('"', '"')
    text = text.replace(''', "'").replace(''', "'")

    return text.strip()


def extract_dates(text: str) -> List[str]:
    """Extract dates from Indonesian text"""
    date_patterns = [
        r'\d{1,2}[-/]\d{1,2}[-/]\d{2,4}',  # DD-MM-YYYY or DD/MM/YYYY
        r'\d{1,2}\s+(?:Januari|Februari|Maret|April|Mei|Juni|Juli|Agustus|September|Oktober|November|Desember)\s+\d{4}',
        r'\d{4}[-/]\d{1,2}[-/]\d{1,2}',  # YYYY-MM-DD
    ]

    dates = []
    for pattern in date_patterns:
        matches = re.findall(pattern, text, re.IGNORECASE)
        dates.extend(matches)

    return list(set(dates))


def extract_numbers(text: str) -> List[Tuple[str, str]]:
    """Extract numbers with context from text"""
    # Pattern to match numbers with surrounding context
    pattern = r'(\w+\s+)?(\d+(?:[.,]\d+)?(?:\s*(?:juta|miliar|triliun|ribu|persen|%))?)(\s+\w+)?'

    matches = re.findall(pattern, text)
    results = []

    for match in matches:
        context = f"{match[0].strip()} {match[1]} {match[2].strip()}".strip()
        if match[1]:
            results.append((match[1], context))

    return results


def calculate_text_statistics(text: str) -> Dict[str, Any]:
    """Calculate various statistics about the text"""
    words = text.split()
    sentences = re.split(r'[.!?]+', text)

    return {
        'char_count': len(text),
        'word_count': len(words),
        'sentence_count': len([s for s in sentences if s.strip()]),
        'avg_word_length': np.mean([len(w) for w in words]) if words else 0,
        'avg_sentence_length': np.mean([len(s.split()) for s in sentences if s.strip()]) if sentences else 0
    }


def get_indonesian_stopwords() -> set:
    """Return a set of Indonesian stopwords"""
    return {
        'yang', 'dan', 'di', 'ke', 'dari', 'ini', 'itu', 'untuk', 'dengan',
        'pada', 'adalah', 'dalam', 'akan', 'tidak', 'juga', 'atau', 'ada',
        'oleh', 'setelah', 'karena', 'saya', 'kami', 'kita', 'mereka', 'anda',
        'tersebut', 'dapat', 'bisa', 'sudah', 'telah', 'belum', 'masih',
        'seperti', 'sebagai', 'melalui', 'bahwa', 'secara', 'antara', 'saat',
        'ketika', 'jika', 'bila', 'agar', 'supaya', 'namun', 'tetapi', 'sedang',
        'serta', 'yaitu', 'yakni', 'maupun', 'hingga', 'sampai', 'selama',
        'sebelum', 'sesudah', 'terhadap', 'tentang', 'kepada', 'bagi', 'atas',
        'bawah', 'lain', 'semua', 'setiap', 'beberapa', 'banyak', 'sedikit',
        'hanya', 'sangat', 'lebih', 'kurang', 'paling', 'cukup', 'hampir',
        'sekitar', 'kira', 'mungkin', 'tentu', 'pasti', 'memang', 'sungguh'
    }


def extract_key_phrases(text: str, top_n: int = 10) -> List[Tuple[str, int]]:
    """Extract key phrases from Indonesian text using n-grams"""
    stopwords = get_indonesian_stopwords()

    # Clean and tokenize
    words = re.findall(r'\b\w+\b', text.lower())
    filtered_words = [w for w in words if w not in stopwords and len(w) > 2]

    # Generate bigrams
    bigrams = [f"{filtered_words[i]} {filtered_words[i+1]}"
               for i in range(len(filtered_words)-1)]

    # Count frequencies
    bigram_counts = Counter(bigrams)

    return bigram_counts.most_common(top_n)


def sentiment_keywords() -> Dict[str, List[str]]:
    """Return sentiment indicator keywords for Indonesian"""
    return {
        'positive': [
            'baik', 'bagus', 'sukses', 'berhasil', 'positif', 'optimis',
            'meningkat', 'maju', 'berkembang', 'unggul', 'prestasi',
            'apresiasi', 'dukungan', 'setuju', 'mendukung', 'terbaik',
            'hebat', 'luar biasa', 'cemerlang', 'gemilang'
        ],
        'negative': [
            'buruk', 'gagal', 'negatif', 'pesimis', 'menurun', 'mundur',
            'kritik', 'keluhan', 'masalah', 'kendala', 'hambatan',
            'kontroversi', 'skandal', 'korupsi', 'penolakan', 'protes',
            'demonstrasi', 'kecewa', 'marah', 'khawatir'
        ],
        'neutral': [
            'menyatakan', 'mengatakan', 'mengungkapkan', 'menjelaskan',
            'dibahas', 'membahas', 'disampaikan', 'dilaporkan', 'tercatat'
        ]
    }


def simple_sentiment_analysis(text: str) -> Dict[str, Any]:
    """Perform simple keyword-based sentiment analysis"""
    text_lower = text.lower()
    keywords = sentiment_keywords()

    positive_count = sum(1 for word in keywords['positive'] if word in text_lower)
    negative_count = sum(1 for word in keywords['negative'] if word in text_lower)
    neutral_count = sum(1 for word in keywords['neutral'] if word in text_lower)

    total = positive_count + negative_count + neutral_count

    if total == 0:
        sentiment = 'neutral'
        confidence = 0.5
    elif positive_count > negative_count:
        sentiment = 'positive'
        confidence = positive_count / total
    elif negative_count > positive_count:
        sentiment = 'negative'
        confidence = negative_count / total
    else:
        sentiment = 'neutral'
        confidence = 0.5

    return {
        'sentiment': sentiment,
        'confidence': confidence,
        'positive_indicators': positive_count,
        'negative_indicators': negative_count,
        'neutral_indicators': neutral_count
    }


def format_number_id(number: float) -> str:
    """Format number in Indonesian style"""
    if number >= 1_000_000_000_000:
        return f"{number/1_000_000_000_000:.1f} triliun"
    elif number >= 1_000_000_000:
        return f"{number/1_000_000_000:.1f} miliar"
    elif number >= 1_000_000:
        return f"{number/1_000_000:.1f} juta"
    elif number >= 1_000:
        return f"{number/1_000:.1f} ribu"
    else:
        return str(int(number))


def highlight_political_terms(text: str) -> str:
    """Add HTML highlighting to political terms"""
    terms_to_highlight = [
        'PDI-P', 'PDIP', 'Golkar', 'Gerindra', 'Nasdem', 'PKB',
        'Demokrat', 'PKS', 'PAN', 'PPP', 'Perindo', 'PSI', 'Hanura',
        'KPU', 'Bawaslu', 'MK', 'DPR', 'MPR', 'MA', 'KPK', 'BPK',
        'pemilu', 'pilpres', 'pilkada', 'koalisi', 'oposisi'
    ]

    result = text
    for term in terms_to_highlight:
        pattern = re.compile(re.escape(term), re.IGNORECASE)
        result = pattern.sub(f'<mark>{term}</mark>', result)

    return result


def truncate_text(text: str, max_length: int = 200, suffix: str = "...") -> str:
    """Truncate text to specified length at word boundary"""
    if len(text) <= max_length:
        return text

    truncated = text[:max_length]
    last_space = truncated.rfind(' ')

    if last_space > max_length * 0.8:
        truncated = truncated[:last_space]

    return truncated + suffix


def generate_document_id(content: str, source: str) -> str:
    """Generate a unique document ID"""
    import hashlib
    combined = f"{source}_{content[:100]}"
    return hashlib.md5(combined.encode()).hexdigest()[:12]


def categorize_political_content(text: str) -> str:
    """Categorize political content by topic"""
    categories = {
        'pemilu': ['pemilu', 'pilpres', 'pilkada', 'caleg', 'capres', 'cawapres', 'kampanye', 'suara'],
        'legislatif': ['dpr', 'dprd', 'mpr', 'undang-undang', 'ruu', 'legislasi', 'fraksi'],
        'eksekutif': ['presiden', 'menteri', 'kabinet', 'pemerintah', 'gubernur', 'bupati', 'walikota'],
        'yudikatif': ['mahkamah', 'mk', 'ma', 'pengadilan', 'hakim', 'putusan', 'vonis'],
        'partai': ['partai', 'koalisi', 'oposisi', 'ketua umum', 'kader', 'kongres'],
        'korupsi': ['korupsi', 'kpk', 'suap', 'gratifikasi', 'tersangka', 'terdakwa']
    }

    text_lower = text.lower()
    category_scores = {}

    for category, keywords in categories.items():
        score = sum(1 for keyword in keywords if keyword in text_lower)
        if score > 0:
            category_scores[category] = score

    if not category_scores:
        return 'umum'

    return max(category_scores, key=category_scores.get)
