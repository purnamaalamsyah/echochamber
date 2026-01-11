"""
RAG Engine for Political Information Extraction
Uses vector embeddings and LLM for intelligent Q&A
"""

import os
from typing import List, Dict, Any, Optional, Tuple
import numpy as np
from dataclasses import dataclass
import hashlib
import json

# For embeddings
try:
    from sentence_transformers import SentenceTransformer
    HAS_SENTENCE_TRANSFORMERS = True
except ImportError:
    HAS_SENTENCE_TRANSFORMERS = False

# For Google Generative AI
try:
    import google.generativeai as genai
    HAS_GOOGLE_AI = True
except ImportError:
    HAS_GOOGLE_AI = False

from document_processor import Document


@dataclass
class SearchResult:
    """Represents a search result from the vector store"""
    document: Document
    score: float
    rank: int


class SimpleVectorStore:
    """Simple in-memory vector store using numpy"""

    def __init__(self):
        self.embeddings: Optional[np.ndarray] = None
        self.documents: List[Document] = []
        self.embedding_model = None

    def initialize_embeddings(self, model_name: str = "all-MiniLM-L6-v2"):
        """Initialize the embedding model"""
        if HAS_SENTENCE_TRANSFORMERS:
            self.embedding_model = SentenceTransformer(model_name)
        else:
            # Fallback to simple TF-IDF-like embeddings
            self.embedding_model = None

    def add_documents(self, documents: List[Document]):
        """Add documents to the vector store"""
        self.documents.extend(documents)

        # Generate embeddings
        texts = [doc.content for doc in documents]

        if self.embedding_model:
            new_embeddings = self.embedding_model.encode(texts)
        else:
            new_embeddings = self._simple_embeddings(texts)

        if self.embeddings is None:
            self.embeddings = new_embeddings
        else:
            self.embeddings = np.vstack([self.embeddings, new_embeddings])

    def _simple_embeddings(self, texts: List[str], dim: int = 384) -> np.ndarray:
        """Fallback simple hash-based embeddings"""
        embeddings = []
        for text in texts:
            # Create deterministic embeddings from text hash
            hash_val = hashlib.md5(text.encode()).hexdigest()
            np.random.seed(int(hash_val[:8], 16))
            embedding = np.random.randn(dim)
            embedding = embedding / np.linalg.norm(embedding)
            embeddings.append(embedding)
        return np.array(embeddings)

    def search(self, query: str, top_k: int = 5) -> List[SearchResult]:
        """Search for similar documents"""
        if len(self.documents) == 0:
            return []

        # Get query embedding
        if self.embedding_model:
            query_embedding = self.embedding_model.encode([query])[0]
        else:
            query_embedding = self._simple_embeddings([query])[0]

        # Calculate cosine similarity
        similarities = np.dot(self.embeddings, query_embedding) / (
            np.linalg.norm(self.embeddings, axis=1) * np.linalg.norm(query_embedding)
        )

        # Get top-k indices
        top_indices = np.argsort(similarities)[::-1][:top_k]

        results = []
        for rank, idx in enumerate(top_indices):
            results.append(SearchResult(
                document=self.documents[idx],
                score=float(similarities[idx]),
                rank=rank + 1
            ))

        return results

    def clear(self):
        """Clear all documents and embeddings"""
        self.embeddings = None
        self.documents = []


class RAGEngine:
    """Main RAG Engine for Political Information Extraction"""

    def __init__(self, api_key: Optional[str] = None):
        self.vector_store = SimpleVectorStore()
        self.api_key = api_key
        self.llm_initialized = False

        # Initialize LLM if API key provided
        if api_key and HAS_GOOGLE_AI:
            self._initialize_llm(api_key)

    def _initialize_llm(self, api_key: str):
        """Initialize Google Generative AI"""
        try:
            genai.configure(api_key=api_key)
            self.model = genai.GenerativeModel('gemini-pro')
            self.llm_initialized = True
        except Exception as e:
            print(f"Error initializing LLM: {e}")
            self.llm_initialized = False

    def initialize(self, use_gpu: bool = False):
        """Initialize the RAG engine components"""
        model_name = "all-MiniLM-L6-v2"
        self.vector_store.initialize_embeddings(model_name)

    def index_documents(self, documents: List[Document]):
        """Index documents into the vector store"""
        self.vector_store.add_documents(documents)

    def retrieve(self, query: str, top_k: int = 5) -> List[SearchResult]:
        """Retrieve relevant documents for a query"""
        return self.vector_store.search(query, top_k)

    def generate_response(self, query: str, context_docs: List[SearchResult],
                          mode: str = "default") -> str:
        """Generate response using LLM with retrieved context"""

        # Build context from retrieved documents
        context = self._build_context(context_docs)

        # Create prompt based on mode
        prompt = self._create_prompt(query, context, mode)

        if self.llm_initialized:
            try:
                response = self.model.generate_content(prompt)
                return response.text
            except Exception as e:
                return f"Error generating response: {e}\n\nContext found:\n{context}"
        else:
            # Return context-based response without LLM
            return self._generate_fallback_response(query, context_docs, context)

    def _build_context(self, results: List[SearchResult]) -> str:
        """Build context string from search results"""
        context_parts = []
        for result in results:
            source = result.document.source
            content = result.document.content[:1500]  # Limit content length
            context_parts.append(f"[Sumber: {source}]\n{content}")

        return "\n\n---\n\n".join(context_parts)

    def _create_prompt(self, query: str, context: str, mode: str) -> str:
        """Create prompt for LLM"""

        base_prompt = f"""Anda adalah asisten AI yang ahli dalam menganalisis informasi politik Indonesia.
Berdasarkan konteks berita politik berikut, jawab pertanyaan pengguna dengan akurat dan informatif.

KONTEKS BERITA:
{context}

PERTANYAAN: {query}

"""

        if mode == "analysis":
            base_prompt += """
Berikan analisis mendalam yang mencakup:
1. Ringkasan temuan utama
2. Aktor politik yang terlibat
3. Implikasi politik
4. Konteks yang relevan

ANALISIS:"""

        elif mode == "summary":
            base_prompt += """
Berikan ringkasan singkat dan padat (maksimal 3 paragraf) yang mencakup poin-poin kunci.

RINGKASAN:"""

        elif mode == "entities":
            base_prompt += """
Identifikasi dan daftar semua entitas politik yang disebutkan:
- Tokoh Politik
- Partai Politik
- Institusi Pemerintah
- Kebijakan/Regulasi

ENTITAS POLITIK:"""

        elif mode == "sentiment":
            base_prompt += """
Analisis sentimen berita ini terhadap subjek politik utama:
1. Identifikasi subjek utama
2. Tentukan sentimen (Positif/Netral/Negatif)
3. Berikan justifikasi

ANALISIS SENTIMEN:"""

        else:
            base_prompt += """
JAWABAN:"""

        return base_prompt

    def _generate_fallback_response(self, query: str, results: List[SearchResult],
                                    context: str) -> str:
        """Generate response without LLM (fallback mode)"""

        response = f"""**Hasil Pencarian untuk:** "{query}"

**Ditemukan {len(results)} dokumen relevan:**

"""
        for result in results:
            response += f"""
---
**Sumber:** {result.document.source}
**Relevansi:** {result.score:.2%}

{result.document.content[:500]}...

"""

        response += """
---
*Catatan: Untuk mendapatkan analisis AI yang lebih mendalam, silakan masukkan API key Google AI (Gemini).*
"""
        return response

    def analyze_political_landscape(self, documents: List[Document]) -> Dict[str, Any]:
        """Analyze overall political landscape from documents"""
        from document_processor import PoliticalEntityExtractor

        extractor = PoliticalEntityExtractor()

        all_parties = []
        all_keywords = []
        all_institutions = []
        relevance_scores = []

        for doc in documents:
            entities = extractor.extract_entities(doc.content)
            all_parties.extend(entities['parties'])
            all_keywords.extend(entities['keywords'])
            all_institutions.extend(entities['institutions'])
            relevance_scores.append(extractor.calculate_political_relevance(doc.content))

        # Count frequencies
        from collections import Counter

        party_counts = Counter(all_parties)
        keyword_counts = Counter(all_keywords)
        institution_counts = Counter(all_institutions)

        return {
            'total_documents': len(documents),
            'avg_relevance': np.mean(relevance_scores) if relevance_scores else 0,
            'top_parties': party_counts.most_common(10),
            'top_keywords': keyword_counts.most_common(15),
            'top_institutions': institution_counts.most_common(10),
            'relevance_distribution': relevance_scores
        }

    def get_topic_clusters(self, num_topics: int = 5) -> List[Dict[str, Any]]:
        """Get topic clusters from indexed documents"""
        if len(self.vector_store.documents) < num_topics:
            return []

        # Simple clustering based on embeddings
        if self.vector_store.embeddings is None:
            return []

        from collections import defaultdict

        # Use k-means-like clustering
        embeddings = self.vector_store.embeddings
        n_samples = len(embeddings)

        # Random initialization
        np.random.seed(42)
        centroids_idx = np.random.choice(n_samples, min(num_topics, n_samples), replace=False)
        centroids = embeddings[centroids_idx]

        # Assign documents to clusters
        clusters = defaultdict(list)
        for idx, emb in enumerate(embeddings):
            distances = np.linalg.norm(centroids - emb, axis=1)
            cluster_id = np.argmin(distances)
            clusters[cluster_id].append(self.vector_store.documents[idx])

        # Create topic summaries
        topic_results = []
        for cluster_id, docs in clusters.items():
            # Get common words in cluster
            all_text = " ".join([doc.content for doc in docs])
            words = all_text.lower().split()
            word_counts = Counter(words)

            # Filter stop words (basic Indonesian stop words)
            stop_words = {'yang', 'dan', 'di', 'ke', 'dari', 'ini', 'itu', 'untuk',
                         'dengan', 'pada', 'adalah', 'dalam', 'akan', 'tidak',
                         'juga', 'atau', 'ada', 'oleh', 'setelah', 'karena'}

            top_words = [w for w, c in word_counts.most_common(20)
                        if w not in stop_words and len(w) > 3][:5]

            topic_results.append({
                'id': cluster_id,
                'num_documents': len(docs),
                'top_words': top_words,
                'sample_content': docs[0].content[:200] if docs else ""
            })

        return sorted(topic_results, key=lambda x: x['num_documents'], reverse=True)


class QueryEngine:
    """High-level query interface for the RAG system"""

    def __init__(self, rag_engine: RAGEngine):
        self.rag_engine = rag_engine

    def ask(self, question: str, mode: str = "default") -> Dict[str, Any]:
        """Ask a question and get a response"""

        # Retrieve relevant documents
        results = self.rag_engine.retrieve(question, top_k=5)

        # Generate response
        response = self.rag_engine.generate_response(question, results, mode)

        return {
            'question': question,
            'response': response,
            'sources': [
                {
                    'source': r.document.source,
                    'score': r.score,
                    'preview': r.document.content[:200]
                }
                for r in results
            ],
            'mode': mode
        }

    def get_suggested_questions(self, documents: List[Document]) -> List[str]:
        """Generate suggested questions based on document content"""
        from document_processor import PoliticalEntityExtractor

        extractor = PoliticalEntityExtractor()

        all_parties = set()
        all_keywords = set()

        for doc in documents[:50]:  # Sample first 50 docs
            entities = extractor.extract_entities(doc.content)
            all_parties.update(entities['parties'])
            all_keywords.update(entities['keywords'])

        suggestions = [
            "Apa isu politik utama yang dibahas dalam berita-berita ini?",
            "Bagaimana sentimen media terhadap pemerintah?",
            "Siapa saja tokoh politik yang paling sering disebutkan?",
        ]

        # Add party-specific questions
        for party in list(all_parties)[:3]:
            suggestions.append(f"Apa peran {party} dalam pemberitaan ini?")

        # Add keyword-specific questions
        if 'pemilu' in all_keywords:
            suggestions.append("Bagaimana liputan media tentang pemilu?")
        if 'koalisi' in all_keywords:
            suggestions.append("Apa dinamika koalisi politik yang terlihat?")

        return suggestions[:8]
