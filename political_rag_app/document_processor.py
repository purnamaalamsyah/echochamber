"""
Document Processor Module for Political RAG Application
Handles loading and processing of various document formats (Excel, CSV, TXT)
"""

import pandas as pd
import os
from typing import List, Dict, Any, Optional
from dataclasses import dataclass
import re
from datetime import datetime


@dataclass
class Document:
    """Represents a processed document"""
    content: str
    metadata: Dict[str, Any]
    source: str
    doc_id: str


class DocumentProcessor:
    """Processes various document formats for the RAG system"""

    def __init__(self):
        self.supported_formats = ['.csv', '.xlsx', '.xls', '.txt']
        self.documents: List[Document] = []

    def process_uploaded_files(self, uploaded_files: List[Any]) -> List[Document]:
        """Process multiple uploaded files"""
        all_documents = []

        for uploaded_file in uploaded_files:
            file_extension = os.path.splitext(uploaded_file.name)[1].lower()

            if file_extension in ['.csv']:
                docs = self._process_csv(uploaded_file)
            elif file_extension in ['.xlsx', '.xls']:
                docs = self._process_excel(uploaded_file)
            elif file_extension in ['.txt']:
                docs = self._process_txt(uploaded_file)
            else:
                continue

            all_documents.extend(docs)

        self.documents = all_documents
        return all_documents

    def _process_csv(self, file) -> List[Document]:
        """Process CSV file"""
        documents = []
        try:
            df = pd.read_csv(file)
            documents = self._dataframe_to_documents(df, file.name)
        except Exception as e:
            print(f"Error processing CSV {file.name}: {e}")
        return documents

    def _process_excel(self, file) -> List[Document]:
        """Process Excel file"""
        documents = []
        try:
            df = pd.read_excel(file)
            documents = self._dataframe_to_documents(df, file.name)
        except Exception as e:
            print(f"Error processing Excel {file.name}: {e}")
        return documents

    def _process_txt(self, file) -> List[Document]:
        """Process TXT file - splits by paragraphs or sections"""
        documents = []
        try:
            content = file.read().decode('utf-8')

            # Split by double newlines (paragraphs) or section markers
            sections = re.split(r'\n\s*\n|\n---\n|\n===\n', content)

            for idx, section in enumerate(sections):
                section = section.strip()
                if len(section) > 50:  # Only include substantial sections
                    doc = Document(
                        content=section,
                        metadata={
                            'source_file': file.name,
                            'section_index': idx,
                            'char_count': len(section),
                            'word_count': len(section.split())
                        },
                        source=file.name,
                        doc_id=f"{file.name}_{idx}"
                    )
                    documents.append(doc)

        except Exception as e:
            print(f"Error processing TXT {file.name}: {e}")
        return documents

    def _dataframe_to_documents(self, df: pd.DataFrame, source_name: str) -> List[Document]:
        """Convert DataFrame to list of Documents"""
        documents = []

        # Try to identify content columns
        content_columns = self._identify_content_columns(df)

        for idx, row in df.iterrows():
            # Combine relevant text columns
            content_parts = []
            metadata = {'source_file': source_name, 'row_index': idx}

            for col in df.columns:
                value = row[col]
                if pd.notna(value):
                    if col.lower() in content_columns:
                        content_parts.append(f"{col}: {value}")
                    else:
                        # Add as metadata
                        metadata[col] = str(value)

            if content_parts:
                content = "\n".join(content_parts)
                doc = Document(
                    content=content,
                    metadata=metadata,
                    source=source_name,
                    doc_id=f"{source_name}_{idx}"
                )
                documents.append(doc)

        return documents

    def _identify_content_columns(self, df: pd.DataFrame) -> List[str]:
        """Identify columns that likely contain main text content"""
        content_keywords = [
            'content', 'text', 'body', 'article', 'berita', 'isi',
            'konten', 'paragraf', 'description', 'deskripsi', 'title',
            'judul', 'headline', 'summary', 'ringkasan', 'abstrak'
        ]

        content_columns = []
        for col in df.columns:
            col_lower = col.lower()
            # Check if column name contains content keywords
            if any(keyword in col_lower for keyword in content_keywords):
                content_columns.append(col_lower)
            # Check if column has long text values (likely content)
            elif df[col].dtype == 'object':
                avg_length = df[col].astype(str).str.len().mean()
                if avg_length > 100:  # Likely a content column
                    content_columns.append(col_lower)

        # If no content columns found, use all object columns
        if not content_columns:
            content_columns = [col.lower() for col in df.columns if df[col].dtype == 'object']

        return content_columns


class TextChunker:
    """Splits documents into smaller chunks for better retrieval"""

    def __init__(self, chunk_size: int = 1000, chunk_overlap: int = 200):
        self.chunk_size = chunk_size
        self.chunk_overlap = chunk_overlap

    def chunk_documents(self, documents: List[Document]) -> List[Document]:
        """Split documents into smaller chunks"""
        chunked_docs = []

        for doc in documents:
            chunks = self._split_text(doc.content)

            for chunk_idx, chunk in enumerate(chunks):
                chunked_doc = Document(
                    content=chunk,
                    metadata={
                        **doc.metadata,
                        'chunk_index': chunk_idx,
                        'total_chunks': len(chunks)
                    },
                    source=doc.source,
                    doc_id=f"{doc.doc_id}_chunk{chunk_idx}"
                )
                chunked_docs.append(chunked_doc)

        return chunked_docs

    def _split_text(self, text: str) -> List[str]:
        """Split text into overlapping chunks"""
        chunks = []

        # Split by sentences first
        sentences = re.split(r'(?<=[.!?])\s+', text)

        current_chunk = ""
        for sentence in sentences:
            if len(current_chunk) + len(sentence) <= self.chunk_size:
                current_chunk += " " + sentence if current_chunk else sentence
            else:
                if current_chunk:
                    chunks.append(current_chunk.strip())
                current_chunk = sentence

        if current_chunk:
            chunks.append(current_chunk.strip())

        return chunks if chunks else [text]


class PoliticalEntityExtractor:
    """Extracts political entities from Indonesian text"""

    def __init__(self):
        # Indonesian political parties
        self.political_parties = [
            'PDI-P', 'PDIP', 'Partai Demokrasi Indonesia Perjuangan',
            'Golkar', 'Partai Golongan Karya',
            'Gerindra', 'Partai Gerakan Indonesia Raya',
            'Nasdem', 'NasDem', 'Partai Nasional Demokrat',
            'PKB', 'Partai Kebangkitan Bangsa',
            'Demokrat', 'Partai Demokrat',
            'PKS', 'Partai Keadilan Sejahtera',
            'PAN', 'Partai Amanat Nasional',
            'PPP', 'Partai Persatuan Pembangunan',
            'Perindo', 'Partai Persatuan Indonesia',
            'PSI', 'Partai Solidaritas Indonesia',
            'Hanura', 'Partai Hati Nurani Rakyat'
        ]

        # Political keywords
        self.political_keywords = [
            'pemilu', 'pilpres', 'pilkada', 'politik', 'partai',
            'presiden', 'menteri', 'DPR', 'DPRD', 'MPR',
            'gubernur', 'bupati', 'walikota', 'caleg', 'capres',
            'cawapres', 'koalisi', 'oposisi', 'kampanye', 'suara',
            'demokrasi', 'reformasi', 'kabinet', 'pemerintah',
            'legislatif', 'eksekutif', 'yudikatif', 'konstitusi',
            'undang-undang', 'peraturan', 'kebijakan', 'regulasi'
        ]

        # Government institutions
        self.institutions = [
            'KPU', 'Bawaslu', 'MK', 'MA', 'KPK', 'BPK',
            'Kemendagri', 'Kemlu', 'Kemhan', 'Kemenkes',
            'Kemendikbud', 'Kemenag', 'Kemensos', 'BUMN'
        ]

    def extract_entities(self, text: str) -> Dict[str, List[str]]:
        """Extract political entities from text"""
        entities = {
            'parties': [],
            'keywords': [],
            'institutions': []
        }

        text_lower = text.lower()

        # Find political parties
        for party in self.political_parties:
            if party.lower() in text_lower:
                if party not in entities['parties']:
                    entities['parties'].append(party)

        # Find political keywords
        for keyword in self.political_keywords:
            if keyword.lower() in text_lower:
                if keyword not in entities['keywords']:
                    entities['keywords'].append(keyword)

        # Find institutions
        for inst in self.institutions:
            if inst.lower() in text_lower:
                if inst not in entities['institutions']:
                    entities['institutions'].append(inst)

        return entities

    def calculate_political_relevance(self, text: str) -> float:
        """Calculate political relevance score (0-1)"""
        entities = self.extract_entities(text)

        total_entities = (
            len(entities['parties']) * 3 +  # Parties weighted more
            len(entities['keywords']) * 1 +
            len(entities['institutions']) * 2
        )

        # Normalize score (max around 20 entities)
        score = min(total_entities / 20, 1.0)
        return round(score, 2)
