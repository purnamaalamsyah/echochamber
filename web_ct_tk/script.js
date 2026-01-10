const appState = {
    currentView: 'home', // home, lesson, quiz, result
    lessonIndex: 0,
    quizIndex: 0,
    score: 0
};

// Data for Lessons
const lessons = [
    {
        title: "Belajar Urutan",
        content: `
            <div class="sequence-container">
                <div class="step">
                    <div class="emoji">🌱</div>
                    <p>1. Biji</p>
                </div>
                <div class="arrow">➡️</div>
                <div class="step">
                    <div class="emoji">🌿</div>
                    <p>2. Tunas</p>
                </div>
                <div class="arrow">➡️</div>
                <div class="step">
                    <div class="emoji">🌳</div>
                    <p>3. Pohon</p>
                </div>
            </div>
        `,
        narration: "Ayo belajar urutan. Pertama biji, lalu menjadi tunas, dan akhirnya menjadi pohon besar. Biji, Tunas, Pohon."
    },
    {
        title: "Belajar Pola",
        content: `
            <div class="pattern-container">
                <div class="shape red">🔴</div>
                <div class="shape blue">🔵</div>
                <div class="shape red">🔴</div>
                <div class="shape unknown">❓</div>
            </div>
        `,
        narration: "Ayo belajar pola. Merah, Biru, Merah. Apa selanjutnya? Ya, selanjutnya harusnya Biru."
    }
];

// Data for Quiz
const quizzes = [
    {
        question: "Yang mana urutan pertama?",
        content: `
            <div class="sequence-container">
                <div class="step">
                    <div class="emoji">❓</div>
                </div>
                <div class="arrow">➡️</div>
                 <div class="step">
                    <div class="emoji">🌿</div>
                </div>
                <div class="arrow">➡️</div>
                <div class="step">
                    <div class="emoji">🌳</div>
                </div>
            </div>
        `,
        options: [
            { text: "🌱", isCorrect: true },
            { text: "🍎", isCorrect: false }
        ],
        narration: "Yang mana urutan pertama? Biji atau Apel?"
    },
    {
        question: "Apa selanjutnya?",
        content: `
             <div class="pattern-container">
                <div class="shape red">🔴</div>
                <div class="shape blue">🔵</div>
                <div class="shape red">🔴</div>
                <div class="shape unknown">❓</div>
            </div>
        `,
        options: [
            { text: "🔵", isCorrect: true },
            { text: "🔴", isCorrect: false }
        ],
        narration: "Merah, Biru, Merah. Apa selanjutnya? Biru atau Merah?"
    }
];


// Text to Speech Function
function speak(text) {
    if ('speechSynthesis' in window) {
        window.speechSynthesis.cancel();
        const utterance = new SpeechSynthesisUtterance(text);
        utterance.lang = 'id-ID';
        utterance.rate = 0.9;
        utterance.pitch = 1.1;
        window.speechSynthesis.speak(utterance);
    } else {
        console.warn("Browser tidak mendukung Web Speech API");
    }
}

// Navigation Helper
function switchView(viewId) {
    document.querySelectorAll('.view').forEach(el => el.classList.remove('active'));
    document.getElementById(viewId).classList.add('active');
    appState.currentView = viewId;
}

function loadLesson(index) {
    if (index >= lessons.length) {
        // Go to Quiz start
        startQuiz();
        return;
    }

    const lesson = lessons[index];
    document.getElementById('lesson-title').innerText = lesson.title;
    document.getElementById('lesson-content').innerHTML = lesson.content;

    switchView('lesson-view');
    speak(lesson.narration);
}

function startQuiz() {
    appState.quizIndex = 0;
    appState.score = 0;
    speak("Sekarang waktunya kuis! Ayo jawab pertanyaan.");
    setTimeout(() => {
        loadQuiz(0);
    }, 3000);
}

function loadQuiz(index) {
    if (index >= quizzes.length) {
        showResult();
        return;
    }

    const quiz = quizzes[index];
    document.getElementById('quiz-question').innerText = quiz.question;
    document.getElementById('quiz-content').innerHTML = quiz.content;

    const optionsContainer = document.getElementById('quiz-options');
    optionsContainer.innerHTML = '';
    document.getElementById('feedback').innerText = '';

    quiz.options.forEach(opt => {
        const btn = document.createElement('button');
        btn.className = 'option-btn';
        btn.innerText = opt.text;
        btn.onclick = () => checkAnswer(opt.isCorrect);
        optionsContainer.appendChild(btn);
    });

    switchView('quiz-view');
    speak(quiz.narration);
}

function checkAnswer(isCorrect) {
    const feedback = document.getElementById('feedback');
    if (isCorrect) {
        feedback.innerText = "✅ Benar! Hore!";
        feedback.style.color = "green";
        speak("Benar! Hore!");
        appState.score++;
        setTimeout(() => {
            appState.quizIndex++;
            loadQuiz(appState.quizIndex);
        }, 2000);
    } else {
        feedback.innerText = "❌ Coba lagi ya.";
        feedback.style.color = "red";
        speak("Coba lagi ya.");
    }
}

function showResult() {
    document.getElementById('final-score').innerText = `Skor kamu: ${appState.score} dari ${quizzes.length}`;
    switchView('result-view');
    if (appState.score === quizzes.length) {
        speak("Wah, kamu hebat! Semua jawaban benar.");
    } else {
        speak("Bagus! Belajar lagi ya biar makin pintar.");
    }
}

// Initialization
document.addEventListener('DOMContentLoaded', () => {
    const startBtn = document.getElementById('start-btn');
    const nextLessonBtn = document.getElementById('next-lesson-btn');
    const restartBtn = document.getElementById('restart-btn');

    if (startBtn) {
        startBtn.addEventListener('click', () => {
            appState.lessonIndex = 0;
            speak("Halo! Ayo kita belajar urutan dan pola.");
            setTimeout(() => {
                 loadLesson(0);
            }, 3000);
        });
    }

    if (nextLessonBtn) {
        nextLessonBtn.addEventListener('click', () => {
            appState.lessonIndex++;
            loadLesson(appState.lessonIndex);
        });
    }

    if (restartBtn) {
        restartBtn.addEventListener('click', () => {
            switchView('home-view');
        });
    }
});
