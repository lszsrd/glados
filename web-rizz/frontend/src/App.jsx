import { useState, useRef, useEffect } from 'react';
import Split from 'react-split';
import Navbar from './components/Navbar/Navbar';
import Tutorial from './components/Tutorial/Tutorial';
import FileTree from './components/FileTree/FileTree';
import Editor from './components/Editor/Editor';
import Terminal from './components/Terminal/Terminal';
import { tuto } from './tuto/index.js';
import { executeCode, connect, getStats } from './services/api';
import './App.css';

function App() {
  const [current, setCurrent] = useState(0);
  const [code, setCode] = useState(tuto[0].code);
  const [output, setOutput] = useState('');
  const [workers, setWorkers] = useState(null);
  const [session, setSession] = useState(null);
  const [timeLeft, setTimeLeft] = useState(0);
  const [mobileView, setMobileView] = useState('tutorial');
  const [isMobile, setIsMobile] = useState(false);

  const timerRef = useRef(null);
  const countdownRef = useRef(null);

  useEffect(() => {
    fetchStats();
    const interval = setInterval(fetchStats, 10000);

    const checkMobile = () => setIsMobile(window.innerWidth < 768);
    checkMobile();
    window.addEventListener('resize', checkMobile);

    return () => {
      clearInterval(interval);
      window.removeEventListener('resize', checkMobile);
    };
  }, []);

  useEffect(() => {
    if (session?.expiresAt) {
      countdownRef.current = setInterval(() => {
        const remaining = Math.max(0, session.expiresAt - Date.now());
        setTimeLeft(remaining);
        if (remaining === 0) {
          clearInterval(countdownRef.current);
          setSession(null);
          setOutput('Session expired. Click "Run" to reconnect.');
        }
      }, 1000);
      return () => clearInterval(countdownRef.current);
    }
  }, [session?.expiresAt]);

  const fetchStats = async () => {
    const stats = await getStats();
    if (stats) setWorkers(stats);
  };

  const handleCodeChange = (newCode) => {
    setCode(newCode);
  };

  const handleRun = async () => {
    setOutput('');
    const filename = tuto[current].filename;
    if (!session) {
      try {
        const newSession = await connect();
        setSession(newSession);
        fetchStats();
        const result = await executeCode(newSession.sessionId, code, filename);
        setOutput(result);
        setSession(s => ({ ...s, expiresAt: Date.now() + 2 * 60 * 1000 }));
      } catch (err) {
        setOutput(`error: ${err.message}`);
      }
    } else {
      runCode(code, filename);
    }
  };

  const runCode = async (codeToRun, filename) => {
    if (!session) return;
    setOutput('');
    const result = await executeCode(session.sessionId, codeToRun, filename || tuto[current].filename);
    setOutput(result);
    setSession(s => ({ ...s, expiresAt: Date.now() + 5 * 60 * 1000 }));
    fetchStats();
  };

  const handleLessonChange = (index) => {
    setCurrent(index);
    setCode(tuto[index].code);
    setOutput('');
  };

  const handlePrev = () => {
    if (current > 0) handleLessonChange(current - 1);
  };

  const handleNext = () => {
    if (current < tuto.length - 1) handleLessonChange(current + 1);
  };

  const formatTime = (ms) => {
    const s = Math.floor(ms / 1000);
    const m = Math.floor(s / 60);
    const sec = s % 60;
    return `${m}:${sec.toString().padStart(2, '0')}`;
  };

  const tutorialPanel = (
    <Tutorial
      lesson={tuto[current]}
      lessons={tuto}
      current={current}
      total={tuto.length}
      onPrev={handlePrev}
      onNext={handleNext}
      onGoto={handleLessonChange}
    />
  );

  const runBar = (
    <div className="run-bar">
      <div className="run-info">
        {session ? (
          <span className="session-timer">{formatTime(timeLeft)}</span>
        ) : (
          <span className="session-hint">Click Run to start</span>
        )}
      </div>
      <button className="run-btn" onClick={handleRun} aria-label="Run code">
        <svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor" aria-hidden="true">
          <path d="M8 5v14l11-7z" />
        </svg>
        Run
      </button>
    </div>
  );

  const workspacePanel = (
    <div className="workspace-container">
      <Split className="workspace" direction="vertical" sizes={[60, 40]} minSize={100} gutterSize={4}>
        <div className="editor-panel">
          <FileTree filename={tuto[current].filename} />
          <Editor code={code} onChange={handleCodeChange} onRun={handleRun} />
        </div>
        <Terminal output={output} />
      </Split>
      {runBar}
    </div>
  );

  if (isMobile) {
    return (
      <div className="app">
        <Navbar workers={workers} />
        <main className="main mobile" role="main">
          <div className={`panel ${mobileView === 'tutorial' ? 'active' : ''}`}>
            {tutorialPanel}
          </div>
          <div className={`panel ${mobileView === 'code' ? 'active' : ''}`}>
            {workspacePanel}
          </div>
        </main>
        <div className="mobile-toggle" role="tablist">
          <button
            role="tab"
            aria-selected={mobileView === 'tutorial'}
            className={mobileView === 'tutorial' ? 'active' : ''}
            onClick={() => setMobileView('tutorial')}
          >
            Tutorial
          </button>
          <button
            role="tab"
            aria-selected={mobileView === 'code'}
            className={mobileView === 'code' ? 'active' : ''}
            onClick={() => setMobileView('code')}
          >
            Code
          </button>
        </div>
      </div>
    );
  }

  return (
    <div className="app">
      <Navbar workers={workers} />
      <Split className="main" sizes={[30, 70]} minSize={200} gutterSize={4} role="main">
        {tutorialPanel}
        {workspacePanel}
      </Split>
    </div>
  );
}

export default App;
