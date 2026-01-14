import { useState, useRef, useEffect } from 'react';
import Split from 'react-split';
import Navbar from './components/Navbar/Navbar';
import Tutorial from './components/Tutorial/Tutorial';
import FileTree from './components/FileTree/FileTree';
import Editor from './components/Editor/Editor';
import Terminal from './components/Terminal/Terminal';
import { tuto } from './tuto/index.js';
import { executeCode, connect, getStats } from './services/api';
import { formatTime, isMobileWidth } from './utils';
import './App.css';

const SESSION_TIMEOUT = 2 * 60 * 1000;

function App() {
  const [current, setCurrent] = useState(0);
  const [code, setCode] = useState(tuto[0].code);
  const [output, setOutput] = useState('');
  const [workers, setWorkers] = useState(null);
  const [session, setSession] = useState(null);
  const [timeLeft, setTimeLeft] = useState(0);
  const [mobileView, setMobileView] = useState('tutorial');
  const [isMobile, setIsMobile] = useState(false);
  const countdownRef = useRef(null);

  useEffect(() => {
    getStats().then(s => s && setWorkers(s));
    const interval = setInterval(() => getStats().then(s => s && setWorkers(s)), 10000);
    const onResize = () => setIsMobile(isMobileWidth());
    onResize();
    window.addEventListener('resize', onResize);
    return () => { clearInterval(interval); window.removeEventListener('resize', onResize); };
  }, []);

  useEffect(() => {
    if (!session?.expiresAt) return;
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
  }, [session?.expiresAt]);

  const handleRun = async () => {
    setOutput('');
    const filename = tuto[current].filename;
    let sid = session?.sessionId;

    if (!sid) {
      try {
        const s = await connect();
        setSession(s);
        sid = s.sessionId;
        getStats().then(s => s && setWorkers(s));
      } catch (err) {
        return setOutput(`error: ${err.message}`);
      }
    }

    const result = await executeCode(sid, code, filename);
    setOutput(result);
    setSession(s => ({ ...s, expiresAt: Date.now() + SESSION_TIMEOUT }));
    getStats().then(s => s && setWorkers(s));
  };

  const goToLesson = (index) => {
    setCurrent(index);
    setCode(tuto[index].code);
    setOutput('');
  };

  const tutorialPanel = (
    <Tutorial
      lesson={tuto[current]}
      lessons={tuto}
      current={current}
      total={tuto.length}
      onPrev={() => current > 0 && goToLesson(current - 1)}
      onNext={() => current < tuto.length - 1 && goToLesson(current + 1)}
      onGoto={goToLesson}
    />
  );

  const workspacePanel = (
    <div className="workspace-container">
      <Split className="workspace" direction="vertical" sizes={[60, 40]} minSize={100} gutterSize={4}>
        <div className="editor-panel">
          <FileTree filename={tuto[current].filename} />
          <Editor code={code} onChange={setCode} onRun={handleRun} />
        </div>
        <Terminal output={output} />
      </Split>
      <div className="run-bar">
        <div className="run-info">
          {session ? <span className="session-timer">{formatTime(timeLeft)}</span> : <span className="session-hint">Click Run to start</span>}
        </div>
        <button className="run-btn" onClick={handleRun} aria-label="Run code">
          <svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor" aria-hidden="true"><path d="M8 5v14l11-7z" /></svg>
          Run
        </button>
      </div>
    </div>
  );

  return (
    <div className="app">
      <Navbar workers={workers} />
      {isMobile ? (
        <>
          <main className="main mobile" role="main">
            <div className={`panel ${mobileView === 'tutorial' ? 'active' : ''}`}>{tutorialPanel}</div>
            <div className={`panel ${mobileView === 'code' ? 'active' : ''}`}>{workspacePanel}</div>
          </main>
          <div className="mobile-toggle" role="tablist">
            <button role="tab" aria-selected={mobileView === 'tutorial'} className={mobileView === 'tutorial' ? 'active' : ''} onClick={() => setMobileView('tutorial')}>Tutorial</button>
            <button role="tab" aria-selected={mobileView === 'code'} className={mobileView === 'code' ? 'active' : ''} onClick={() => setMobileView('code')}>Code</button>
          </div>
        </>
      ) : (
        <Split className="main" sizes={[30, 70]} minSize={200} gutterSize={4} role="main">
          {tutorialPanel}
          {workspacePanel}
        </Split>
      )}
    </div>
  );
}

export default App;
