import Markdown from 'react-markdown';
import './Tutorial.css';

function Tutorial({ lesson, current, total, lessons, onPrev, onNext, onGoto }) {
  return (
    <div className="tutorial">
      <div className="tutorial-header">
        <button className="nav-btn" onClick={onPrev} disabled={current === 0} aria-label="Previous">
          <svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor" aria-hidden="true">
            <path d="M15.41 7.41L14 6l-6 6 6 6 1.41-1.41L10.83 12z"/>
          </svg>
        </button>
        <select className="lesson-select" value={current} onChange={(e) => onGoto(Number(e.target.value))}>
          {lessons.map((l, i) => (
            <option key={i} value={i}>{l.title}</option>
          ))}
        </select>
        <button className="nav-btn" onClick={onNext} disabled={current === total - 1} aria-label="Next">
          <svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor" aria-hidden="true">
            <path d="M8.59 16.59L10 18l6-6-6-6-1.41 1.41L13.17 12z"/>
          </svg>
        </button>
      </div>
      <div className="tutorial-content">
        <Markdown>{lesson.body}</Markdown>
      </div>
      <div className="tutorial-footer">
        <span className="progress">{current + 1} / {total}</span>
        <div className="footer-nav">
          <button onClick={onPrev} disabled={current === 0}>Previous</button>
          <button onClick={onNext} disabled={current === total - 1}>Next</button>
        </div>
      </div>
    </div>
  );
}

export default Tutorial;
