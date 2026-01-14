import './FileTree.css';

function FileTree({ filename }) {
  return (
    <div className="filetree">
      <div className="folder">
        <svg className="icon" viewBox="0 0 24 24" fill="#e8a838">
          <path d="M10 4H4c-1.1 0-2 .9-2 2v12c0 1.1.9 2 2 2h16c1.1 0 2-.9 2-2V8c0-1.1-.9-2-2-2h-8l-2-2z"/>
        </svg>
        <span>src</span>
      </div>
      <div className="file active">
        <svg className="icon" viewBox="0 0 24 24" fill="#f0db4f">
          <path d="M14 2H6c-1.1 0-2 .9-2 2v16c0 1.1.9 2 2 2h12c1.1 0 2-.9 2-2V8l-6-6zm-1 7V3.5L18.5 9H13z"/>
        </svg>
        <span>{filename}</span>
      </div>
    </div>
  );
}

export default FileTree;
