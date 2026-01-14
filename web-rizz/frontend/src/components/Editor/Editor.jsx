import { useEffect, useRef, useState } from 'react';
import { EditorView, basicSetup } from 'codemirror';
import { keymap } from '@codemirror/view';
import { javascript } from '@codemirror/lang-javascript';
import { oneDark } from '@codemirror/theme-one-dark';
import { vim } from '@replit/codemirror-vim';
import './Editor.css';

function Editor({ code, onChange, onRun }) {
  const editorRef = useRef(null);
  const viewRef = useRef(null);
  const [vimMode, setVimMode] = useState(true);
  const onRunRef = useRef(onRun);

  useEffect(() => {
    onRunRef.current = onRun;
  }, [onRun]);

  useEffect(() => {
    if (!editorRef.current) return;
    if (viewRef.current) viewRef.current.destroy();

    const extensions = [
      basicSetup,
      javascript(),
      oneDark,
      EditorView.updateListener.of(update => {
        if (update.docChanged) {
          onChange(update.state.doc.toString());
        }
      }),
      keymap.of([{
        key: 'Ctrl-Enter',
        run: () => {
          onRunRef.current?.();
          return true;
        }
      }])
    ];

    if (vimMode) {
      extensions.push(vim());
    }

    const view = new EditorView({
      doc: code,
      extensions,
      parent: editorRef.current
    });

    viewRef.current = view;

    return () => view.destroy();
  }, [vimMode]);

  useEffect(() => {
    if (viewRef.current && viewRef.current.state.doc.toString() !== code) {
      viewRef.current.dispatch({
        changes: { from: 0, to: viewRef.current.state.doc.length, insert: code }
      });
    }
  }, [code]);

  return (
    <div className="editor-container">
      <div className="editor-toolbar">
        <button
          className={`vim-toggle ${vimMode ? 'active' : ''}`}
          onClick={() => setVimMode(!vimMode)}
        >
          VIM
        </button>
      </div>
      <div className="editor" ref={editorRef}></div>
    </div>
  );
}

export default Editor;
