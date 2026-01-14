import { useEffect, useRef } from 'react';
import { Terminal as XTerm } from '@xterm/xterm';
import { FitAddon } from '@xterm/addon-fit';
import '@xterm/xterm/css/xterm.css';
import './Terminal.css';

function Terminal({ output }) {
  const containerRef = useRef(null);
  const terminalRef = useRef(null);
  const fitAddonRef = useRef(null);

  useEffect(() => {
    if (!containerRef.current) return;

    const terminal = new XTerm({
      theme: {
        background: '#1e1e1e',
        foreground: '#d4d4d4',
        cursor: '#d4d4d4',
        cursorAccent: '#1e1e1e',
        black: '#1e1e1e',
        red: '#f44747',
        green: '#6a9955',
        yellow: '#f0db4f',
        blue: '#569cd6',
        magenta: '#c586c0',
        cyan: '#4ec9b0',
        white: '#d4d4d4',
        brightBlack: '#808080',
        brightRed: '#f44747',
        brightGreen: '#6a9955',
        brightYellow: '#f0db4f',
        brightBlue: '#569cd6',
        brightMagenta: '#c586c0',
        brightCyan: '#4ec9b0',
        brightWhite: '#ffffff'
      },
      fontFamily: 'monospace',
      fontSize: 14,
      cursorBlink: true,
      disableStdin: true,
      convertEol: true
    });

    const fitAddon = new FitAddon();
    terminal.loadAddon(fitAddon);
    terminal.open(containerRef.current);
    fitAddon.fit();

    terminalRef.current = terminal;
    fitAddonRef.current = fitAddon;

    const resizeObserver = new ResizeObserver(() => {
      fitAddon.fit();
    });
    resizeObserver.observe(containerRef.current);

    return () => {
      resizeObserver.disconnect();
      terminal.dispose();
    };
  }, []);

  useEffect(() => {
    if (terminalRef.current) {
      terminalRef.current.reset();
      if (output) {
        terminalRef.current.write(output);
      }
    }
  }, [output]);

  return (
    <div className="terminal">
      <div className="terminal-header">
        <span className="terminal-title">Output</span>
      </div>
      <div className="terminal-content" ref={containerRef}></div>
    </div>
  );
}

export default Terminal;
