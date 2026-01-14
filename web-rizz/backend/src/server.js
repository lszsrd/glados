import express from 'express';
import cors from 'cors';
import dotenv from 'dotenv';
import path from 'path';
import { fileURLToPath } from 'url';
import { GithubService } from './services/GithubService.js';
import { WorkerService } from './services/WorkerService.js';

dotenv.config();

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const app = express();

app.use(cors());
app.use(express.json());

const binDir = path.join(__dirname, '../bin');

const github = new GithubService({
  owner: process.env.GITHUB_OWNER,
  repo: process.env.GITHUB_REPO,
  token: process.env.GITHUB_TOKEN,
  binDir
});

const workers = new WorkerService({
  compilerPath: path.join(binDir, 'glados-compiler-linux'),
  vmPath: path.join(binDir, 'glados-vm-linux'),
  maxWorkers: 10,
  sessionTimeout: 2 * 60 * 1000
});

app.get('/stats', (req, res) => {
  res.json(workers.getStats());
});

app.post('/connect', async (req, res) => {
  try {
    const session = await workers.connect();
    res.json(session);
  } catch (err) {
    res.status(503).json({ error: err.message });
  }
});

app.post('/disconnect', (req, res) => {
  const { sessionId } = req.body;
  if (!sessionId) {
    return res.status(400).json({ error: 'sessionId required' });
  }
  workers.disconnect(sessionId);
  res.json({ ok: true });
});

app.post('/execute', async (req, res) => {
  const { sessionId, code, filename } = req.body;

  if (!github.binaryExists()) {
    return res.json({ stderr: 'glados binary not found' });
  }

  if (!sessionId) {
    return res.status(400).json({ error: 'sessionId required' });
  }

  try {
    const result = await workers.execute(sessionId, code, filename);
    res.json(result);
  } catch (err) {
    res.status(400).json({ error: err.message });
  }
});

async function start() {
  try {
    await github.update();
  } catch (err) {
    if (!github.binaryExists()) {
      console.log('no binary found');
    }
  }

  if (github.binaryExists()) {
    await workers.init();
  }

  setInterval(() => github.update().catch(() => { }), 3600000);

  app.listen(3001, () => console.log('workinggg on 3001'));
}

process.on('SIGINT', async () => {
  await workers.cleanup();
  process.exit(0);
});

start();
