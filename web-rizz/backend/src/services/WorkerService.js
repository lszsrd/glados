import { spawn } from 'child_process';
import crypto from 'crypto';

export class WorkerService {
  constructor(config) {
    this.compilerPath = config.compilerPath;
    this.vmPath = config.vmPath;
    this.maxWorkers = config.maxWorkers;
    this.sessionTimeout = config.sessionTimeout || 300000;
    this.pool = [];
    this.sessions = new Map();
  }

  async init() {
    for (let i = 0; i < 4; i++) {
      const id = `w${i}`;
      await this.destroyContainer(id);
      await this.createContainer(id);
      this.pool.push({ id, sessionId: null });
    }
    console.log('workers ready');
  }

  createContainer(id) {
    return new Promise((resolve, reject) => {
      const docker = spawn('docker', [
        'run', '-d', '--name', `rizz-${id}`, '--rm',
        '--memory', '256m', '--cpus', '0.2',
        '--network', 'none', 'ubuntu:24.04', 'sleep', 'infinity'
      ]);

      let stderr = '';
      docker.stderr.on('data', d => stderr += d);

      docker.on('close', code => {
        if (code !== 0) {
          console.log(`[worker] container ${id} failed: ${stderr}`);
          return reject(new Error('container fail'));
        }

        const cpCompiler = spawn('docker', ['cp', this.compilerPath, `rizz-${id}:/usr/bin/glados-compiler`]);
        cpCompiler.on('close', () => {
          const cpVm = spawn('docker', ['cp', this.vmPath, `rizz-${id}:/usr/bin/glados-vm`]);
          cpVm.on('close', () => resolve());
        });
      });
    });
  }

  destroyContainer(id) {
    return new Promise(resolve => {
      spawn('docker', ['rm', '-f', `rizz-${id}`]).on('close', () => resolve());
    });
  }

  async connect() {
    const free = this.pool.find(w => !w.sessionId);

    if (!free) {
      if (this.pool.length >= this.maxWorkers) {
        throw new Error('no workers available');
      }
      const id = `t${Date.now()}`;
      await this.createContainer(id);
      this.pool.push({ id, sessionId: null });
      return this.connect();
    }

    const sessionId = crypto.randomBytes(16).toString('hex');
    free.sessionId = sessionId;

    const timeout = setTimeout(() => this.disconnect(sessionId), this.sessionTimeout);
    this.sessions.set(sessionId, {
      workerId: free.id,
      timeout,
      expiresAt: Date.now() + this.sessionTimeout
    });

    return { sessionId, expiresAt: Date.now() + this.sessionTimeout };
  }

  disconnect(sessionId) {
    const session = this.sessions.get(sessionId);
    if (!session) return;

    clearTimeout(session.timeout);
    this.sessions.delete(sessionId);

    const worker = this.pool.find(w => w.id === session.workerId);
    if (worker) worker.sessionId = null;
  }

  async execute(sessionId, code, filename = 'code.rz') {
    const session = this.sessions.get(sessionId);
    if (!session) {
      throw new Error('session expired');
    }

    clearTimeout(session.timeout);
    session.timeout = setTimeout(() => this.disconnect(sessionId), this.sessionTimeout);
    session.expiresAt = Date.now() + this.sessionTimeout;

    return this.runCode(session.workerId, code, filename);
  }

  runCode(id, code, filename) {
    const ext = filename.split('.').pop();
    const filePath = `/tmp/code.${ext}`;

    return new Promise(resolve => {
      const copyCode = spawn('docker', ['exec', '-i', `rizz-${id}`, 'sh', '-c', `cat > ${filePath}`]);
      copyCode.stdin.write(code);
      copyCode.stdin.end();

      copyCode.on('close', () => {
        const compile = spawn('docker', ['exec', `rizz-${id}`, '/usr/bin/glados-compiler', filePath]);
        let stderr = '';
        let timeout = false;

        const timer = setTimeout(() => {
          timeout = true;
          compile.kill();
        }, 16000);

        compile.stderr.on('data', d => stderr += d);

        compile.on('close', code => {
          if (timeout) {
            clearTimeout(timer);
            return resolve({ stdout: '', stderr: 'timeout 16s' });
          }

          if (code !== 0) {
            clearTimeout(timer);
            return resolve({ stdout: '', stderr });
          }

          const vm = spawn('docker', ['exec', `rizz-${id}`, '/usr/bin/glados-vm', `${filePath}.bc`]);
          let stdout = '';
          let vmStderr = '';

          vm.stdout.on('data', d => stdout += d);
          vm.stderr.on('data', d => vmStderr += d);

          vm.on('close', () => {
            clearTimeout(timer);
            resolve({ stdout, stderr: vmStderr });
          });
        });
      });
    });
  }

  getStats() {
    const used = this.pool.filter(w => w.sessionId).length;
    return {
      available: this.pool.length - used,
      total: this.pool.length,
      maxWorkers: this.maxWorkers
    };
  }

  async cleanup() {
    for (const session of this.sessions.values()) {
      clearTimeout(session.timeout);
    }
    for (const w of this.pool) {
      await this.destroyContainer(w.id);
    }
  }
}
