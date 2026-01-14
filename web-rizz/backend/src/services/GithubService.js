import fs from 'fs';
import path from 'path';
import { spawn } from 'child_process';

export class GithubService {
  constructor(config) {
    this.owner = config.owner;
    this.repo = config.repo;
    this.token = config.token;
    this.binDir = config.binDir;
    this.compilerPath = path.join(config.binDir, 'glados-compiler-linux');
    this.vmPath = path.join(config.binDir, 'glados-vm-linux');
  }

  async fetchLatestRelease() {
    const url = `https://api.github.com/repos/${this.owner}/${this.repo}/releases/latest`;
    const response = await fetch(url, {
      headers: {
        'Authorization': `Bearer ${this.token}`,
        'Accept': 'application/vnd.github.v3+json',
        'User-Agent': 'Rizz-Playground'
      }
    });

    if (!response.ok) {
      throw new Error(`gh api fail: ${response.status}`);
    }

    return await response.json();
  }

  async downloadAsset(asset) {
    const response = await fetch(asset.url, {
      headers: {
        'Authorization': `Bearer ${this.token}`,
        'Accept': 'application/octet-stream',
        'User-Agent': 'Rizz-Playground'
      }
    });

    if (!response.ok) {
      throw new Error(`dl failed: ${response.status}`);
    }

    return Buffer.from(await response.arrayBuffer());
  }

  findLinuxAsset(assets) {
    return assets.find(a =>
      a.name === 'glados' || a.name === 'glados.zip'
    );
  }

  async saveBinary(buffer, isZip = false) {
    if (!fs.existsSync(this.binDir)) {
      fs.mkdirSync(this.binDir, { recursive: true });
    }

    if (isZip) {
      const zipPath = path.join(this.binDir, 'glados.zip');
      fs.writeFileSync(zipPath, buffer);

      await new Promise((resolve, reject) => {
        const unzip = spawn('unzip', ['-o', zipPath, '-d', this.binDir]);
        unzip.on('close', code => {
          fs.unlinkSync(zipPath);
          code === 0 ? resolve() : reject(new Error('unzip fail'));
        });
      });

      fs.chmodSync(this.compilerPath, 0o755);
      fs.chmodSync(this.vmPath, 0o755);
    }
  }

  isUpToDate(assetDate) {
    if (!this.binaryExists()) {
      return false;
    }

    const stats = fs.statSync(this.compilerPath);
    return stats.mtime >= new Date(assetDate);
  }

  binaryExists() {
    return fs.existsSync(this.compilerPath) && fs.existsSync(this.vmPath);
  }

  async update() {
    const release = await this.fetchLatestRelease();

    const asset = this.findLinuxAsset(release.assets);

    if (!asset) {
      throw new Error('no linux bin found in relase');
    }

    if (this.isUpToDate(asset.updated_at)) {
      return { updated: false, version: release.tag_name };
    }

    const buffer = await this.downloadAsset(asset);
    await this.saveBinary(buffer, asset.name.endsWith('.zip'));

    return { updated: true, version: release.tag_name };
  }
}

