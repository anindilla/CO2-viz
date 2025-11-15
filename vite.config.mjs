import { defineConfig } from 'vite';
import { mkdtemp, readFile, rm, stat } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { spawn } from 'node:child_process';

const elmExecutable = process.platform === 'win32' ? 'elm.cmd' : 'elm';
const elmBin = path.join(process.cwd(), 'node_modules', '.bin', elmExecutable);

const GLOBAL_PATCH_NEEDLE = '}(this));';
const GLOBAL_PATCH_REPLACEMENT = '}(typeof globalThis !== "undefined" ? globalThis : this));';

const elmPlugin = () => {
  const cache = new Map();

  return {
    name: 'elm-inline-compiler',
    enforce: 'pre',
    async load(id) {
      if (!id.endsWith('.elm')) return null;

      const cleanId = id.split('?')[0];
      const fileStat = await stat(cleanId);
      const cacheKey = `${cleanId}:${fileStat.mtimeMs}`;

      if (cache.has(cacheKey)) {
        return cache.get(cacheKey);
      }

      const tmpDir = await mkdtemp(path.join(tmpdir(), 'elm-vite-'));
      const outFile = path.join(tmpDir, 'elm.js');

      try {
        await runElm(cleanId, outFile, process.env.NODE_ENV === 'production');
        const js = await readFile(outFile, 'utf8');
        const patched = js.includes(GLOBAL_PATCH_NEEDLE)
          ? js.replace(GLOBAL_PATCH_NEEDLE, GLOBAL_PATCH_REPLACEMENT)
          : js;
        const wrapped = `${patched}\nexport const Elm = globalThis.Elm;`;
        cache.set(cacheKey, wrapped);
        return wrapped;
      } finally {
        await rm(tmpDir, { recursive: true, force: true });
      }
    }
  };
};

function runElm(input, output, optimize) {
  return new Promise((resolve, reject) => {
    const args = [ 'make', input, '--output', output ];
    if (optimize) {
      args.push('--optimize');
    }

    const child = spawn(elmBin, args, { stdio: 'inherit' });
    child.on('close', (code) => {
      if (code === 0) resolve();
      else reject(new Error('Elm compilation failed'));
    });
  });
}

export default defineConfig({
  plugins: [elmPlugin()],
  server: {
    host: '127.0.0.1',
    port: 5173
  }
});

