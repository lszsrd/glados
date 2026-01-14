import { defineConfig, loadEnv } from 'vite'
import react from '@vitejs/plugin-react'

// https://vite.dev/config/
export default defineConfig(({ mode }) => {
  const env = loadEnv(mode, process.cwd(), '')
  const proxyTarget = env.VITE_DEV_API_PROXY_TARGET
  return {
    plugins: [react()],
    server: proxyTarget ? {
      proxy: {
        '/api': {
          target: proxyTarget,
          changeOrigin: true,
          rewrite: p => p.replace(/^\/api/, ''),
        }
      }
    } : undefined,
  }
})
