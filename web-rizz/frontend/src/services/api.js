const API_URL = import.meta.env.VITE_API_URL;

export async function getStats() {
  try {
    const res = await fetch(`${API_URL}/stats`);
    return await res.json();
  } catch {
    return null;
  }
}

export async function connect() {
  const res = await fetch(`${API_URL}/connect`, { method: 'POST' });
  let data = null;
  try {
    data = await res.json();
  } catch (_) {
  }
  if (!res.ok) {
    const message = (data && data.error) ? data.error : `connection failed (${res.status})`;
    throw new Error(message);
  }
  return data;
}

export async function disconnect(sessionId) {
  await fetch(`${API_URL}/disconnect`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ sessionId })
  });
}

export async function executeCode(sessionId, code, filename) {
  try {
    const res = await fetch(`${API_URL}/execute`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ sessionId, code, filename })
    });

    const data = await res.json();
    if (data.error) return `error: ${data.error}`;
    return data.stdout || data.stderr || '';
  } catch (err) {
    return `error: ${err.message}`;
  }
}
