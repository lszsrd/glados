import { Buffer } from 'buffer';
window.Buffer = Buffer;

import matter from 'gray-matter';

const modules = import.meta.glob('./*.md', { eager: true, query: '?raw', import: 'default' });

export const tuto = Object.entries(modules)
  .sort(([a], [b]) => a.localeCompare(b))
  .map(([, content]) => {
    const { data, content: body } = matter(content);
    return { ...data, body };
  });
