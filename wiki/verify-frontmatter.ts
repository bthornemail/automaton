import * as fs from 'fs';
import * as path from 'path';

function verifyFrontmatter(filePath: string): { valid: boolean; missing: string[] } {
  const content = fs.readFileSync(filePath, 'utf-8');
  const frontmatterMatch = content.match(/^---\n([\s\S]*?)\n---\n/);
  
  if (!frontmatterMatch) {
    return { valid: false, missing: ['frontmatter block'] };
  }
  
  const frontmatter = frontmatterMatch[1];
  const required = ['id:', 'title:', 'level:', 'type:', 'tags:', 'keywords:', 'prerequisites:', 'enables:', 'related:', 'readingTime:', 'difficulty:', 'blackboard:'];
  const missing = required.filter(field => !frontmatter.includes(field));
  
  return { valid: missing.length === 0, missing };
}

function main() {
  const wikiDir = path.join(__dirname);
  const files: string[] = [];
  
  function walkDir(dir: string) {
    const entries = fs.readdirSync(dir, { withFileTypes: true });
    for (const entry of entries) {
      const fullPath = path.join(dir, entry.name);
      if (entry.isDirectory()) {
        walkDir(fullPath);
      } else if (entry.isFile() && entry.name.endsWith('.md')) {
        files.push(fullPath);
      }
    }
  }
  
  walkDir(wikiDir);
  
  let valid = 0;
  let invalid = 0;
  
  for (const file of files) {
    const result = verifyFrontmatter(file);
    if (result.valid) {
      valid++;
    } else {
      invalid++;
      console.log(`✗ ${file}: Missing ${result.missing.join(', ')}`);
    }
  }
  
  console.log(`\n✓ Valid: ${valid}`);
  console.log(`✗ Invalid: ${invalid}`);
  console.log(`Total: ${files.length}`);
}

main();
