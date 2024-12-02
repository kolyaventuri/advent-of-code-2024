import path from 'node:path';
import fs from 'node:fs';

const file = path.join('input/day1.txt');
const content = fs.readFileSync(file, 'utf8');

const lines = content.split('\n').map(l => l.replace(/\s+/g, ' ')).filter(l => l.trim().length > 0);
const numbers = lines.map((line) => line.split(' ').map((num) => parseInt(num, 10)));

const sortLeft = numbers.map(([a]) => a).sort();
const sortRight = numbers.map(([, b]) => b).sort();

const diff = sortLeft.map((a, i) => Math.abs(a - sortRight[i]));
const sum = diff.reduce((a, b) => a + b, 0);

console.log('Part 1:', sum);

const count: Record<number, number> = {};

for (const n of sortRight) {
  count[n] = (count[n] || 0) + 1;
}

let sum2 = 0;
for (const n of sortLeft) {
  sum2 += n * (count[n] || 0);
}

console.log('Part 2:', sum2);
