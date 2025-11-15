'use strict';

const fs = require('fs');
const path = require('path');

const OUTPUT_PATH = path.join(__dirname, '..', 'src', 'elm.js');
const NEEDLE = '}(this));';
const REPLACEMENT = '}(typeof globalThis !== "undefined" ? globalThis : this));';

function main() {
  if (!fs.existsSync(OUTPUT_PATH)) {
    console.warn(`Could not find ${OUTPUT_PATH} to patch.`);
    return;
  }

  const content = fs.readFileSync(OUTPUT_PATH, 'utf8');

  if (!content.includes(NEEDLE) && content.includes(REPLACEMENT)) {
    return;
  }

  const patched = content.replace(NEEDLE, REPLACEMENT);
  fs.writeFileSync(OUTPUT_PATH, patched);
  console.log('Patched Elm output for environments without window.this');
}

main();


