import { chromium } from 'playwright';

const target = process.argv[2] ?? 'http://127.0.0.1:4173';

async function main() {
  const browser = await chromium.launch();
  const page = await browser.newPage();
  let hasErrors = false;

  page.on('console', (msg) => {
    const entry = `[console.${msg.type()}] ${msg.text()}`;
    console.log(entry);
    if (msg.type() === 'error') {
      hasErrors = true;
    }
  });

  try {
    await page.goto(target, { waitUntil: 'networkidle' });
    await page.waitForTimeout(1500);
  } finally {
    await browser.close();
  }

  if (hasErrors) {
    process.exitCode = 1;
  }
}

main().catch((error) => {
  console.error('Unable to verify console output:', error);
  process.exitCode = 1;
});

