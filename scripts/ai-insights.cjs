'use strict';

const fs = require('fs');
const path = require('path');
const crypto = require('crypto');
const { parse } = require('csv-parse/sync');

const DATASET_PATH = path.join(__dirname, '..', 'dataset', 'visualizing_global_co2_data.csv');
const OUTPUT_FILE = path.join(__dirname, '..', 'public', 'data', 'insights.json');

const MODEL = process.env.HF_MODEL || 'mistralai/Mixtral-8x7B-Instruct';
const API_URL = `https://api-inference.huggingface.co/models/${MODEL}`;
const HF_TOKEN = process.env.HF_TOKEN;
const FORCE = process.env.FORCE_AI === 'true';

const MAX_PROMPT_CHARS = Number(process.env.AI_PROMPT_LIMIT ?? '240000');

async function main() {
  const csv = fs.readFileSync(DATASET_PATH, 'utf8');
  const datasetHash = crypto.createHash('sha256').update(csv).digest('hex').slice(0, 32);

  if (!FORCE && fs.existsSync(OUTPUT_FILE)) {
    try {
      const existing = JSON.parse(fs.readFileSync(OUTPUT_FILE, 'utf8'));
      if (existing.datasetHash === datasetHash) {
        console.log('AI insights already up to date. Skipping.');
        return;
      }
    } catch (error) {
      console.warn('Existing insights.json could not be parsed. Regenerating.');
    }
  }

  let result;

  if (HF_TOKEN) {
    try {
      result = await requestAiInsights(csv);
    } catch (error) {
      console.warn(`AI summarization failed (${error.message}). Falling back to heuristic insights.`);
    }
  } else {
    console.warn('HF_TOKEN not provided. Generating deterministic fallback insights.');
  }

  if (!result) {
    result = buildFallbackInsights(csv);
  }

  const output = {
    generatedAt: new Date().toISOString(),
    model: result.model,
    source: result.source,
    status: result.status,
    datasetHash,
    insights: result.insights
  };

  fs.mkdirSync(path.dirname(OUTPUT_FILE), { recursive: true });
  fs.writeFileSync(OUTPUT_FILE, JSON.stringify(output, null, 2));
  const sizeKb = (fs.statSync(OUTPUT_FILE).size / 1024).toFixed(1);
  console.log(`Insights ready (${output.status}) ${sizeKb} kB -> ${OUTPUT_FILE}`);
}

function buildPrompt(csv) {
  const trimmedCsv =
    csv.length > MAX_PROMPT_CHARS ? `${csv.slice(0, MAX_PROMPT_CHARS)}\n<!-- CSV truncated -->` : csv;

  return `
You are an expert climate analyst. Read the CSV and respond ONLY with JSON following this schema:
{
  "highlights": [
    { "title": "string", "detail": "string", "evidence": "short citation" }
  ],
  "narrative": "2-3 sentences weaving the story",
  "actions": ["3 concise mitigation or policy actions"],
  "questions": ["3 open questions the data raises"]
}
Prefer absolute numbers (Mt CO2) and keep each string under 220 characters.
CSV_DATA_BEGIN
${trimmedCsv}
CSV_DATA_END
`;
}

async function requestAiInsights(csv) {
  if (typeof fetch !== 'function') {
    throw new Error('Global fetch is unavailable in this Node.js runtime.');
  }

  const response = await fetch(API_URL, {
    method: 'POST',
    headers: {
      Authorization: `Bearer ${HF_TOKEN}`,
      'Content-Type': 'application/json'
    },
    body: JSON.stringify({
      inputs: buildPrompt(csv),
      parameters: {
        max_new_tokens: 800,
        temperature: 0.2,
        return_full_text: false
      }
    })
  });

  if (!response.ok) {
    const body = await response.text();
    throw new Error(`HuggingFace request failed (${response.status}): ${body}`);
  }

  const payload = await response.json();
  const generated = Array.isArray(payload)
    ? payload.map((entry) => entry.generated_text || entry.summary_text || '').join('\n').trim()
    : payload.generated_text || payload.data || '';

  const parsed = safeParseJSON(generated);
  if (!parsed) {
    throw new Error('AI response was not valid JSON.');
  }

  return {
    model: MODEL,
    source: 'huggingface',
    status: 'success',
    insights: normalizeInsights(parsed)
  };
}

function safeParseJSON(text) {
  try {
    return JSON.parse(text);
  } catch (error) {
    const firstBrace = text.indexOf('{');
    const lastBrace = text.lastIndexOf('}');
    if (firstBrace !== -1 && lastBrace !== -1 && lastBrace > firstBrace) {
      try {
        return JSON.parse(text.slice(firstBrace, lastBrace + 1));
      } catch (innerError) {
        return null;
      }
    }
    return null;
  }
}

function normalizeInsights(parsed) {
  const highlights = Array.isArray(parsed.highlights) ? parsed.highlights : [];
  const actions = Array.isArray(parsed.actions) ? parsed.actions : [];
  const questions = Array.isArray(parsed.questions) ? parsed.questions : [];

  return {
    highlights: highlights.map((item) => ({
      title: sanitizeText(item.title) || 'Insight',
      detail: sanitizeText(item.detail || item.text || item.note),
      evidence: sanitizeText(item.evidence || item.source)
    })),
    narrative: sanitizeText(parsed.narrative || parsed.story || ''),
    actions: actions.map(sanitizeText).filter(Boolean),
    questions: questions.map(sanitizeText).filter(Boolean)
  };
}

function sanitizeText(value) {
  if (typeof value !== 'string') {
    return '';
  }
  return value.replace(/\s+/g, ' ').trim();
}

function buildFallbackInsights(csv) {
  const records = parse(csv, { columns: true, skip_empty_lines: true });

  const totals = new Map();
  let minYear = Infinity;
  let maxYear = -Infinity;
  let worldLatest = null;
  let worldPrev = null;

  for (const row of records) {
    const year = Number(row.year);
    if (!Number.isFinite(year)) continue;
    minYear = Math.min(minYear, year);
    maxYear = Math.max(maxYear, year);

    const co2 = Number(row.co2);
    const perCapita = Number(row.co2_per_capita);

    if (row.country === 'World') {
      if (!worldLatest || year > worldLatest.year) {
        worldPrev = worldLatest;
        worldLatest = { year, co2, perCapita };
      } else if (!worldPrev || (year > worldPrev.year && year < worldLatest.year)) {
        worldPrev = { year, co2, perCapita };
      }
    }

    if (Number.isFinite(co2)) {
      const iso = row.iso_code || row.country;
      totals.set(iso, {
        name: row.country,
        total: (totals.get(iso)?.total || 0) + co2
      });
    }
  }

  const sortedTotals = Array.from(totals.values())
    .sort((a, b) => b.total - a.total)
    .slice(0, 3);

  const highlights = [
    formattedHighlight(
      'Dataset span',
      `Records cover ${minYear}–${maxYear} with ${records.length.toLocaleString()} observations.`,
      'year column'
    )
  ];

  if (worldLatest && worldPrev && Number.isFinite(worldLatest.co2) && Number.isFinite(worldPrev.co2)) {
    const delta = worldLatest.co2 - worldPrev.co2;
    const direction = delta >= 0 ? 'above' : 'below';
    highlights.push(
      formattedHighlight(
        'Global momentum',
        `World CO₂ was ${worldLatest.co2.toFixed(1)} Mt in ${worldLatest.year}, ${Math.abs(delta).toFixed(
          1
        )} Mt ${direction} ${worldPrev.year}.`,
        'World rows'
      )
    );
  }

  if (sortedTotals.length > 0) {
    const leader = sortedTotals[0];
    highlights.push(
      formattedHighlight(
        'Top emitter',
        `${leader.name} contributes ${leader.total.toFixed(0)} Mt across the record, leading global totals.`,
        'co2 column'
      )
    );
  }

  const narrative = `The CO₂ dataset spans ${minYear}-${maxYear}, tracing the rise of fossil emissions from industrialized economies while global totals climbed into the tens of gigatonnes.`;

  return {
    model: 'local-fallback',
    source: 'deterministic-summary',
    status: 'fallback',
    insights: {
      highlights,
      narrative,
      actions: [
        'Scale zero-carbon grids to displace coal and gas baseload.',
        'Cut oil demand with transit, EV freight, and active mobility.',
        'Rebuild materials loops with circular steel, cement, and recycling.'
      ],
      questions: [
        'Which regions bent their emissions curves after policy shifts?',
        'How fast can top emitters close the gap to net-zero trajectories?',
        'Where do per-capita trends diverge from economic growth?'
      ]
    }
  };
}

function formattedHighlight(title, detail, evidence) {
  return {
    title,
    detail,
    evidence
  };
}

main().catch((error) => {
  console.error(error);
  process.exitCode = 1;
});

