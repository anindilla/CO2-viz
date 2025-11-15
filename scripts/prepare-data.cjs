'use strict';

const fs = require('fs');
const path = require('path');
const { parse } = require('csv-parse/sync');

const DATASET_PATH = path.join(__dirname, '..', 'dataset', 'visualizing_global_co2_data.csv');
const OUTPUT_DIR = path.join(__dirname, '..', 'public', 'data');
const OUTPUT_FILE = path.join(OUTPUT_DIR, 'dashboard.json');

const START_YEAR = 1900;
const EMITTER_YEAR_START = 1990;
const EMITTER_LIMIT = 10;
const PRECISION = 3;

function toNumber(value) {
  if (value === undefined || value === null) return null;
  const trimmed = String(value).trim();
  if (trimmed === '') return null;
  const num = Number(trimmed);
  return Number.isFinite(num) ? num : null;
}

function roundFloat(value) {
  if (value === null || value === undefined) return null;
  return Number(Number(value).toFixed(PRECISION));
}

function main() {
  const rawCsv = fs.readFileSync(DATASET_PATH, 'utf8');
  const records = parse(rawCsv, {
    columns: true,
    skip_empty_lines: true
  });

  const globalPoints = [];
  const countryPoints = new Map();
  const emittersMap = new Map();

  for (const row of records) {
    const year = toNumber(row.year);
    if (year === null || year < START_YEAR) continue;

    const iso = row.iso_code && row.iso_code.trim() !== '' ? row.iso_code.trim() : row.country;
    if (!iso) continue;

    const co2 = roundFloat(toNumber(row.co2));
    const perCapita = roundFloat(toNumber(row.co2_per_capita));
    const share = roundFloat(toNumber(row.share_global_co2));
    const population = toNumber(row.population);

    if (row.country === 'World') {
      globalPoints.push({ year, co2, perCapita });
    }

    if (!countryPoints.has(iso)) {
      countryPoints.set(iso, {
        iso,
        name: row.country,
        points: [],
        latest: null
      });
    }

    const entry = countryPoints.get(iso);
    entry.points.push({ year, co2, perCapita, share });

    if (!entry.latest || year > entry.latest.year) {
      entry.latest = {
        year,
        co2,
        perCapita,
        share,
        population: population ? Math.round(population) : null
      };
    }

    if (year >= EMITTER_YEAR_START && co2 !== null) {
      if (!emittersMap.has(year)) {
        emittersMap.set(year, []);
      }
      emittersMap.get(year).push({
        iso,
        name: row.country,
        co2,
        share
      });
    }
  }

  const dashboard = {
    version: 1,
    generatedAt: new Date().toISOString(),
    global: finalizeSeries(globalPoints),
    countries: finalizeCountries(countryPoints),
    emitters: finalizeEmitters(emittersMap)
  };

  fs.mkdirSync(OUTPUT_DIR, { recursive: true });
  fs.writeFileSync(OUTPUT_FILE, JSON.stringify(dashboard));
  const sizeKb = (fs.statSync(OUTPUT_FILE).size / 1024).toFixed(1);
  console.log(`Dashboard ready (${sizeKb} kB) with ${dashboard.countries.length} countries.`);
}

function finalizeSeries(points) {
  const sorted = points.sort((a, b) => a.year - b.year);
  return {
    years: sorted.map((point) => point.year),
    co2: sorted.map((point) => point.co2),
    perCapita: sorted.map((point) => point.perCapita),
    share: sorted.map((point) => point.share ?? null)
  };
}

function finalizeCountries(map) {
  return Array.from(map.values())
    .filter((entry) => entry.points.length > 0)
    .map((entry) => ({
      iso: entry.iso,
      name: entry.name,
      series: finalizeSeries(entry.points),
      latest: entry.latest
    }))
    .sort((a, b) => a.name.localeCompare(b.name));
}

function finalizeEmitters(map) {
  return Array.from(map.entries())
    .map(([year, list]) => ({
      year,
      items: list
        .sort((a, b) => b.co2 - a.co2)
        .slice(0, EMITTER_LIMIT)
    }))
    .sort((a, b) => a.year - b.year);
}

main();

