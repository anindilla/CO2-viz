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
    const gdp = toNumber(row.gdp);

    if (row.country === 'World') {
      globalPoints.push({
        year,
        co2,
        perCapita,
        share: null,
        population: population ? Math.round(population) : null,
        gdp: gdp ? Math.round(gdp) : null
      });
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
    entry.points.push({
      year,
      co2,
      perCapita,
      share,
      population: population ? Math.round(population) : null,
      gdp: gdp ? Math.round(gdp) : null
    });

    if (!entry.latest || year > entry.latest.year) {
      entry.latest = {
        year,
        co2,
        perCapita,
        share,
        population: population ? Math.round(population) : null,
        gdp: gdp ? Math.round(gdp) : null
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

  dashboard.analytics = buildAnalytics(dashboard, countryPoints);

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
    share: sorted.map((point) => point.share ?? null),
    population: sorted.map((point) => point.population ?? null),
    gdp: sorted.map((point) => point.gdp ?? null)
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

function buildAnalytics(dashboard, countryPoints) {
  const ranking = buildRanking(dashboard, countryPoints);
  const growth = buildGrowth(dashboard, ranking);
  const choropleth = buildMapPayload(ranking);

  return {
    ranking,
    growth,
    map: choropleth
  };
}

function buildRanking(dashboard, countryPoints) {
  const latestYear = Math.max(...dashboard.global.years);
  const prevYear = latestYear - 1;
  const globalTotal =
    dashboard.global.years.reduce((acc, year, idx) => {
      if (year === latestYear) {
        const value = dashboard.global.co2[idx];
        return Number.isFinite(value) ? value : acc;
      }
      return acc;
    }, 0) || 0;

  return Array.from(countryPoints.values())
    .map((entry) => {
      if (isAggregator(entry.name) || typeof entry.iso !== 'string' || entry.iso.length !== 3) return null;
      const latest = entry.points.find((point) => point.year === latestYear);
      if (!latest || latest.co2 === null) return null;

      const previous = entry.points.find((point) => point.year === prevYear);
      const yoy = previous && previous.co2 !== null ? Number((latest.co2 - previous.co2).toFixed(3)) : null;

      return {
        iso: entry.iso,
        name: entry.name,
        year: latestYear,
        total: latest.co2,
        yoy,
        perCapita: latest.perCapita,
        share: globalTotal > 0 ? Number(((latest.co2 / globalTotal) * 100).toFixed(3)) : null,
        population: latest.population
      };
    })
    .filter(Boolean)
    .sort((a, b) => b.total - a.total)
    .slice(0, 100);
}

function isAggregator(name) {
  if (!name) return false;
  const lower = name.toLowerCase();
  return (
    lower.includes('income') ||
    lower.includes('world') ||
    lower.includes('europe') ||
    lower.includes('(gcp') ||
    lower.includes('union') ||
    lower.includes('transport')
  );
}
function buildGrowth(dashboard, ranking) {
  const topIsos = ranking.slice(0, 5).map((row) => row.iso);
  const countriesByIso = new Map(dashboard.countries.map((country) => [country.iso, country]));

  const topSeries = topIsos
    .map((iso) => {
      const country = countriesByIso.get(iso);
      if (!country) return null;

      const points = country.series.years.map((year, idx) => {
        const value = country.series.co2[idx];
        return {
          year,
          value: value ?? 0
        };
      });

      return {
        iso,
        name: country.name,
        points
      };
    })
    .filter(Boolean);

  const globalPoints = dashboard.global.years.map((year, idx) => ({
    year,
    value: dashboard.global.co2[idx] ?? 0
  }));

  return {
    global: globalPoints,
    topEmitters: topSeries
  };
}

function buildMapPayload(ranking) {
  const values = ranking.map((row) => row.total).filter((value) => Number.isFinite(value) && value > 0);
  if (values.length === 0) {
    return {
      bins: [],
      entries: []
    };
  }

  const bins = computeQuantileBreaks(values, 5);

  return {
    bins,
    entries: ranking.map((row) => ({
      iso: row.iso,
      name: row.name,
      value: row.total
    }))
  };
}

function computeQuantileBreaks(values, segments) {
  const sorted = values.slice().sort((a, b) => a - b);
  const breaks = [];
  for (let i = 1; i < segments; i += 1) {
    const position = (sorted.length - 1) * (i / segments);
    const lower = Math.floor(position);
    const upper = lower + 1;
    const weight = position - lower;
    const value =
      upper < sorted.length ? sorted[lower] * (1 - weight) + sorted[upper] * weight : sorted[lower];
    breaks.push(Number(value.toFixed(2)));
  }
  breaks.push(Number(sorted[sorted.length - 1].toFixed(2)));
  return breaks;
}

main();

