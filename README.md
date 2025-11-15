# Carbon Pulse (Elm + Vite)

Story-driven CO₂ explorer that preprocesses the Our World in Data CSV into a compact JSON feed, then renders lightweight Elm visualizations (global trajectories, regional composition, Pareto shares, GDP vs CO₂ scatter, and an interactive MapLibre highlight of the top emitters).

## Local development

```bash
cd "/Users/dilleuh/Coding/CO2 Viz"
npm install
npm run dev        # Vite dev server (auto re-runs data prep)
```

Preview the production bundle (used for Vercel) with:

```bash
npm run build
npm run preview -- --host 127.0.0.1 --port 4281
```

## Data pipeline

- Source: `dataset/visualizing_global_co2_data.csv` (Our World in Data).
- `npm run prepare:data` (auto-run before dev/build) slices/quantizes the dataset into `public/data/dashboard.json`.
- The Elm decoder expects the shape defined in `src/Data.elm` (series arrays for `co2`, `perCapita`, `share`, `population`, `gdp`).

## UI sections

1. **Hero metrics** – latest global totals, per-capita, coverage, and year slider.
2. **Global momentum** – toggle between absolute vs per-capita trajectories.
3. **Map of major emitters** – MapLibre markers from Elm port payloads.
4. **Composition** – switch between top countries vs income-group aggregates.
5. **Pareto concentration** – cumulative share of top emitters.
6. **GDP vs CO₂ scatter** – log-friendly bubble plot (population-sized).
7. **Country compare** – dual panels with sparklines + contextual badges.

## Testing / verification

- `npm run build` — ensures Elm compilation + Vite bundling succeed.
- `npm run preview` — smoke-test the production build.
- `npm run test:console` — Playwright-based console scan (optional; requires browsers installed).

## Deployment

Vercel can use:

```
Build command: npm run prepare:data && npm run build
Output dir: dist
```

Ensure `public/data/dashboard.json` is committed, or the pipeline will regenerate it during the build step. MapLibre uses the public demo style URL by default; swap `style` in `src/main.js` if you need a custom map or self-hosted tiles.


