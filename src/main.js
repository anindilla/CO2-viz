import maplibregl from 'maplibre-gl';
import 'maplibre-gl/dist/maplibre-gl.css';
import { Elm } from './Main.elm';
import './styles.css';

const mountNode = document.getElementById('app');

let mapInstance = null;
let mapMarkers = [];
let pendingPoints = null;

const destroyMap = () => {
  mapMarkers.forEach((marker) => marker.remove());
  mapMarkers = [];
  pendingPoints = null;

  if (mapInstance) {
    mapInstance.remove();
    mapInstance = null;
  }
};

const render = () => {
  if (!mountNode) return;
  destroyMap();
  mountNode.innerHTML = '';
  const app = Elm.Main.init({ node: mountNode });
  console.log('Elm app initialized');
  setupMapBridge(app);
  dedupePanels();
  observePanels();
};

const setupMapBridge = (app) => {
  if (app?.ports?.mapUpdate) {
    app.ports.mapUpdate.subscribe((payload) => {
      requestAnimationFrame(() => updateMap(payload));
      dedupePanels();
    });
  }
};

const dedupePanels = () => {
  const seen = new Set();
  const container = document.querySelector('.stack');
  if (!container) return;
  container.querySelectorAll('section.panel[data-panel]').forEach((section) => {
    const key = section.getAttribute('data-panel');
    if (!key) return;
    if (seen.has(key)) {
      section.remove();
    } else {
      seen.add(key);
    }
  });
};

const observePanels = () => {
  const container = document.querySelector('.stack');
  if (!container || container.__dedupeObserver) return;
  const observer = new MutationObserver(() => {
    dedupePanels();
  });
  observer.observe(container, { childList: true });
  container.__dedupeObserver = observer;
};

const ensureMap = () => {
  if (mapInstance) return mapInstance;

  const container = document.getElementById('emissions-map');
  if (!container) return null;

  mapInstance = new maplibregl.Map({
    container,
    style: 'https://demotiles.maplibre.org/style.json',
    center: [ 10, 20 ],
    zoom: 1.25,
    attributionControl: false
  });

  return mapInstance;
};

const updateMap = (points = []) => {
  const map = ensureMap();
  if (!map) {
    pendingPoints = points;
    window.setTimeout(() => {
      if (pendingPoints) {
        updateMap(pendingPoints);
      }
    }, 80);
    return;
  }

  pendingPoints = null;
  mapMarkers.forEach((marker) => marker.remove());
  mapMarkers = points.map((point) => {
    const marker = new maplibregl.Marker({ color: '#2563eb' })
      .setLngLat([ point.lon, point.lat ])
      .setPopup(
        new maplibregl.Popup({ offset: 12 }).setHTML(
          `<strong>${point.name}</strong><br>${point.co2.toFixed(1)} Mt`
        )
      )
      .addTo(map);

    return marker;
  });
};

render();

if (import.meta.hot) {
  import.meta.hot.accept(() => {
    render();
  });
}

