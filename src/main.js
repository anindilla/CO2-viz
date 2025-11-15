import { Elm } from './Main.elm';
import './styles.css';

const mountNode = document.getElementById('app');

const render = () => {
  mountNode.innerHTML = '';
  Elm.Main.init({ node: mountNode });
};

render();

if (import.meta.hot) {
  import.meta.hot.accept(() => {
    render();
  });
}

