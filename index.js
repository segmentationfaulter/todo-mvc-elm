import { Elm } from './src/Main.elm'

const app = Elm.Main.init({
  node: document.getElementById('app')
})

app.ports.persistData.subscribe((data) => {
  window.localStorage.setItem('todos-elm', JSON.stringify(data))
})
