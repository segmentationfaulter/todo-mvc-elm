import { Elm } from './src/Main.elm'

const LOCAL_STORAGE_KEY = "todos-elm"
const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: hydrateData()
})

app.ports.persistData.subscribe((data) => {
  window.localStorage.setItem(LOCAL_STORAGE_KEY, JSON.stringify(data))
})

function hydrateData () {
  const data = window.localStorage.getItem(LOCAL_STORAGE_KEY)
  if (data) {
    const parsedData = JSON.parse(data)
    return parsedData
  }
  
  return {
    tasks: [],
    uid: 0,
  }
}
