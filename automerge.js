// https://automerge.org/docs/library_initialization/#unbundled-vanilla-js
automergeInitP = Promise.all([
    import("https://esm.sh/@automerge/automerge-repo@2.0.0-alpha.14/slim?bundle-deps")
        .then((m) => {
            window.AutomergeRepo = m
            window.Automerge = m.Automerge
        }),
    import("https://esm.sh/@automerge/automerge-repo-storage-indexeddb@2.0.0-alpha.14?bundle-deps")
        .then((m) => window.IndexedDBStorageAdapter = m.IndexedDBStorageAdapter),
    import("https://esm.sh/@automerge/automerge-repo-network-websocket@2.0.0-alpha.14?bundle-deps")
        .then((m) => window.BrowserWebSocketClientAdapter = m.BrowserWebSocketClientAdapter),
    import("https://esm.sh/@automerge/automerge-repo-network-messagechannel@2.0.0-alpha.14?bundle-deps")
        .then((m) => window.MessageChannelNetworkAdapter = m.MessageChannelNetworkAdapter)
]).then(() => {
    console.log("Automerge modules loaded")
    return AutomergeRepo.initializeWasm(fetch("https://esm.sh/@automerge/automerge/dist/automerge.wasm"))
}).then(() => {
    console.log("Automerge Wasm initialized")
    window.repo = new AutomergeRepo.Repo({
        storage: new IndexedDBStorageAdapter(),
        // network: [new BrowserWebSocketClientAdapter("wss://sync.automerge.org")],
        network: [new BrowserWebSocketClientAdapter("ws://localhost:3030")],
    })
    console.log("Automerge repo constructed")
})

async function createDocument(prop, json) {
    await automergeInitP
    console.log("Creating document from JSON: \n" + json)
    var data = JSON.parse(json)
    var holder = {}
    holder[prop] = data
    var h = repo.create(holder)
    console.log("Created document: " + h.url)
    return h.url
}

async function updateDocument(url, prop, json) {
    await automergeInitP
    console.log("Updating document from JSON: \n" + json)
    var h = repo.find(url)
    var data = JSON.parse(json)
    h.change(holder => holder[prop] = data)
}

async function findDocument(url, prop) {
    await automergeInitP
    if (!AutomergeRepo.isValidAutomergeUrl(url))
        return null
    var doc = await repo.find(url).doc()
    if (!doc)
        return null
    var propVal = doc[prop]
    if (!propVal)
        return null
    return JSON.stringify(propVal)
}

async function deleteDocument(url) {
    await automergeInitP
    repo.delete(url)
}
