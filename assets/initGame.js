const app = Elm.Main.init({
    node: document.getElementById('app'),
    flags: {
        randomSeed: (typeof crypto !== "undefined" && crypto.getRandomValues)
            ? crypto.getRandomValues(new Uint32Array(1))[0]
            : Math.floor(Math.random() * 4294967296)
    }
});