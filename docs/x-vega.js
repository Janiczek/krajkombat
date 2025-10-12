var requestAnimationFrame = window.requestAnimationFrame || window.mozRequestAnimationFrame || window.webkitRequestAnimationFrame || window.msRequestAnimationFrame;

customElements.define('x-vega',
    class extends HTMLElement {
        constructor() { super(); this._spec = null; }

        get spec() {
            return (this._spec);
        }

        set spec(value) {
            if (value != this._spec) {
                this._spec = value;
                this.renderPlot();
            }
        }

        renderPlot() {
            const spec = this._spec;
            const thisref = this;
            if (!spec) {
                requestAnimationFrame(function () {
                    while (thisref.firstChild) {
                        thisref.removeChild(thisref.firstChild);
                    }
                });
            } else {
                vegaEmbed(spec, { actions: false }).then((newplot) => {
                    requestAnimationFrame(function () {
                        while (thisref.firstChild) {
                            thisref.removeChild(thisref.firstChild);
                        }
                        thisref.appendChild(newplot);
                    })
                });
            }
        }
    }
);