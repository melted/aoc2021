
export interface Display {
    run : (input : string, output : HTMLDivElement) => void
}

class DisplayElement {
    n : number
    display : Display
    inputUri : string

    constructor(n : number, disp : Display) {
        this.n = n
        this.display = disp
        this.inputUri = document.location.origin + `/input/input${n}.txt`
    }

    Render(el : HTMLElement) {
        let div = document.createElement('div')
        div.classList.add('disp_box')
        let header = document.createElement('div')
        header.classList.add('disp_header')
        let output = document.createElement('div')
        output.classList.add('disp_output')
        header.innerHTML = `<h2>Day ${this.n}</h2><a href='${this.inputUri}'>input data</a>`
        let button = document.createElement('button')
        button.textContent = 'Run'
        let time = document.createElement('div')
        button.addEventListener('click', async () => {
            let text = await this.LoadInput()
            let timeStart = Date.now()
            this.display.run(text, output)
            let timeEnd = Date.now()
            time.innerText = `Time: ${timeEnd - timeStart} ms`
        })
        header.appendChild(button)
        header.appendChild(time)
        div.appendChild(header)
        div.appendChild(output)
        el.appendChild(div)
    }

    async LoadInput() {
        let response = await fetch(this.inputUri)
        return response.text()
    }
}

let displays : DisplayElement[] = []

export function AddDisplay(n : number, disp : Display) {
    displays[n] = new DisplayElement(n, disp)
}

export function RenderDisplays(el : HTMLElement) {
    for(let d of displays) {
        if (d) d.Render(el)
    }
}