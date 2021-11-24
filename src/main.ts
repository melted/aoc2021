
export class Main {
    NewDiv(message) {
        let div = document.createElement('div')
        div.innerText = message
        return div
    }

    SayHello() {
        console.log("here")
        let body = document.getElementsByTagName("body")[0]
        body.appendChild(this.NewDiv('hellow'))
    }

    async LoadInput(file) {
        let uri = document.location.origin + `/input/${file}`
        let response = await fetch(uri)
        return response.text()
    }
}

console.log(document.location)
var main = new Main()
main.SayHello()

main.LoadInput('input1.txt').then(t => console.log(t))