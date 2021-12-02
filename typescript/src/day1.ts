import { AddDisplay, Display } from "./display";

class Day1 {
    run(input : string, output : HTMLElement) {
        let lines = input.split('\n').map(n => parseInt(n)).filter(n => !isNaN(n))
        let countIncreasing = xs => {
            let result = 0
            let last = Number.MAX_VALUE
            for (let v of xs) {
                if (v > last) result++
                last = v
            }
            return result
        }
        let windows = lines.map((n, idx, arr) => (idx+2 < arr.length)?arr[idx]+arr[idx+1]+arr[idx+2]:0)

        output.innerText = `${countIncreasing(lines)} ${countIncreasing(windows)}`
    }
}

AddDisplay(1, new Day1())