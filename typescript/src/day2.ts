import { AddDisplay } from "./display";

interface Command {
    command : string,
    value : number
}

interface State {
    pos : number,
    depth : number,
    aim : number
}

class Day2 {
    Parse(str : string) : Command {
        let s = str.split(' ')
        return { command: s[0], value: parseInt(s[1]) }
    }

    Execute(cmd : Command, state : State) {
        switch(cmd.command) {
            case 'forward':
                state.pos += cmd.value
                break;
            case 'up':
                state.depth -= cmd.value
                break
            case 'down':
                state.depth += cmd.value
                break
        }
    }

    Execute2(cmd : Command, state : State) {
        switch(cmd.command) {
            case 'forward':
                state.pos += cmd.value;
                state.depth += cmd.value*state.aim
                break
            case 'up':
                state.aim -= cmd.value
                break;
            case 'down':
                state.aim += cmd.value
                break
        }
    }


    run(input : string, output : HTMLElement) {
        let cmds = input.split('\n').filter(s => s.length > 0).map(this.Parse)
        let state = { pos: 0, depth: 0, aim: 0 }
        for (const c of cmds) {
            this.Execute(c, state)
        }
        output.innerText = `${state.pos*state.depth}`
        let state2 = { pos: 0, depth: 0, aim: 0 }
        for (const c of cmds) {
            this.Execute2(c, state2)
        }
        output.innerText += `\n${state2.pos*state2.depth}`
    }
}

AddDisplay(2, new Day2())