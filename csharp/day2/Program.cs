var cmds = File.ReadAllLines("d:/Niklas/repos/aoc2021/data/input2.txt")
            .Select(s => s.Split(' ')).Select(p => (p[0], int.Parse(p[1])));

(int, int) ExecCmd((int pos, int depth) state, (string cmd, int val) cmd) =>
    cmd switch {
        ("up", var x) => (state.pos, state.depth - x),
        ("down", var x) => (state.pos, state.depth + x),
        ("forward", var x) => (state.pos + x, state.depth)
    };

var (pos, depth) = cmds.Aggregate((0,0), (acc, c) => ExecCmd(acc, c));

Console.WriteLine(pos*depth);

(int, int, int) ExecCmd2((int pos, int depth, int aim) state, (string cmd, int val) cmd) =>
    cmd switch {
        ("up", var x) => (state.pos, state.depth, state.aim - x),
        ("down", var x) => (state.pos, state.depth, state.aim + x),
        ("forward", var x) => (state.pos + x, state.depth + x * state.aim, state.aim)
    };

var (pos2, depth2, _) = cmds.Aggregate((0, 0, 0), (acc, c) => ExecCmd2(acc, c));

Console.WriteLine(pos2*depth2);
