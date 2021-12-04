var input = File.ReadAllText("D:/Niklas/repos/aoc2021/data/input4.txt");

var splits = input.Split("\n\n");

(int rounds, int score) ParseBoard(string data, List<int> drawings)
{
    var rows = data.Split('\n', StringSplitOptions.RemoveEmptyEntries)
                .Select(s => s.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                .Select(s => int.Parse(s)).ToArray()).ToList();
    var rowRounds = rows.Select(row => row.Select(v => drawings.IndexOf(v)).Select(i => i < 0?1000:i).Max()).Min();
    var cols = Enumerable.Range(0, rows.Count()).Select(i => rows.Select(r => r.ElementAt(i)).ToArray()).ToList();
    var colRounds = cols.Select(col => col.Select(v => drawings.IndexOf(v)).Select(i => i < 0?1000:i).Max()).Min();
    var rounds = Math.Min(colRounds, rowRounds);
    var lastVal = drawings[rounds];
    var ignore = drawings.Take(rounds+1).ToHashSet();
    var score = rows.Select(row => row.Select(v => ignore.Contains(v) ? 0 : v).Sum()).Sum();
    return (rounds, score*lastVal);
}

var drawing = splits[0].Split(',').Select(s => int.Parse(s)).ToList();

var boards = splits.Skip(1).Select(s => ParseBoard(s, drawing));

var fastest = boards.MinBy(b => b.rounds);
var slowest = boards.MaxBy(b => b.rounds);

Console.WriteLine($"{fastest.score} {slowest.score}");

