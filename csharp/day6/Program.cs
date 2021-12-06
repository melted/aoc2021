var input = File.ReadAllText("../../data/input6.txt").Split(',').Select(s => long.Parse(s));

var start = new long[10];

foreach (var n in input)
{
    start[n] += 1;
}

long[] Step(long[] previous)
{
    var next = new long[previous.Length];
    for(int i = 0; i < 8; i++) {
        next[i] = previous[i+1];
    }
    next[6] += previous[0];
    next[8] = previous[0];
    return next;
}

var part1 = start;
for (int i = 0; i < 80; i++)
{
    part1 = Step(part1);
}

var part2 = start;
for (int i = 0; i < 256; i++)
{
    part2 = Step(part2);
}

Console.WriteLine($"{part1.Sum()} {part2.Sum()}");

