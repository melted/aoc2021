var input = File.ReadAllLines("../../data/input5.txt").Select(s => Parse(s)).ToList();

Line Parse(string s)
{
    var regex = new Regex(@"(\d+),(\d+) -> (\d+),(\d+)");
    var match = regex.Match(s);
    if (match.Success)
    {
        var from = (int.Parse(match.Groups[1].Value), int.Parse(match.Groups[2].Value));
        var to = (int.Parse(match.Groups[3].Value), int.Parse(match.Groups[4].Value));
        return new Line(from, to);
    }
    throw new ArgumentException();
}

int[] AddPoints(int[] board, IEnumerable<(int x, int y)> points)
{
    foreach (var point in points)
    {
        var index = 1000*point.y+point.x;
        board[index] = board[index] + 1;
    }
    return board;
}

var horizontals = input.Where(l => !l.Diagonal()).Aggregate(new int[1000000], (board, l) => AddPoints(board, l.Points()));

var part1 = horizontals.Count(v => v > 1);

var diagonals =  input.Aggregate(new int[1000000], (board, l) => AddPoints(board, l.Points()));

var part2 = diagonals.Count(v => v > 1);

Console.WriteLine($"{part1} {part2}");

record struct Line((int x, int y) From, (int x, int y) To)
{
    public bool Diagonal() => From.x != To.x && From.y != To.y;

    public List<(int, int)> Points()
    {
        var result = new List<(int,int)>();
        var dirX = Math.Sign(To.x - From.x);
        var dirY = Math.Sign(To.y - From.y);
        for ((int x, int y) p = From; p != (To.x + dirX, To.y+dirY); p = (p.x+dirX, p.y+dirY))
        {
            result.Add(p);
        }
        return result;
    }
};