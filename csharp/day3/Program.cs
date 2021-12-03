var input = File.ReadAllLines("d:/Niklas/repos/aoc2021/data/input3.txt");

var split = input.Select(s => s.ToCharArray().Select(c => c == '1' ? 1 : 0));
var gammaPop = split.Aggregate((acc, x) => acc.Zip(x, (a, b) => a + b)).Select(n => n > input.Length/2 ? 1 : 0);
var epsilonPop = gammaPop.Select(x => 1-x);

Func<IEnumerable<int>, int> ToDecimal = pop => pop.Aggregate((acc, x) => acc*2+x);

var gamma = ToDecimal(gammaPop);
var epsilon = ToDecimal(epsilonPop);
Console.WriteLine($"{gamma*epsilon}");

// Part 2
int Sift(IEnumerable<IEnumerable<int>> vals, bool leastCommon = false)
{
    Func<IEnumerable<int>, int> MostCommon = pop => pop.Count(n => n == 1) >= pop.Count(n => n == 0) ? 1 : 0;
    var sifted = vals.Select(s => s.ToArray()).ToHashSet();
    var index = 0;
    while (sifted.Count() > 1)
    {
        var keeper = MostCommon(sifted.Select(s => s[index]));
        if (leastCommon) keeper = (1 - keeper);
        sifted = sifted.Where(s => s[index] == keeper).ToHashSet();
        index++;
    }
    return ToDecimal(sifted.First());
}

var oxy = Sift(split);
var co2 = Sift(split, true);
Console.WriteLine($"{oxy*co2}");

