var input = File.ReadAllLines("d:/Niklas/repos/aoc2021/data/input1.txt")
                .Where(s => s.Length > 0).Select(s => int.Parse(s));

var count = input.Zip(input.Skip(1), (a, b) => a < b).Count(t => t);

Console.WriteLine(count);

var count2 = input.Zip(input.Skip(3), (a, b) => a < b).Count(t => t);

Console.WriteLine(count2);