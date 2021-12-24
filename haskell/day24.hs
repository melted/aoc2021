import qualified Data.Map as M
import Data.Char

import Debug.Trace

data Reg = X | Y | Z | W deriving (Show, Eq, Ord, Enum)
data Op = R Reg | Imm Int deriving (Show, Eq)
data Instruction = Inp Reg | Add Reg Op | Mul Reg Op
         | Div Reg Op | Mod Reg Op | Eql Reg Op deriving (Show, Eq)

data Expr =
    Lit Int | Input Int | Addt Expr Expr | Multiply Expr Expr |
    Divide Expr Expr | Modulo Expr Expr | Equal Expr Expr deriving (Show, Eq)


reg "x" = X
reg "y" = Y
reg "z" = Z
reg "w" = W
reg _ = error "wrong reg"

op x | isAsciiLower (head x) = R (reg x)
op x = Imm (read x)

data State = State (M.Map Reg Int) [Instruction] [Int]

data EState = EState (M.Map Reg Expr) [Instruction] Int

initRegs = M.fromList [(X,0), (Y,0), (Z,0), (W,0)]

initERegs = M.fromList [(X, Lit 0), (Y, Lit 0), (Z, Lit 0), (W, Lit 0)]

start = State initRegs

parse s = case words s of
            ["inp", x] -> Inp (reg x)
            ["add", x, y] -> Add (reg x) (op y)
            ["mul", x, y] -> Mul (reg x) (op y)
            ["div", x, y] -> Div (reg x) (op y)
            ["mod", x, y] -> Mod (reg x) (op y)
            ["eql", x, y] -> Eql (reg x) (op y)
            _ -> error "parse error"

getReg regs r = regs M.! r

getOp regs (R r) = regs M.! r
getOp _ (Imm i) = i

getOpE regs (R r) = regs M.! r
getOpE _ (Imm i) = Lit i

execute (State regs ((Inp r):xs) (i:is)) =
    State (M.insert r i regs) xs is
execute (State regs ((Add r o):xs) is) =
    State (M.insert r (getReg regs r + getOp regs o) regs) xs is
execute (State regs ((Mul r o):xs) is) =
    State (M.insert r (getReg regs r * getOp regs o) regs) xs is
execute (State regs ((Div r o):xs) is) =
    State (M.insert r (getReg regs r `div` getOp regs o) regs) xs is
execute (State regs ((Mod r o):xs) is) =
    State (M.insert r (getReg regs r `mod` getOp regs o) regs) xs is
execute (State regs ((Eql r o):xs) is) =
    State (M.insert r (if getReg regs r == getOp regs o then 1 else 0) regs) xs is
execute _ = error "bad instruction"

run (State regs [] _) = regs
run s@(State regs (x:xs) is) = traceShow (regs, x, length is) $ run (execute s)

getData = do
    input <- readFile "../data/input24.txt"
    pure $ map parse $ lines input


biggest =  [5,9,6,9,2,9,9,4,9,9,4,9,9,8]
smallest = [1,6,1,8,1,1,1,1,6,4,1,5,2,1]

main = do
    d <- getData
    let r =  run (start d biggest)
    print r
    let r =  run (start d smallest)
    print r
    