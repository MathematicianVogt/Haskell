data Exp = T ExpP

data ExpP = Add T ExpP | Subtract T ExpP | epsilon 

data T = F TP
data TP = Multiply F TP | Divison F TP | epsilon
data F = F | Exponential P F | P
data P = Num | Id | Id  OP Exp CP |  OP Expc CP
