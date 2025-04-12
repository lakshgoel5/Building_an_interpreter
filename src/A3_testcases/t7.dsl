matrixf 3 3 A := Input(t7_A.txt);
vectorf 3 v := Input(t7_v.txt);

matrixf 3 3 C;
C := A + v;

(*
import Matrix

A = input(t7_A.txt)    
v = input(t7_v.txt)   

C = Matrix.add(A, v) 
print(C)

// Here t7_A.txt has a float 3×3 matrix and t7_v.txt has a 3×1 float vector*)