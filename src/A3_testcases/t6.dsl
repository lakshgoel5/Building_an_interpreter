matrixf 3 4 A := Input(t6_A.txt);
matrixf 5 2 B := Input(t6_B.txt);

matrixf 3 2 C;
C := A * B;
Print(C);

(*
import Matrix

A = input(t6_A.txt)   
B = input(t6_B.txt)

C = Matrix.multiply(A, B)
print(C)

// Here t6_A.txt has a float 3x4 matrix and t6_B.txt has a float 5x2 matrix*)