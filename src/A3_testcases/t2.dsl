matrixf 4 5 A := Input(t2_A.txt);
matrixf 4 5 B := Input(t2_B.txt);
matrixf 5 4 D := Input(t2_D.txt);
vectorf 4 u := Input(t2_u.txt);

matrixf 4 5 C;
C := A + B;

matrixf 4 4 E;
E := C * D;

matrixf 4 1 u_mat;

for (int i := 0; i < 4; i := i + 1) {
  for (int j := 0; j < 1; j := j + 1) {
    u_mat[i,j] := u[i];
  };
};

if (det(E) != 0.0) then{
  matrixf 4 4 E_inverse;
  E_inverse := inv(E);
  matrixf 4 1 x;
  x := E_inverse * u_mat;
  Print(x);
} else {
  raise(MatrixNotInvertible);
};

(*Changed file A,B,D,u*)

(*import Matrix

A = input(t2_A.txt)
B = input(t2_B.txt)
D = input(t2_D.txt)
u = input(t2_u.txt)

C = Matrix.add(A, B)

E = Matrix.multiply(C, D)

if Matrix.determinant(E) != 0:
    E_inverse = Matrix.inverse(E)
    x = Matrix.vector_multiply(E_inverse, u)
    print(x)
else:
    raise MatrixNotInvertible

// Here t2_A.txt, t2_B.txt have 4 x 5 float matrix, t2_D.txt has 5 x 4 float matrix, and t2_u.txt has a 4 x 1 float vector*)