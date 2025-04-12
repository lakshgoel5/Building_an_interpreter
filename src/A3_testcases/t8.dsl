matrixf 2 2 A := Input(t8_A.txt);

if (det(A) != 0.0) then {
  matrixf 2 2 A_inv;
  A_inv := inv(A);
  Print(A_inv);
} else {
  raise(MatrixNotInvertible)
};

(*import Matrix

A = input(t8_A.txt)   

if Matrix.determinant(A) != 0:
    A_inv = Matrix.inverse(A)
    print(A_inv)
else:
    raise MatrixNotInvertible

// Here t8_A.txt has a 2×2 matrix with determinant 0
*)