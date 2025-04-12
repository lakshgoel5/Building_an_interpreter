matrixf 6 6 A := Input(t1_A.txt);
vectorf 6 b := Input(t1_b.txt);

matrixf 6 6 A_T := transpose(A);

matrixf 6 6 A_TA := A_T * A;

matrixf 6 1 b_mat;
for(int i := 0; i < dim(b); i := i + 1){
    b_mat[i,0] := b[i];
};


if ( det(A_TA) != 0.0 )
then{
    matrixf 6 6 MA_TA_inv := inv(A_TA);
    (*Print(MA_TA_inv);*) (*Correct*)
    matrixf 6 1 A_Tb := A_T * b_mat;
    matrixf 6 1 theta := MA_TA_inv * A_Tb;
    Print(theta);
}
else{
    raise(MatrixNotInvertible);
};

(*Removed -ve from input A*)

(*import Matrix

A = input(t1_A.txt)
b = input(t1_b.txt)

A_T = Matrix.transpose(A)
A_TA = Matrix.multiply(A_T, A)

if Matrix.determinant(A_TA) != 0:
    A_TA_inv = Matrix.inverse(A_TA)
    A_Tb = Matrix.vector_multiply(A_T, b)
    theta = Matrix.vector_multiply(A_TA_inv, A_Tb)
    print(theta)
else:
    raise MatrixNotInvertible

// Here t1_A.txt, t1_b.txt have 5 x 6 float matrix and 5 x 1 float vector*)