matrixf 4 5 A := Input(t4_A.txt);
float threshold := 1e-3;
float sum_of_squares := 0.0;
float norm;

for (int i := 0; i < row(A); i := i + 1) {
  for (int j := 0; j < cols(A); j := j + 1) {
    sum_of_squares := sum_of_squares + A[i,j] * A[i,j];
  };
};

norm := sqrt(sum_of_squares);

while (norm > threshold) {
  for (int i := 0; i < row(A); i := i + 1) {
    for (int j := 0; j < cols(A); j := j + 1) {
      A[i,j] := A[i,j] * 0.5
    };
  };
  
  sum_of_squares := 0.0;
  
  for (int i := 0; i < row(A); i := i + 1) {
    for (int j := 0; j < cols(A); j := j + 1) {
      sum_of_squares := sum_of_squares + A[i,j] * A[i,j];
    };
  };
  
  norm := sqrt(sum_of_squares);
};

Print(A);


(*import Matrix

A = input(t4_A.txt)
threshold = 1e-3
sum_of_squares = 0.0
for i = 0 to A.rows - 1:
    for j = 0 to A.columns - 1:
        sum_of_squares := sum_of_squares + A[i, j] * A[i, j]
norm = sqrt(sum_of_squares)

while norm > threshold:
    for i = 0 to A.rows - 1:
        for j = 0 to A.columns - 1:
            A[i, j] := A[i, j] * 0.5
    sum_of_squares = 0.0
    for i = 0 to A.rows - 1:
        for j = 0 to A.columns - 1:
            sum_of_squares := sum_of_squares + A[i, j] * A[i, j]
    norm = sqrt(sum_of_squares)

print(A)

// Here t4_A.txt has a 4 x 5 float matrix*)