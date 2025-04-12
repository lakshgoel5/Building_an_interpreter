vectorf 4 v := Input(t3_v.txt);
float sum_result := 0.0;
float ans;

for (int i := 0; i < dim(v); i := i + 1) {
  sum_result := sum_result + v[i];
};

ans := 2.5 * sum_result;
Print(ans);

(*import Matrix

v =  input(t3_v.txt)
sum_result = 0.0

for i = 0 to v.rows - 1:
    sum_result := sum_result + v[i, 0]

ans = 2.5 * sum_result
print(ans)

// Here t3_v.txt has a 4 x 1 float vector*)