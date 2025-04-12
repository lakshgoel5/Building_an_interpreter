matrixf 3 3 x := 3,3[[1.2,3.2,1.1];[2.0,4.3,5.1];[1.0,2.0,3.0]];
matrixf 3 3 v := inv(x);
x:= x*v;
Print(x);