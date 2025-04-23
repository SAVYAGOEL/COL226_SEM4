import Matrix

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

// Here t1_A.txt, t1_b.txt have 5 x 6 float matrix and 5 x 1 float vector