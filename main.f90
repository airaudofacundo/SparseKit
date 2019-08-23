module RandomMod
  implicit none
  type(Sparse) :: matrixA
  type(Sparse) :: matrixL
  type(Sparse) :: ver

  matrixA = sparse( nnz = 16, rows = 4)

  call matrixA%append( value =    4, row = 1, col = 1)
  call matrixA%append( value =  -30, row = 1, col = 2)
  call matrixA%append( value =   60, row = 1, col = 3)
  call matrixA%append( value =  -35, row = 1, col = 4)
  call matrixA%append( value =  -30, row = 2, col = 1)
  call matrixA%append( value =  300, row = 2, col = 2)
  call matrixA%append( value = -675, row = 2, col = 3)
  call matrixA%append( value =  420, row = 2, col = 4)
  call matrixA%append( value =   60, row = 3, col = 1)
  call matrixA%append( value = -675, row = 3, col = 2)
  call matrixA%append( value = 1620, row = 3, col = 3)
  call matrixA%append( value =-1050, row = 3, col = 4)
  call matrixa%append( value =  -35, row = 4, col = 1)
  call matrixA%append( value =  420, row = 4, col = 2)
  call matrixA%append( value =-1050, row = 4, col = 3)
  call matrixA%append( value =  700, row = 4, col = 4)
  
  call matrixA%makeCRS

  print'(/,A)','Matrix A'
  call matrixA%printAll

  matrixL = lcholesky(matrixA)
  print'(/,A)','Matrix L'
  call matrixL%printAll

  ver = matrixL * transpose(matrixL)
  print'(/,A)','Verification'
  call ver%printAll
  
end program test
