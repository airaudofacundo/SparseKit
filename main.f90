program test
  use SparseKit
  implicit none
  type(Sparse) :: matrixA
  type(Sparse) :: inversematrixA
  type(Sparse) :: inverseGMRESDmatrixA

  matrixA = sparse(16,4)
  
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

  print*,''
  print*,'Matrix A'
  call matrixA%printAll

  inversematrixA = inverse(matrixA)

  print*,''
  print*,'Inverse'
  call inversematrixA%printAll

  print*,''
  print*,'InverseGMRESD'
  inverseGMRESDmatrixA = inverseGMRESD(matrixA)
  call inverseGMRESDmatrixA%printAll

end program test
