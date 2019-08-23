program test2
  use SparseKit
  implicit none
  type(Sparse) :: matrixA
  type(Sparse) :: matrixB
  type(Sparse) :: addition
  type(Sparse) :: subtraction
  type(Sparse) :: multiplication
  real*8       :: vector(4), coef

  coef = 2.d0
  vector  = (/2.2, 6.3, 5.1, 7.5/)
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

  matrixB = sparse( nnz = 12, rows = 4)

  call matrixB%append( value =    4, row = 1, col = 1)
  call matrixB%append( value =    1, row = 1, col = 2)
  call matrixB%append( value =   60, row = 1, col = 3)
  call matrixB%append( value =   -5, row = 1, col = 4)
  call matrixB%append( value =  300, row = 2, col = 2)
  call matrixB%append( value = -675, row = 2, col = 3)
  call matrixB%append( value =  420, row = 2, col = 4)
  call matrixB%append( value =   60, row = 3, col = 1)
  call matrixB%append( value =    1, row = 3, col = 3)
  call matrixB%append( value =  -35, row = 4, col = 1)
  call matrixB%append( value =  420, row = 4, col = 2)
  call matrixB%append( value =  700, row = 4, col = 4)
  
  call matrixB%makeCRS

  print'(/,A)','Matrix B'
  call matrixB%printAll

  print'(/,A)','Vector'
  print'(*(E10.4,2X))', vector

  addition = matrixA + matrixB
  print'(/,A)','A + B'
  call addition%printAll
  
  subtraction = matrixA - matrixB
  print'(/,A)','A - B'
  call subtraction%printAll

  multiplication = matrixA*matrixB
  print'(/,A)','A * B'
  call multiplication%printAll

  multiplication = coef*matrixA
  print'(/,A)','2 * A'
  call multiplication%printAll
  
  print'(/,A)','A*V'
  print'(*(E10.4,2X))', matrixA * vector

end program test2
