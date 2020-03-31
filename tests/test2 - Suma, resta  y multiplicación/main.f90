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
  call matrixA%append( val =    4.d0, row = 1, col = 1)
  call matrixA%append( val =  -30.d0, row = 1, col = 2)
  call matrixA%append( val =   60.d0, row = 1, col = 3)
  call matrixA%append( val =  -35.d0, row = 1, col = 4)
  call matrixA%append( val =  -30.d0, row = 2, col = 1)
  call matrixA%append( val =  300.d0, row = 2, col = 2)
  call matrixA%append( val = -675.d0, row = 2, col = 3)
  call matrixA%append( val =  420.d0, row = 2, col = 4)
  call matrixA%append( val =   60.d0, row = 3, col = 1)
  call matrixA%append( val = -675.d0, row = 3, col = 2)
  call matrixA%append( val = 1620.d0, row = 3, col = 3)
  call matrixA%append( val =-1050.d0, row = 3, col = 4)
  call matrixA%append( val =  -35.d0, row = 4, col = 1)
  call matrixA%append( val =  420.d0, row = 4, col = 2)
  call matrixA%append( val =-1050.d0, row = 4, col = 3)
  call matrixA%append( val =  700.d0, row = 4, col = 4)
  
  call matrixA%makeCRS

  print'(/,A)','Matrix A'
  call matrixA%printAll

  matrixB = sparse( nnz = 12, rows = 4)

  call matrixB%append( val =    4.d0, row = 1, col = 1)
  call matrixB%append( val =    1.d0, row = 1, col = 2)
  call matrixB%append( val =   60.d0, row = 1, col = 3)
  call matrixB%append( val =   -5.d0, row = 1, col = 4)
  call matrixB%append( val =  300.d0, row = 2, col = 2)
  call matrixB%append( val = -675.d0, row = 2, col = 3)
  call matrixB%append( val =  420.d0, row = 2, col = 4)
  call matrixB%append( val =   60.d0, row = 3, col = 1)
  call matrixB%append( val =    1.d0, row = 3, col = 3)
  call matrixB%append( val =  -35.d0, row = 4, col = 1)
  call matrixB%append( val =  420.d0, row = 4, col = 2)
  call matrixB%append( val =  700.d0, row = 4, col = 4)
  
  call matrixB%makeCRS

  print'(/,A)','Matrix B'
  call matrixB%printAll

  print'(/,A)','Vector'
  print'(*(E14.7,2X))', vector

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
  print'(*(E14.7,2X))', matrixA * vector

end program test2
