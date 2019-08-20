program test8
  use SparseKit
  implicit none
  type(Sparse) :: matrixA
  type(Sparse) :: matrixB
  type(Sparse) :: addition
  type(Sparse) :: multiplication
  real*8 :: vector(4)

  vector = (/2.2, 6.3, 5.1, 7.5/)

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

  print*,'Matrix A'
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

  print*,'Matrix B'
  call matrixB%printAll

  print*,'Vector'
  print*, vector

  multiplication = matrixA*matrixB
  print*,'A * B'
  call multiplication%printAll

  addition = matrixA + matrixB
  print*,'A + B'
  call addition%printAll

  print*,'A*V'
  print*, matrixA * vector

!!$  print*,'V*A'
!!$  print*, vector * matrixA
end program test8
