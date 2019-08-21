program test8
  use SparseKit
  implicit none
  type(Sparse) :: matrix
  real*8 :: vector(4)

  vector = (/2.2, 6.3, 5.1, 7.5/)

  matrix = sparse( nnz = 19, rows = 4)

  call matrix%append( value =    4, row = 1, col = 1)
  call matrix%append( value =  -30, row = 1, col = 2)
  call matrix%append( value =   60, row = 1, col = 3)
  call matrix%append( value =  -35, row = 1, col = 4)
  call matrix%append( value =  -30, row = 2, col = 1)
  call matrix%append( value =  300, row = 2, col = 2)
  call matrix%append( value = -675, row = 2, col = 3)
  call matrix%append( value =  420, row = 2, col = 4)
  call matrix%append( value =   60, row = 3, col = 1)
  call matrix%append( value = -675, row = 3, col = 2)
  call matrix%append( value = 1620, row = 3, col = 3)
  call matrix%append( value =-1050, row = 3, col = 4)
  call matrix%append( value =  -35, row = 4, col = 1)
  call matrix%append( value =  420, row = 4, col = 2)
  call matrix%append( value =-1050, row = 4, col = 3)
  call matrix%append( value =  700, row = 4, col = 4)
  
  call matrix%makeCRS

  print*,'Matrix'
  call matrix%printAll

  print*,'Vector'
  print*, vector
  print*,'Solution'
  print*, gmres(matrix, vector)

  print*,'Verification (M*x=V)'
  print*, matrix*gmres(matrix, vector)

end program test8
