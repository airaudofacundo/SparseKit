program test9
  use UtilitiesM
  use SparseKit
  implicit none
  type(Sparse) :: identity, matrixA, matrixL
  integer :: n

  print'(A)', 'SparseKit Test'

  n = 4

  matrixA = sparse(nnz = 16, rows = 4)

  call matrixA%append( val =    4._rkind, row = 1, col = 1)
  call matrixA%append( val =  -30._rkind, row = 1, col = 2)
  call matrixA%append( val =   60._rkind, row = 1, col = 3)
  call matrixA%append( val =  -35._rkind, row = 1, col = 4)
  call matrixA%append( val =  -30._rkind, row = 2, col = 1)
  call matrixA%append( val =  300._rkind, row = 2, col = 2)
  call matrixA%append( val = -675._rkind, row = 2, col = 3)
  call matrixA%append( val =  420._rkind, row = 2, col = 4)
  call matrixA%append( val =   60._rkind, row = 3, col = 1)
  call matrixA%append( val = -675._rkind, row = 3, col = 2)
  call matrixA%append( val = 1620._rkind, row = 3, col = 3)
  call matrixA%append( val =-1050._rkind, row = 3, col = 4)
  call matrixa%append( val =  -35._rkind, row = 4, col = 1)
  call matrixA%append( val =  420._rkind, row = 4, col = 2)
  call matrixA%append( val =-1050._rkind, row = 4, col = 3)
  call matrixA%append( val =  700._rkind, row = 4, col = 4)
  
  call matrixA%makeCRS

  print'(/,A)', 'Matrix A'
  call matrixA%printAll
  
  identity = id(n)

  print'(/,A)', 'Identity'
  call identity%printAll

  matrixL = transpose(matrixA)
  print'(/,A)','Matrix L'
  call matrixL%printAll
  
end program test9
