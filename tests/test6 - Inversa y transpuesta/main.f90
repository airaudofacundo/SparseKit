program test6
  use SparseKit
  implicit none
  type(Sparse) :: matrix
  type(Sparse) :: inverseMatrix
  type(Sparse) :: transposeMatrix
  type(Sparse) :: verification 

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

  call matrix%printAll

  transposeMatrix = transpose(matrix)
  call transposeMatrix%printAll

  inverseMatrix = inverse(matrix)
  call inverseMatrix%printAll

  verification = matrix*inverseMatrix
  call verification%printAll

end program test6