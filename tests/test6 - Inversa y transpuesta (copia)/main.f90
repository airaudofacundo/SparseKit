program test6
  use SparseKit
  implicit none
  type(Sparse) :: matrix
  type(Sparse) :: inverseMatrix
  type(Sparse) :: inverseGMRESDMatrix
  type(Sparse) :: verification1
  type(Sparse) :: verification2 

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

  print'(/,A)','Matrix'  
  call matrix%printAll

  print'(/,A)','Inverse matrix'
  inverseMatrix = inverse(matrix)
  call inverseMatrix%printAll

  print'(/,A)','Verification 1'
  verification1 = matrix*inverseMatrix
  call verification1%printAll

  print'(/,A)','Inverse GMRESD Algorithm  matrix'
  inverseGMRESDMatrix = inverseGMRESD(matrix)
  call inverseGMRESDMatrix%printAll
  
  print'(/,A)','Verification 2'
  verification2 = matrix*inverseGMRESDMatrix
  call verification2%printAll

end program test6
