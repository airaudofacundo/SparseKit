program test1
  use SparseKit
  implicit none
  type(Sparse) :: matrix

  matrix = sparse( nnz = 16, rows = 4)

  call matrix%append( val =    4.d0, row = 1, col = 1)
  call matrix%append( val =  -30.d0, row = 1, col = 2)
  call matrix%append( val =   60.d0, row = 1, col = 3)
  call matrix%append( val =  -35.d0, row = 1, col = 4)
  call matrix%append( val =  -30.d0, row = 2, col = 1)
  call matrix%append( val =  300.d0, row = 2, col = 2)
  call matrix%append( val = -675.d0, row = 2, col = 3)
  call matrix%append( val =  420.d0, row = 2, col = 4)
  call matrix%append( val =   60.d0, row = 3, col = 1)
  call matrix%append( val = -675.d0, row = 3, col = 2)
  call matrix%append( val = 1620.d0, row = 3, col = 3)
  call matrix%append( val =-1050.d0, row = 3, col = 4)
  call matrix%append( val =  -35.d0, row = 4, col = 1)
  call matrix%append( val =  420.d0, row = 4, col = 2)
  call matrix%append( val =-1050.d0, row = 4, col = 3)
  call matrix%append( val =  700.d0, row = 4, col = 4)
  
  call matrix%makeCRS

  print'(/,A,E12.5,/)','A(3,2) = ', matrix%get      ( 3, 3)
  print'(A,1X,I0,/)','nnz    = ', matrix%getnnz()
  print'(A,1X,I0,/)','n      = ', matrix%getn()
  
end program test1

