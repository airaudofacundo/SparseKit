program test3
  use SparseKit
  implicit none
  type(Sparse) :: matrix

  matrix = sparse( nnz = 12, rows = 4)

  call matrix%append( val =    4.d0, row = 1, col = 1)
  call matrix%append( val =    1.d0, row = 1, col = 2)
  call matrix%append( val =   60.d0, row = 1, col = 3)
  call matrix%append( val =   -5.d0, row = 1, col = 4)
  call matrix%append( val =  300.d0, row = 2, col = 2)
  call matrix%append( val = -675.d0, row = 2, col = 3)
  call matrix%append( val =  420.d0, row = 2, col = 4)
  call matrix%append( val =   60.d0, row = 3, col = 1)
  call matrix%append( val =    1.d0, row = 3, col = 3)
  call matrix%append( val =  -35.d0, row = 4, col = 1)
  call matrix%append( val =  420.d0, row = 4, col = 2)
  call matrix%append( val =  700.d0, row = 4, col = 4)
  
  call matrix%makeCRS

  print'(/,A)','Print Value'
  call matrix%printValue   (3,2)
  print'(/,A)','Print Non Zeros'
  call matrix%printNonZeros
  print'(/,A)','Print All'
  call matrix%printAll     

  call matrix%printNonZeros('NonZeros.dat')
  call matrix%printValue   ( 3, 2,'Value.dat')
  call matrix%printAll     ('matrix.dat')

end program test3

