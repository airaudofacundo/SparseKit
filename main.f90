program Test
  use SparseKit
  implicit none
  type(Sparse) :: mat
  type(Sparse) :: transMat

  mat = sparse(7, 3)

  call mat%append(3.d0, 1, 1)
  call mat%append(2.d0, 1, 2)
  call mat%append(6,    2, 2)
  call mat%append(9.,   3, 3)
  call mat%append(7.d0, 3, 1)
  
  call mat%makeCRS

!!$  print'(A,I0,A,I0,A,F10.8)', 'mat(', 1, ',', 1, ') = ', mat%get(1,1)
!!$  print'(A,I0,A,I0,A,F10.8)', 'mat(', 1, ',', 2, ') = ', mat%get(1,2)
!!$  print'(A,I0,A,I0,A,F10.8)', 'mat(', 2, ',', 2, ') = ', mat%get(2,2)
!!$  print'(A,I0,A,I0,A,F10.8)', 'mat(', 3, ',', 3, ') = ', mat%get(3,3)
!!$  print'(A,I0,A,I0,A,F10.8)', 'mat(', 3, ',', 2, ') = ', mat%get(3,2)

!!$  call mat%printValue(1,1)
!!$  call mat%printValue(1,1,'values.dat')
!!$  call mat%printValue(3,3,'values.dat')

  call mat%printAll

  transMat = transpose(mat)

  call transMat%printAll

end program Test

