program Test
  use SparseKit
  implicit none
  type(Sparse) :: mat
  type(Sparse) :: transMat
  type(Sparse) :: inversa
  Real*8 :: B(3)

  B = (/1.,2.5,5./)
  mat = sparse(10, 3)

  call mat%append(3., 1, 1)
  call mat%append(4., 1, 2)
  call mat%append(5., 1, 3)
  call mat%append(2., 2, 1)
  call mat%append(6., 2, 2)
  call mat%append(4., 2, 3)
  call mat%append(5., 3, 1)
  call mat%append(8., 3, 2)
  call mat%append(9., 3, 3)
  
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

  print*,'solucion'
  print*, gmres(mat, B)

  inversa = inverse(mat) 
  print*,'inversa'
  call inversa%printAll

!!$  call mat%deleteRowAndCol(2,3)

!!$  call mat%printAll()
!!$  transMat = transpose(mat)
!!$  call transMat%printAll
end program Test

