program Test
  use SparseKit
  implicit none
  type(Sparse) :: mat
  type(Sparse) :: transMat
  type(Sparse) :: inversa
  type(Sparse) :: prod
  real*8 :: v(4,4),d(4)
  Real*8 :: B(3),x(3)
  integer :: i,j

  B = (/1.,2.5,5./)
  mat = sparse(16, 4)

  call mat%append(4,  1, 1)
  call mat%append(-30,  1, 2)
  call mat%append(60,  1, 3)
  call mat%append(-35, 1, 4)
  call mat%append(-30,  2, 1)
  call mat%append(300,  2, 2)
  call mat%append(-675,  2, 3)
  call mat%append(420,  2, 4)
  call mat%append(60,  3, 1)
  call mat%append(-675,  3, 2)
  call mat%append(1620, 3, 3)
  call mat%append(-1050,  3, 4)
  call mat%append(-35,  4, 1)
  call mat%append(420,  4, 2)
  call mat%append(-1050,  4, 3)
  call mat%append(700,  4, 4)
  
  call mat%makeCRS

  call mat%printAll()
  !Verificacion de Operaciones
  print*,'--------------------------------------------------------------'
  print*,'-----------------------OPERACIONES----------------------------'
  print*,'--------------------------------------------------------------' 
  print*,'Matriz'
  call mat%printAll
  print*,'--------------------------------------------------------------'
!!$  print*,'Vector'
!!$  print*, B
!!$  print*,'--------------------------------------------------------------'
!!$  x = gmres(mat, B)
!!$  print*,'Solucion de Matriz*x = Vector'
!!$  print*, x
!!$  print*,'--------------------------------------------------------------'
!!$  print*,'Verificacion: Matriz*Solucion = Vector'
!!$  print*, mat*x
!!$  print*,'--------------------------------------------------------------'
!!$  inversa = inverse(mat) 
!!$  print*,'Inversa de Matriz'
!!$  call inversa%printAll
!!$  print*,'--------------------------------------------------------------'
!!$  prod = inversa*mat 
!!$  print*,'Producto de Inversa por Matriz'
!!$  call prod%printAll
!!$  print*,'--------------------------------------------------------------'
!!$  print*,'Transpuesta de Matriz'
!!$  transMat = transpose(mat)
!!$  call transMat%printAll
!!$  print*,'--------------------------------------------------------------'
!!$  print*,'Borrado de fila 2 columna 3'
!!$  call mat%deleteRowAndCol(2,3)
!!$  call mat%printAll()
!!$  print*,'--------------------------------------------------------------'
!!$  call jacobiEigen(mat,d,V)
!!$  print*,'Autovalores'
!!$  print*,d
!!$  print*,'Autovectores'
!!$  do i = 1,4
!!$     print*, (V(i,j),j=1,4)
!!$  end do
!!$  print*,'--------------------------------------------------------------'

end program Test
