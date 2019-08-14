program Test
  use SparseKit
  implicit none
  type(Sparse) :: mat
  type(Sparse) :: transMat
  type(Sparse) :: inversa
  type(Sparse) :: prod
  Real*8 :: B(3),x(3)

  B = (/1.,2.5,5./)
  mat = sparse(7, 3)

  call mat%append(3., 1, 1)
  call mat%append(5., 1, 2)
  call mat%append(5., 2, 1)
  call mat%append(12., 2, 2)
  call mat%append(5., 2, 3)
  call mat%append(5., 3, 2)
  call mat%append(3., 3, 3)
  
  call mat%makeCRS

  !Verificacion de Operaciones
  print*,'--------------------------------------------------------------'
  print*,'-----------------------OPERACIONES----------------------------'
  print*,'--------------------------------------------------------------' 
  print*,'Matriz'
  call mat%printAll
  print*,'--------------------------------------------------------------'
  print*,'Vector'
  print*, B
  print*,'--------------------------------------------------------------'
  x = gmres(mat, B)
  print*,'Solucion de Matriz*x = Vector'
  print*, x
  print*,'--------------------------------------------------------------'
  print*,'Verificacion: Matriz*Solucion = Vector'
  print*, mat*x
  print*,'--------------------------------------------------------------'
  inversa = inverse(mat) 
  print*,'Inversa de Matriz'
  call inversa%printAll
  print*,'--------------------------------------------------------------'
  prod = inversa*mat 
  print*,'Producto de Inversa por Matriz'
  call prod%printAll
  print*,'--------------------------------------------------------------'
  print*,'Transpuesta de Matriz'
  transMat = transpose(mat)
  call transMat%printAll
  print*,'--------------------------------------------------------------'
  print*,'Borrado de fila 2 columna 3'
  call mat%deleteRowAndCol(2,3)
  call mat%printAll()
  print*,'--------------------------------------------------------------'
end program Test

