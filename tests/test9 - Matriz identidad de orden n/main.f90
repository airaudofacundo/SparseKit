program test9
  use SparseKit
  implicit none
  type(Sparse) :: identity
  integer :: n

  n = 4

  identity = id(n)

  call identity%printAll

end program test9

