module RandomMod
  use tools
  implicit none
  private
  public :: randomReal, randomInt
  logical :: isInit = .false.
Contains
  subroutine randomInit
    use tools
    implicit none
    integer :: k, m
    integer :: count
    integer, dimension(:), allocatable :: seed
    Call system_clock(count)
    Call random_seed(size = m)
    Allocate(seed(m), source=count+37*[(k,k=0,m-1)])
    Call random_seed(put = seed)
    isInit = .true.
  end subroutine randomInit

  real(dp) function randomReal(lower, upper)
    use tools
    implicit none
    class(*), intent(in) :: lower
    class(*), intent(in) :: upper
    real(dp) :: l
    real(dp) :: u
    if(.not.isInit) call randomInit
    l = lower
    u = upper
    call random_number(randomReal)
    randomReal = (u-l)*randomReal + l
  end function randomReal

  integer function randomInt(lower, upper)
    use tools
    implicit none
    integer, intent(in) :: lower
    integer, intent(in) :: upper
    real(dp) :: randomNumber
    if(.not.isInit) call randomInit
    call random_number(randomNumber)
    randomInt = nint((upper-lower)*randomNumber + lower)
  end function randomInt
end module RandomMod
  
program timeTest
  use tools
  use RandomMOD
  use SparseKit
  implicit none
  type(Sparse) :: a
  type(Sparse) :: b
  type(Sparse) :: c
  type(Sparse) :: d
  !choose order for a and b, and max number of nnz
  integer, parameter :: MAX_NNZ_A = 500000
  integer, parameter :: MAX_NNZ_B = 500000
  integer, parameter :: N = 50000
  integer :: i
  real(dp) :: start1, start2, finish1, finish2
  
  a = sparse(nnz = MAX_NNZ_A, rows = N)
  b = sparse(nnz = MAX_NNZ_B, rows = N)
  
  !random a
  do i = 1, MAX_NNZ_A
     call a%append(                  &
          value = randomReal(0,100)  &
          , row = randomInt(1, N)      &
          , col = randomInt(1, N)      )
  end do
  call a%makeCRS
  !random b
  do i = 1, MAX_NNZ_b
     call b%append(                  &
          value = randomReal(0,100)  &
          , row = randomInt(1, N)      &
          , col = randomInt(1, N)      )
  end do
  call b%makeCRS

!!$  call a%printNonZeros('annz.dat')
!!$  call b%printNonZeros('bnnz.dat')
  call cpu_time(start1)
  c = sparse_sparse_prod_viejo(a,b)
  call cpu_time(finish1)
  !call c%printNonZeros('nnz1.dat')
  call c%free()
  call cpu_time(start2)
  d = a*b
  call cpu_time(finish2)
  !call d%printNonZeros('nnz2.dat')
  call d%free()


  print*, 'método 1 time = ', finish1-start1
  print*, 'método 2 time = ', finish2-start2



  
  
end program timeTest



