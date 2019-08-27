module tools
!!$ Intrinsic standart iso_fortran Real types REAL32, REAL64, REAL128
  use, intrinsic :: iso_fortran_env
  implicit none

  public :: sp, dp, qp, rkind
  integer, parameter :: isp = int16
  integer, parameter :: idp = int32
  integer, parameter :: iqp = int64
  integer, parameter :: rsp = REAL32
  integer, parameter :: rdp = REAL64
  integer, parameter :: rqp = REAL128
  integer, parameter :: ikind = idp
  integer, parameter :: rkind = rdp

  interface assignment(=)
     procedure assignAnyTypeToR8Scalar
     procedure assignAnyTypeToR8Array
  end interface assignment(=)

contains
  
  subroutine assignAnyTypeToR8Scalar(output,input)
    implicit none
    real(dp), intent(Out) :: output
    class(*), intent(In) :: input
    select type(input)
    type is(integer)
       output=input
    type is(real(sp))
       output=input
    type is(real(dp))
       output=input
    type is(real(qp))
       output=input
    type is(character(*))
       write(*,*) 'Wrong type of argument input!'
       stop
    end select
  end subroutine assignAnyTypeToR8Scalar

  subroutine assignAnyTypeToR8Array(output,input)
    implicit none
    real(dp), intent(Out), dimension(:) :: output
    class(*), intent(In), dimension(:) :: input
    select type(input)
    type is(integer)
       output=input
    type is(real(sp))
       output=input
    type is(real(dp))
       output=input
    type is(real(qp))
       output=input
    type is(character(*))
       write(*,*) 'Wrong type of argument input!'
       stop
    end select
  end subroutine assignAnyTypeToR8Array
end module tools
