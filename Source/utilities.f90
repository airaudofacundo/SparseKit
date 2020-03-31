module tools
  use DebuggerMOD
!!$ Intrinsic standart iso_fortran Real types REAL32, REAL64, REAL128
  use, intrinsic :: iso_fortran_env
  implicit none

  public :: isp, idp, iqp, ikind, rsp, rdp, rqp, rkind!, debugger
  
  type(DebuggerTYPE) :: debugger
  integer, parameter :: isp = int16
  integer, parameter :: idp = int32
  integer, parameter :: iqp = int64
  integer, parameter :: rsp = REAL32
  integer, parameter :: rdp = REAL64
  integer, parameter :: rqp = REAL128
  integer, parameter :: ikind = idp
  integer, parameter :: rkind = rdp
  
  interface assignAnyTypeToR8
     procedure assignAnyTypeToR8Scalar
     procedure assignAnyTypeToR8Array
  end interface assignAnyTypeToR8
  
contains
  
  subroutine assignAnyTypeToR8Scalar(output,input)
    implicit none
    real(rkind), intent(out) :: output
    class(*), intent(in) :: input
    select type(input)
    type is(integer)
       output=input
    type is(real(rsp))
       output=input
    type is(real(rdp))
       output=input
    type is(real(rqp))
       output=input
    type is(character(*))
       write(*,*) 'Wrong type of argument input!'
       stop
    end select
  end subroutine assignAnyTypeToR8Scalar

  subroutine assignAnyTypeToR8Array(output,input)
    implicit none
    real(rkind), intent(out), dimension(:) :: output
    class(*), intent(in), dimension(:) :: input
    select type(input)
    type is(integer)
       output=input
    type is(real(rsp))
       output=input
    type is(real(rdp))
       output=input
    type is(real(rqp))
       output=input
    type is(character(*))
       write(*,*) 'Wrong type of argument input!'
       stop
    end select
  end subroutine assignAnyTypeToR8Array
end module tools
