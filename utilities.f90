Module tools
!!$ Intrinsic standart iso_fortran Real types REAL32, REAL64, REAL128
  Use, intrinsic :: iso_fortran_env
  Implicit none

  Public :: sp, dp, qp 
  Integer, parameter :: sp = REAL32
  Integer, parameter :: dp = REAL64
  Integer, parameter :: qp = REAL128

  Interface assignment(=)
     Procedure assignAnyTypeToR8Scalar
     Procedure assignAnyTypeToR8Array
  End Interface assignment(=)

Contains
  
  Subroutine assignAnyTypeToR8Scalar(output,input)
    Implicit none
    Real(dp), Intent(Out) :: output
    Class(*), Intent(In) :: input
    Select type(input)
    Type is(Integer)
       output=input
    Type is(Real(sp))
       output=input
    Type is(Real(dp))
       output=input
    Type is(Real(qp))
       output=input
    Type is(Character(*))
       Write(*,*) 'Wrong type of argument input!'
       STOP
    End Select
  End Subroutine assignAnyTypeToR8Scalar

  Subroutine assignAnyTypeToR8Array(output,input)
    Implicit none
    Real(dp), Intent(Out), dimension(:) :: output
    Class(*), Intent(In), dimension(:) :: input
    Select type(input)
    Type is(Integer)
       output=input
    Type is(Real(sp))
       output=input
    Type is(Real(dp))
       output=input
    Type is(Real(qp))
       output=input
    Type is(Character(*))
       Write(*,*) 'Wrong type of argument input!'
       STOP
    End Select
  End Subroutine assignAnyTypeToR8Array
End Module tools
