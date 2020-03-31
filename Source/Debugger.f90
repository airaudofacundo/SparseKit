module DebuggerMOD
  implicit none
  public
  type :: DebuggerTYPE
     logical :: isProblemInitiated = .false.
     logical :: isDomainInitiated = .false.
     logical :: isElementListInitiated = .false.
     logical :: areBoundaryCondInitiated = .false.
   contains
  end type DebuggerTYPE
  interface debugLog
     procedure :: printStr
     procedure :: printStrInt
     procedure :: printStrIntStr
     procedure :: printStrStr
  end interface debugLog
  integer, parameter :: logUnit = 95
  integer, dimension(8) :: dateAndTime
  procedure(printStrON), pointer :: printStr
  procedure(printStrIntON), pointer :: printStrInt
  procedure(printStrIntStrON), pointer :: printStrIntStr
  procedure(printStrStrON), pointer :: printStrStr
contains
  subroutine initLog(isWorking, logFile)
    implicit none
    logical, intent(in) :: isWorking
    character(*), optional, intent(in) :: logFile
    if(isWorking) then
       printStr => printStrON
       printStrInt => printStrIntON
       printStrIntStr => printStrIntStrON
       printStrStr => printStrStrON
       open(logUnit, file = trim(logFile))
    else
       printStr => printStrOFF
       printStrInt => printStrIntOFF
       printStrIntStr => printStrIntStrOFF
       printStrStr => printStrStrOFF
    end if
    call date_and_time(VALUES = dateAndTime)
    write(logUnit,'(I2,A,I2,A,I2,2X,A)') dateAndTime(5),":",dateAndTime(6),":",dateAndTime(7) &
         , 'Initiating Program'
  end subroutine initLog
  
  subroutine printStrON(str)
    implicit none
    character(*), intent(in) :: str
    call date_and_time(VALUES = dateAndTime)
    write(logUnit,'(I2,A,I2,A,I2,2X,A)') dateAndTime(5),":",dateAndTime(6),":",dateAndTime(7), str
  end subroutine printStrON
  subroutine printStrOFF(str)
    implicit none
    character(*), intent(in) :: str
  end subroutine printStrOFF
  subroutine printStrIntON(str, int)
    implicit none
    character(*), intent(in) :: str
    integer, intent(in) :: int
    call date_and_time(VALUES = dateAndTime)
    write(logUnit,'(I2,A,I2,A,I2,2X,A,I0)') dateAndTime(5),":",dateAndTime(6),":",dateAndTime(7), str, int
  end subroutine printStrIntON
  subroutine printStrIntOFF(str, int)
    implicit none
    character(*), intent(in) :: str
    integer, intent(in) :: int
  end subroutine printStrIntOFF
  subroutine printStrIntStrON(str1, int, str2)
    implicit none
    character(*), intent(in) :: str1
    integer, intent(in) :: int
    character(*), intent(in) :: str2
    call date_and_time(VALUES = dateAndTime)
    write(logUnit,'(I2,A,I2,A,I2,2X,A,I0,A)') dateAndTime(5),":",dateAndTime(6),":",dateAndTime(7), str1, int, str2
  end subroutine printStrIntStrON
  subroutine printStrIntStrOFF(str1, int, str2)
    implicit none
    character(*), intent(in) :: str1
    integer, intent(in) :: int
    character(*), intent(in) :: str2
  end subroutine printStrIntStrOFF
  subroutine printStrStrON(str1, str2)
    implicit none
    character(*), intent(in) :: str1
    character(*), intent(in) :: str2
    call date_and_time(VALUES = dateAndTime)
    write(logUnit,'(I2,A,I2,A,I2,2X,A,A)') dateAndTime(5),":",dateAndTime(6),":",dateAndTime(7), str1, str2
  end subroutine printStrStrON
    subroutine printStrStrOFF(str1, str2)
    implicit none
    character(*), intent(in) :: str1
    character(*), intent(in) :: str2
  end subroutine printStrStrOFF
end module DebuggerMOD

