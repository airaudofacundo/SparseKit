module QuickSortM
  !Takes 1, 2, 3, 4.. arrays of same dimension, orders first one and modifies the others acordingly
  use UtilitiesM
  implicit none
  public
  
  interface swap
     module procedure swapInt
     module procedure swapReal
  end interface swap
  
  interface quickSort
     module procedure quickSort1vectI
     module procedure quickSort1vectR
     module procedure quickSort2vectII
     module procedure quickSort2vectIR
     module procedure quickSort2vectRR
     module procedure quickSort2vectRI
     module procedure quickSort3vectIII
     module procedure quickSort3vectIIR
     module procedure quickSort3vectIRI
     module procedure quickSort3vectIRR
     module procedure quickSort3vectRRR
     module procedure quickSort3vectRRI
     module procedure quickSort3vectRIR
     module procedure quickSort3vectRII
     module procedure quickSort4vectIIII
     module procedure quickSort4vectIIIR
     module procedure quickSort4vectIIRI
     module procedure quickSort4vectIIRR
     module procedure quickSort4vectIRII
     module procedure quickSort4vectIRIR
     module procedure quickSort4vectIRRI
     module procedure quickSort4vectIRRR
     module procedure quickSort4vectRIII
     module procedure quickSort4vectRIIR
     module procedure quickSort4vectRIRI
     module procedure quickSort4vectRIRR
     module procedure quickSort4vectRRII
     module procedure quickSort4vectRRIR
     module procedure quickSort4vectRRRI
     module procedure quickSort4vectRRRR
  end interface quickSort
  
contains

  recursive subroutine quickSort1vectR(vect1, low, high)
    implicit none
    real(rkind), dimension(:), intent(InOut) :: vect1
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, low, iPivot-1)
       call quickSort(vect1, iPivot+1, high)
    end if       
  end subroutine quickSort1vectR

  recursive subroutine quickSort1vectI(vect1, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect1
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, low, iPivot-1)
       call quickSort(vect1, iPivot+1, high)
    end if       
  end subroutine quickSort1vectI

  recursive subroutine quickSort2vectIR(vect1, vect2, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect1
    real(rkind), dimension(:), intent(InOut) :: vect2
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, low, iPivot-1)
       call quickSort(vect1, vect2, iPivot+1, high)
    end if       
  end subroutine quickSort2vectIR
  
  recursive subroutine quickSort2vectII(vect1, vect2, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect1
    integer(ikind), dimension(:), intent(InOut) :: vect2
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, low, iPivot-1)
       call quickSort(vect1, vect2, iPivot+1, high)
    end if       
  end subroutine quickSort2vectII

  recursive subroutine quickSort2vectRI(vect1, vect2, low, high)
    implicit none
    real(rkind), dimension(:), intent(InOut) :: vect1
    integer(ikind), dimension(:), intent(InOut) :: vect2
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, low, iPivot-1)
       call quickSort(vect1, vect2, iPivot+1, high)
    end if       
  end subroutine quickSort2vectRI

  recursive subroutine quickSort2vectRR(vect1, vect2, low, high)
    implicit none
    real(rkind), dimension(:), intent(InOut) :: vect1
    real(rkind), dimension(:), intent(InOut) :: vect2
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, low, iPivot-1)
       call quickSort(vect1, vect2, iPivot+1, high)
    end if       
  end subroutine quickSort2vectRR

  recursive subroutine quickSort3vectIII(vect1, vect2, vect3, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect1, vect2, vect3
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, iPivot+1, high)
    end if       
  end subroutine quickSort3vectIII

  recursive subroutine quickSort3vectIIR(vect1, vect2, vect3, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect1, vect2
    real(rkind), dimension(:), intent(InOut) :: vect3
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, iPivot+1, high)
    end if       
  end subroutine quickSort3vectIIR

  recursive subroutine quickSort3vectIRR(vect1, vect2, vect3, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect1
    real(rkind), dimension(:), intent(InOut) :: vect2, vect3
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, iPivot+1, high)
    end if       
  end subroutine quickSort3vectIRR

  recursive subroutine quickSort3vectIRI(vect1, vect2, vect3, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect1, vect3
    real(rkind), dimension(:), intent(InOut) :: vect2
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, iPivot+1, high)
    end if
  end subroutine quickSort3vectIRI

  recursive subroutine quickSort3vectRRR(vect1, vect2, vect3, low, high)
    implicit none
    real(rkind), dimension(:), intent(InOut) :: vect1, vect2, vect3
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, iPivot+1, high)
    end if       
  end subroutine quickSort3vectRRR

  recursive subroutine quickSort3vectRRI(vect1, vect2, vect3, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect3
    real(rkind), dimension(:), intent(InOut) :: vect1, vect2
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, iPivot+1, high)
    end if
  end subroutine quickSort3vectRRI

  recursive subroutine quickSort3vectRIR(vect1, vect2, vect3, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect2
    real(rkind), dimension(:), intent(InOut) :: vect1, vect3
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, iPivot+1, high)
    end if
  end subroutine quickSort3vectRIR

  recursive subroutine quickSort3vectRII(vect1, vect2, vect3, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect2, vect3
    real(rkind), dimension(:), intent(InOut) :: vect1
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, iPivot+1, high)
    end if
  end subroutine quickSort3vectRII

  recursive subroutine quickSort4vectIIII(vect1, vect2, vect3, vect4, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect1, vect2, vect3, vect4
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect4(i), vect4(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          call swap(vect4(iPivot-1), vect4(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
    end if       
  end subroutine quickSort4vectIIII

  recursive subroutine quickSort4vectIIIR(vect1, vect2, vect3, vect4, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect1, vect2, vect3
    real(rkind), dimension(:), intent(inout) :: vect4
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect4(i), vect4(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          call swap(vect4(iPivot-1), vect4(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
    end if       
  end subroutine quickSort4vectIIIR

  recursive subroutine quickSort4vectIIRI(vect1, vect2, vect3, vect4, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect1, vect2, vect4
    real(rkind), dimension(:), intent(inout) :: vect3
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect4(i), vect4(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          call swap(vect4(iPivot-1), vect4(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
    end if       
  end subroutine quickSort4vectIIRI

  recursive subroutine quickSort4vectIIRR(vect1, vect2, vect3, vect4, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect1, vect2
    real(rkind), dimension(:), intent(inout) :: vect3, vect4
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect4(i), vect4(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          call swap(vect4(iPivot-1), vect4(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
    end if       
  end subroutine quickSort4vectIIRR

  recursive subroutine quickSort4vectIRRR(vect1, vect2, vect3, vect4, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect1
    real(rkind), dimension(:), intent(inout) :: vect2, vect3, vect4
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect4(i), vect4(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          call swap(vect4(iPivot-1), vect4(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
    end if       
  end subroutine quickSort4vectIRRR

  recursive subroutine quickSort4vectIRRI(vect1, vect2, vect3, vect4, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect1, vect4
    real(rkind), dimension(:), intent(inout) :: vect2, vect3
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect4(i), vect4(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          call swap(vect4(iPivot-1), vect4(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
    end if       
  end subroutine quickSort4vectIRRI

  recursive subroutine quickSort4vectIRIR(vect1, vect2, vect3, vect4, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect1, vect3
    real(rkind), dimension(:), intent(inout) :: vect2, vect4
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect4(i), vect4(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          call swap(vect4(iPivot-1), vect4(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
    end if       
  end subroutine quickSort4vectIRIR

  recursive subroutine quickSort4vectIRII(vect1, vect2, vect3, vect4, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect1, vect3, vect4
    real(rkind), dimension(:), intent(inout) :: vect2
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect4(i), vect4(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          call swap(vect4(iPivot-1), vect4(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
    end if       
  end subroutine quickSort4vectIRII

  recursive subroutine quickSort4vectRRRR(vect1, vect2, vect3, vect4, low, high)
    implicit none
    real(rkind), dimension(:), intent(InOut) :: vect1, vect2, vect3, vect4
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect4(i), vect4(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          call swap(vect4(iPivot-1), vect4(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
    end if       
  end subroutine quickSort4vectRRRR

  recursive subroutine quickSort4vectRRRI(vect1, vect2, vect3, vect4, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect4
    real(rkind), dimension(:), intent(inout) :: vect1, vect2, vect3
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect4(i), vect4(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          call swap(vect4(iPivot-1), vect4(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
    end if       
  end subroutine quickSort4vectRRRI

  recursive subroutine quickSort4vectRRIR(vect1, vect2, vect3, vect4, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect3
    real(rkind), dimension(:), intent(inout) :: vect1, vect2, vect4
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect4(i), vect4(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          call swap(vect4(iPivot-1), vect4(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
    end if       
  end subroutine quickSort4vectRRIR

  recursive subroutine quickSort4vectRRII(vect1, vect2, vect3, vect4, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect3, vect4
    real(rkind), dimension(:), intent(inout) :: vect1, vect2
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect4(i), vect4(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          call swap(vect4(iPivot-1), vect4(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
    end if       
  end subroutine quickSort4vectRRII

  recursive subroutine quickSort4vectRIRR(vect1, vect2, vect3, vect4, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect2
    real(rkind), dimension(:), intent(inout) :: vect1, vect3, vect4
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect4(i), vect4(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          call swap(vect4(iPivot-1), vect4(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
    end if       
  end subroutine quickSort4vectRIRR

  recursive subroutine quickSort4vectRIRI(vect1, vect2, vect3, vect4, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect2, vect4
    real(rkind), dimension(:), intent(inout) :: vect1, vect3
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect4(i), vect4(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          call swap(vect4(iPivot-1), vect4(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
    end if       
  end subroutine quickSort4vectRIRI

  recursive subroutine quickSort4vectRIIR(vect1, vect2, vect3, vect4, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect2, vect3
    real(rkind), dimension(:), intent(inout) :: vect1, vect4
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect4(i), vect4(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          call swap(vect4(iPivot-1), vect4(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
    end if
  end subroutine quickSort4vectRIIR

  recursive subroutine quickSort4vectRIII(vect1, vect2, vect3, vect4, low, high)
    implicit none
    integer(ikind), dimension(:), intent(InOut) :: vect2, vect3, vect4
    real(rkind), dimension(:), intent(inout) :: vect1
    integer(ikind), intent(In) :: low, high
    integer(ikind) i, iPivot, j
    iPivot = high    
    i = low
    do while(iPivot > i)
       if (vect1(i) > vect1(iPivot)) then
          call swap(vect1(i), vect1(iPivot-1))
          call swap(vect2(i), vect2(iPivot-1))
          call swap(vect3(i), vect3(iPivot-1))
          call swap(vect4(i), vect4(iPivot-1))
          call swap(vect1(iPivot-1), vect1(iPivot))
          call swap(vect2(iPivot-1), vect2(iPivot))
          call swap(vect3(iPivot-1), vect3(iPivot))
          call swap(vect4(iPivot-1), vect4(iPivot))
          iPivot = iPivot - 1
       else 
          i=i+1
       end if
    end do
    if (low < high) then
       call quickSort(vect1, vect2, vect3, vect4, low, iPivot-1)
       call quickSort(vect1, vect2, vect3, vect4, iPivot+1, high)
    end if       
  end subroutine quickSort4vectRIII

  subroutine swapInt(a,b)
    implicit none
    integer(ikind), intent(InOut) :: a, b
    integer(ikind) ::  aux
    aux = b
    b = a
    a = aux
  end subroutine swapInt
  
  subroutine swapReal(a,b)
    implicit none
    real(rkind), intent(InOut) :: a, b
    real(rkind) ::  aux
    aux = b
    b = a
    a = aux
  end subroutine swapReal

end module QuickSortM
